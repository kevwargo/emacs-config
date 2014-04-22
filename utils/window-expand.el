(defun window-siblings (window)
  "Return all WINDOW's siblings, starting
from (window-child (window-parent WINDOW)). Return nil if WINDOW is internal."
  (and (window-parent window)
       (let ((sibling (window-child (window-parent window)))
             list)
         (while sibling
           (push sibling list)
           (setq sibling (window-next-sibling sibling)))
         (nreverse list))))

(defun sibling-in-direction (window dir)
  "Return non-nil if window in direction DIR from WINDOW is its sibling."
    (case dir
      ((up down)
       (and (window-combined-p window)
            (if (eq dir 'up)
                (window-prev-sibling window)
              (window-next-sibling window))))
      ((left right)
       (and (window-combined-p window t)
            (if (eq dir 'left)
                (window-prev-sibling window)
              (window-next-sibling window))))))

(defun contains-appropriate-combinations (window horizontal)
  "Recursively check if WINDOW has only one kind of combinations within it."
  ;; `walk-window-subtree' returns nil, so we need to negate result of `catch'
  (not (catch 'done
         (walk-window-subtree (lambda (w)
                                (when (and (window-combined-p w (not horizontal))
                                           (not (eq w window)))
                                  (throw 'done t)))
                              window t))))

(defun delete-all-but-last-live-child (window)
  (let* ((child (window-child window))
         (next-child (window-next-sibling child)))
    (if (not child)
        window
      (while (window-valid-p window)
        (if (window-live-p child)
            (progn
              (delete-window child)
              (setq child next-child
                    next-child (window-next-sibling child)))
          (delete-all-but-last-live-child child)
          (setq child (window-child window))))
      child)))

(defun resize-siblings (to-enlarge to-shrink delta horizontal)
  (when (or (eq to-enlarge (window-next-sibling to-shrink))
            (eq to-shrink (window-next-sibling to-enlarge)))
    (let ((frame (window-frame to-enlarge))
          (normal-delta (/ (float delta)
                           (window-normal-size (window-parent to-enlarge)))))
      (window--resize-reset frame horizontal)
      (window--resize-this-window to-enlarge delta horizontal nil t)
      (window--resize-this-window to-shrink (- delta) horizontal nil t)
      (set-window-new-normal to-enlarge (+ (window-normal-size to-enlarge)
                                           normal-delta))
      (set-window-new-normal to-shrink (- (window-normal-size to-shrink)
                                          normal-delta))
      (window-resize-apply frame horizontal))))

(defun consume-whole-sibling (window sibling horizontal)
  (let ((sibling-size (window-total-size sibling horizontal)))
    (when (window-sizable-p window sibling-size horizontal)
      (if (memq sibling (list (window-next-sibling window)
                              (window-child (window-parent window))
                              (window-last-child (window-parent window))))
          ;; Now we can trust deleting sibling to default `delete-window',
          ;; and WINDOW definitely will be resized as we want
          (delete-window sibling)
          ;; (delete-window (delete-all-but-last-live-child sibling))
        ;; Here we have to delete sibling by ourselves (ignoring window parameters),
        ;; because `window-delete' will resize a non-appropriate window
        ;; (setq sibling (delete-all-but-last-live-child sibling))
        (window--resize-reset (window-frame window) horizontal)
        (window--resize-this-window window (window-total-size sibling horizontal)
                                    horizontal nil t)
        (set-window-new-normal window (+ (window-normal-size window horizontal)
                                         (window-normal-size sibling horizontal)))
        (delete-window-internal sibling)
        (run-window-configuration-change-hook (window-frame window))))))

(defun find-subwindows-to-consume (sibling dir)
  (when (window-child sibling)
    (let* ((horizontal (memq dir '(left right)))
           (smallest-distance (window-total-size sibling horizontal))
           (edge (case dir
                   ((left) 2)
                   ((up) 3)
                   ((right) 0)
                   ((down) 1)))
           (sibling-edge-value (nth edge (window-edges sibling)))
           delenda)
      (walk-window-subtree (lambda (w)
                             (let ((size (window-total-size w horizontal)))
                               (when (= (nth edge (window-edges w))
                                        sibling-edge-value)
                                 (cond ((< size smallest-distance)
                                        (setq smallest-distance size
                                              delenda (list w)))
                                       ((and (= size smallest-distance)
                                             delenda
                                             (not (eq (car delenda)
                                                      (window-parent w))))
                                        (push w delenda))))))
                           sibling t)
      delenda)))

(defun expand-window-dir (window dir)
  (setq window (window-normalize-window window))
  (let ((sibling (sibling-in-direction window dir))
        (parent (window-parent window))
        (window-combination-resize nil)
        (horizontal (memq dir '(left right))))
    (when sibling
      (if (or (window-live-p sibling)
              (contains-appropriate-combinations sibling (not horizontal)))
          (consume-whole-sibling window sibling horizontal)
        (let* ((delenda (find-subwindows-to-consume sibling dir))
               (size (window-total-size (car delenda) horizontal)))
          (dolist (w delenda)
            (delete-window w))
          (resize-siblings window sibling size horizontal))))))
