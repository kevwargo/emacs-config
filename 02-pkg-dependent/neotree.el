(require 'neotree)

(defun neotree-show-cwd ()
  (interactive)
  (neo-global--open-dir (buffer-working-directory)))

(defun neotree-go-up ()
  (interactive)
  (neo-global--open-dir (file-name-directory
                         (directory-file-name
                          (file-name-as-directory neo-buffer--start-node)))))

(defun neotree-fold-all ()
  (interactive)
  (mapc (lambda (s)
          (when (string-prefix-p (file-name-as-directory
                                  neo-buffer--start-node)
                                 s)
            (neo-buffer--set-expand s nil)))
        neo-buffer--expanded-node-list)
  (neo-buffer--refresh t))

(defun neotree-change-root-dir (full-path &optional arg)
  (neotree-change-root))

(defun setup-neotree ()
  (keymap-local-set "<backspace>" 'neotree-go-up)
  (keymap-local-set "RET" (neotree-make-executor
                              :file-fn 'neo-open-file
                              :dir-fn  'neotree-change-root-dir))
  (keymap-local-set "u" 'neotree-select-up-node)
  (keymap-local-set "f" 'neotree-fold-all))

(defvar neotree-custom-map
  (let ((m (make-sparse-keymap)))
    (keymap-set m "RET" 'neotree-show-cwd)
    m)
  "Custom map enabled in minor mode for neotree bindings")

(keymap-set key-overrides-mode-map "C-c n" neotree-custom-map)

(add-hook 'neotree-mode-hook 'setup-neotree)
