(defun keymap-prettify (kmap)
  (if (keymapp kmap)
      (list 'keymap
            (mapcar (lambda (e)
                      (if (consp e)
                          (cl-destructuring-bind (key . def) e
                            (cons (key-description (if (sequencep key)
                                                       key
                                                     (vector key)))
                                  (cond
                                   ((keymapp def) (keymap-prettify def))
                                   ((symbolp def) def)
                                   (t 'garbage))))
                        e))
                    (cdr-safe kmap)))))
