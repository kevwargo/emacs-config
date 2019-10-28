(defun keymap-prettify (kmap)
  (if (null (keymapp kmap))
      nil
    (list 'keymap
          (mapcar (lambda (e)
                    (if (null (consp e))
                        e
                      (destructuring-bind (key . def) e
                        (cons (key-description (if (sequencep key)
                                                   key
                                                 (vector key)))
                              (cond
                               ((keymapp def) (keymap-prettify def))
                               ((symbolp def) def)
                               (t 'garbage))))))
                  (cdr-safe kmap)))))
