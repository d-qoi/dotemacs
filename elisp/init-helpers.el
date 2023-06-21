;;; init-helpers.el --- -*- lexical-binding: t -*-

(defun insert-file-or-directory-path ()
  "Insert the path of a file or directory at point.
If called with a prefix argument, insert the full expanded path."
  (interactive)
  (let* ((path (read-file-name "Insert file or directory path: "))
         (expanded-path (if current-prefix-arg
                            (expand-file-name path)
                          path)))
    (when path
      (insert expanded-path))))

(provide 'init-helpers)
