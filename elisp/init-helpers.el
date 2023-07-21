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

(defun d-qoi/json-query (json-blob &rest keys)
  "Retrieve a nested object from a JSON blob using the specified keys.
   json-blob: The JSON blob to retrieve the nested object from.
   keys: The keys specifying the nested structure."
  (if (not keys)
      json-blob
    (let ((nested-object (gethash (car keys) json-blob))
          (next (cdr keys)))
      (if (and (not (hash-table-p nested-object)) next)
          ;; nested-object is not hash table, and args still remain
          (error (format "Too many keys remaining (%s) for value (%s)" nested-object keys))
          ;; Apply is used to turn next into args.
        (apply 'd-qoi/json-query nested-object next)))))


(provide 'init-helpers)
