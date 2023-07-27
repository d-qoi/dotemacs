;;; init-helpers.el --- -*- lexical-binding: t -*-

(require 'find-lisp)
(require 'autoload)

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

(defconst system-emacs-dirs
  '("/usr/share/emacs/"
    "/usr/local/share/emacs/")
  "Search paths for find-lisp-in-dirs")

(defun d-qoi/find-lisp-in-dirs (dirs name)
  "Finds `name` in `dirs` using find-lisp-find-files and returns the results.
   Results are a list, there may be more than one entry on the list.
   The results are not ordered."
  (when dirs
    (let* ((current-dir (car dirs))
          (next-dirs (cdr dirs))
          (result (find-lisp-find-files current-dir name)))
      (if result
          result
        (d-qoi/find-lisp-in-dirs next-dirs name)))))

(provide 'init-helpers)
