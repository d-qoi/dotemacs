;;; init-helpers.el --- -*- lexical-binding: t -*-

(require 'find-lisp)
(require 'autoload)
(require 'cl)

(defun d-qoi/remove-from-list ()
  "Remove an element by index from any list. It may faulter on duplicates."
  (interactive)
  (let ((list-of-lists))
    (mapatoms (lambda (sym)
                (if (and (boundp sym) (symbol-value sym) (listp (symbol-value sym)))
                    (push sym list-of-lists))))
    (when-let*
        ((list-of-lists)
         (chosen-list-str (completing-read "Choose a list: " list-of-lists nil t))
         (chosen-list (intern-soft chosen-list-str))
         (list-value (symbol-value chosen-list))
         (selection-map (cl-mapcar (lambda (val index) (cons (format "%d: %s" index (prin1-to-string val)) index))
                                   list-value
                                   (number-sequence 0 (length list-value))))
         (chosen-index-str (completing-read "Choose the item to remove: " selection-map nil t))
         (chosen-index (cdr (assoc chosen-index-str selection-map))))
      (set chosen-list (delete (nth chosen-index list-value) list-value))
      (message "Removed %s from %s" chosen-index-str chosen-list))))

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

(defun d-qoi/set-element-at-position (sequence elem position)
  (let ((seq-val (symbol-value sequence)))
    (if (or (null seq-val) (> position (length seq-val)))
        (set sequence (append seq-val (list elem)))
      (set sequence (append (seq-subseq seq-val 0 position)
                        (list elem)
                        (seq-subseq seq-val position))))))

(defun d-qoi/add-hook-after (hook new-func existing-func)
  (let ((position (1+ (seq-position (symbol-value hook) existing-func))))
    (d-qoi/set-element-at-position hook new-func position)))

(defun d-qoi/term-toggle-line-char ()
  "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
  (interactive)
  (cond
   ((term-in-line-mode)
    (term-char-mode)
    (hl-line-mode -1))
   ((term-in-char-mode)
    (term-line-mode)
    (hl-line-mode 1))))

(provide 'init-helpers)
