;;; package --- custom functions that don't have another place to go -*- lexical-bindings -*-

;;; Code:

(defun revert-all-file-buffer ()
  "Refresh all open files buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted.  They
will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffer containing files, which are not modified;
      ;; try not to revert non-file buffers
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename) ;; only revert if readable file
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes)
              (message "Reverted buffer: %s" filename))
          ;; otherwise kill buffer (file no longer exists)
          (let (kill-buffer-query-functions)
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finnished reverting all buffers"))

(defun kill-all-file-buffers ()
  "Kill all file buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when filename
        (let (kill-buffer-query-functions)
          (kill-buffer buf)
          (message "Killed %s" filename))))))

(defun d-qoi/load-ssh-abbrev ()
  "Create Dired Abbreviations from ~/.ssh/config.
The ssh/config file will needs to be layed out in specific format to allow
the regex to operate properly.  Advice should be added next."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (while (re-search-forward "host \\(.+\\)\n\\W+hostname \\(.+\\)\n\\W+ user \\(.+\\)" (point-max) t 1)
      (let ((host (upcase (match-string 1)))
            (hostname (match-string 2))
            (username (match-string 3)))
        (add-to-list 'directory-abbrev-alist
                     (cons (format "^/%s" host)
                           (format "/scp:%s@%s:" username hostname)))
        (message (format (format "^/%s" host)))))))

(defun d-qoi/push-mark ()
  "Interactive 'push-mark'."
  (interactive)
  (push-mark))

(defun d-qoi/remove-all-lines-with-spaces ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (flush-lines " ")))

(provide 'custom-functions)

