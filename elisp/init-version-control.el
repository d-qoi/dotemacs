;;; init-version-control.el --- -*- lexical-binding: t -*-

(use-package transient
  :ensure t)

(use-package transient-dwim
  :ensure t)

(use-package magit
  :ensure t
  :if *git*
  :bind ("C-x g" . magit-status))

(use-package p4
  :if *p4*
  :load-path (lambda () (expand-file-name "site-elisp/p4.el" user-emacs-directory))
  :custom (p4-open-in-changelist t)
  :config
  (defun d-qoi/p4-changes ()
    (interactive)
    (p4-changes '("-u" "ahirschfeld")))
  (defun d-qoi/p4-changes-shelved ()
    (interactive)
    (p4-changes '("-s" "shelved" "-u" "ahirschfeld")))
  (let ((map (symbol-function 'p4-prefix-map)))
    (define-key map "Mc" 'd-qoi/p4-changes)
    (define-key map "Ms" 'd-qoi/p4-changes-shelved)))

(provide 'init-version-control)
