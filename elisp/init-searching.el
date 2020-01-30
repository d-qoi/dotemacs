;;; init-searching.el --- -*- lexical-binding: t -*-

(use-package ag
  :ensure t
  :if *ag*
  :bind
  (("C-z a g" . ag)
   ("C-z a f" . ag-files)
   ("C-z a r" . ag-regexp)
   ("C-z a p" . ag-project)
   ("C-z a ." . d-qoi/ag-from-point))
  :config
  (defun d-qoi/ag-from-point (sym directory)
    (interactive
     (let* ((sym (thing-at-point 'symbol t))
           (dir-prompt (format "Searching \"%s\" in Directory: " sym)))
       (list sym (read-directory-name dir-prompt))))
    (ag/search sym directory))
  :custom
  (ag-reuse-buffers t))

(use-package anzu
  :ensure t
  :diminish
  :bind
  (("C-z q r" . anzu-query-replace)
   ("C-z q R" . anzu-query-replace-regexp)
   ("C-z q ." . anzu-replace-at-cursor-thing))
  :config
  (global-anzu-mode 1))

(use-package wgrep
  :load-path (lambda () (expand-file-name "site-elisp/Emacs-wgrep" user-emacs-directory))
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package winnow
  :ensure t
  :config
  (add-hook 'ag-mode-hook 'winnow-mode))

(provide 'init-searching)
