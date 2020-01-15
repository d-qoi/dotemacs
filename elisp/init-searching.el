;;; init-searching.el --- -*- lexical-binding: t -*-

(use-package ag
  :ensure t
  :if *ag*
  :bind
  (("C-z a g" . ag)
   ("C-z a f" . ag-files)
   ("C-z a r" . ag-regex)
   ("C-z a p" . ag-project))
  :init
  (setq ag-reuse-window t))

(use-package anzu
  :ensure t
  :diminish
  :bind
  (("C-z q r" . anzu-query-replace)
   ("C-z q R" . anzu-query-replace-regexp)
   ("C-z q c" . anzu-replace-at-cursor-thing))
  :config
  (global-anzu-mode 1))

(use-package wgrep
  :ensure t)

(use-package winnow
  :ensure t
  :hook ag-mode)

(provide 'init-searching)

