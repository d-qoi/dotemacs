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

(provide 'init-searching)
