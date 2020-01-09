;;; init-magit.el --- -*- lexical-binding: t -*-

(use-package magit
  :ensure t
  :if *git*
  :bind ("C-x g" . magit-status))

(provide 'init-magit)
