;; init-dired.el --- -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  (dired-dwim-target t))

(provide 'init-dired)
