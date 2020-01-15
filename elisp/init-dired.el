;; init-dired.el --- -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  (dired-dwim-target t))

(use-package neotree
  :ensure t
  :bind ("C-z n" . neotree-toggle)
  :custom
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide 'init-dired)
