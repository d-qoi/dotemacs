;;; init-projectile.el --- -*- lexical-binding: t -*-

(use-package projectile
  :ensure t
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config
  (define-key projectile-mode-map (kbd "S-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(provide 'init-projectile)
