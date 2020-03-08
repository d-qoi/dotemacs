;;; init-projectile.el --- -*- lexical-binding: t -*-

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-root-files #'( ".projectile" )
        projectile-project-root-files-functions #'(projectile-root-top-down
                                                   projectile-root-top-down-recurring
                                                   projectile-root-bottom-up
                                                   projectile-root-local)
        projectile-switch-project-action #'projectile-dired)
  :config
  (define-key projectile-mode-map (kbd "S-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(provide 'init-projectile)
