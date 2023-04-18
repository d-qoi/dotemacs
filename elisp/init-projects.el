;; -*- lexical-binding -*-

(require 'use-package)

(use-package projectile
  :straight t
  :hook (after-init-hook . projectile-mode)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))


(use-package transient
  :straight t)

(use-package transient-dwim
  :straight t
  :after (:all (transient magit)))

(use-package magit
  :straight t
  :after transient
  :bind ("C-x g" . magit-status))


(provide 'init-projects)
