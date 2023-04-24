;; -*- lexical-binding -*-

(require 'use-package)

(defun dd/projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :straight t
  :demand t
  :hook (after-init-hook . projectile-mode)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))


(use-package project
  :custom
  (add-to-list 'project-find-functions
               'dd/projectile-proj-find-function))

(use-package transient
  :straight t)

(use-package transient-dwim
  :straight t
  :after (:all (transient magit)))

(use-package magit
  :straight t
  :after transient
  :demand t
  :bind ("C-x g" . magit-status))


(provide 'init-projects)
