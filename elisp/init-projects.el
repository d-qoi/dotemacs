;; -*- lexical-binding: t -*-

(require 'use-package)

(defcustom *justfile*
  (executable-find "just")
  "Do we have just?")

(defcustom project-local-identifier ".project"
  "Specify a single filename or a list of names."
  :type '(choice (string :tag "Single file")
                 (repeat (string :tag "Filename")))
  :group 'project)

(defun d-qoi/project-touch-and-remember ()
  "Create project-local-identifier and remember in current dir."
  (interactive)
  (shell-command (format "touch %s" project-local-identifier))
  (project-remember-projects-under default-directory))

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(defun project-local-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a file with the name determined by the
variable `project-local-identifier' to be considered a project."
  (if-let ((root (if (listp project-local-identifier)
                     (seq-some (lambda (n)
                                 (locate-dominating-file dir n))
                               project-local-identifier)
                   (locate-dominating-file dir project-local-identifier))))
      (cons 'local root)))

(use-package project
  :straight (:type built-in)
  :demand t
  :bind (:map project-prefix-map
              ("s" . eat-project))
  :config
  (if (not (boundp 'project-vc-extra-root-markers))
      (setq project-vc-extra-root-markers '()))
  (add-hook 'project-find-functions 'project-local-try-local))

(use-package magit
  :straight t
  :after transient
  :demand t
  :bind ("C-x g" . magit-status))
  ;; :init
  ;; (d-qoi/add-hook-after
  ;;  'magit-status-sections-hook
  ;;  'magit-insert-ignored-files
  ;;  'magit-insert-untracked-files))

(use-package forge
  :straight t
  :after magit)

(use-package git-timemachine
  :straight t
  :after magit)

(use-package just-mode
  :straight t
  :if *justfile*
  :custom
  (just-executable *justfile*))

(use-package justl
  :straight t
  :if *justfile*
  :bind (:map project-prefix-map
              ("j" . d-qoi/justl-in-project))
  :custom
  (justl-executable *justfile*)
  :config
  (defun d-qoi/justl-in-project ()
    "Run Justl in the current project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (justl))))


(provide 'init-projects)
