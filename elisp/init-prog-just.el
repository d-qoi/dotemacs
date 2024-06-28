(require 'use-package)
(require 'project)

(defcustom *justfile*
  (executable-find "just")
  "Do we have just?")

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

(provide 'init-prog-just)
