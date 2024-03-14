;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)
(require 'project)

(unless (require 'treesit nil t)
  (defun treesit-available-p () nil))

(defconst *golang*
  (executable-find "go")
  "Is go installed?")

(defconst *gopls*
  (executable-find "gopls")
  "Do we have gopls")

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions 'project-find-go-module)

(add-to-list 'eglot-workspace-configuration
             '(:gopls .
                      ((staticcheck . t)
                       (matcher . "CaseSensitive"))))

(when (and *golang* *gopls*)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-format-buffer-on-save))

(use-package go-mode
  :straight (go-mode :repo "dominikh/go-mode.el"))

(when (and (treesit-available-p) (treesit-language-available-p 'go))
  (add-hook 'go-ts-mode-hook (lambda () (run-hooks 'go-mode-hook))))


(provide 'init-prog-go)
