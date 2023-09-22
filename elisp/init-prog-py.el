;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)

;; eglot-server-program defaults:
;; ((python-mode python-ts-mode)
;;  . ,(eglot-alternatives
;;      '("pylsp" "pyls" ("pyright-langserver" "--stdio") "jedi-language-server")))

(defcustom *python-lsps-available*
  (remove nil (list
               (executable-find "pylsp")
               (executable-find "jedi-language-server")
               (executable-find "pyright-langserver")))
  "Find the Python language servers that are available"
  :group 'python)

(defcustom *python-black*
  (executable-find "black-macchiato")
  "Do we have Black and Black-macchiato? The executable of black-macchiato"
  :group 'python)

(use-package python
  :straight (:type built-in)
  :init
  (push
   `((python-mode python-ts-mode) .
     ,(eglot-alternatives
       '(("pyright-langserver" "--stdio") "pylsp" "pyls" "jedi-language-server")))
   eglot-server-programs))

(use-package pyvenv
  :straight t
  :after python
  :hook (python-base-mode . pyvenv-mode))

(defun d-qoi/set-pyvenv-to-dirlocal ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (venv (or pyvenv-virtual-env
                  (progn (call-interactively 'pyvenv-activate)
                         pyvenv-virtual-env))))
    (add-dir-local-variable 'python-base-mode 'pyvenv-activate venv)))

(use-package python-black
  :straight t
  :demand t
  :after python
  :hook (python-base-mode . python-black-on-save-mode-enable-dwim))

(defun d-qoi/init-prog-py-after-init ()
  (when *python-lsps-available*
    (add-hook 'python-base-mode-hook 'eglot-ensure))

  (when (and (treesit-available-p) (treesit-language-available-p 'python))
    (add-hook 'python-ts-mode-hook (lambda () (run-hooks 'python-mode-hook)))))

(add-hook 'after-init-hook 'd-qoi/init-prog-py-after-init)

(provide 'init-prog-py)
