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
               (executable-find "basedpyright-langserver")
               (executable-find "pyright-langserver")))
  "Find the Python language servers that are available"
  :group 'python)

(defcustom *python-black*
  (executable-find "black-macchiato")
  "Do we have Black and Black-macchiato? The executable of black-macchiato"
  :group 'python)

(defcustom *ipython*
  (executable-find "ipython")
  "Do we have ipython?"
  :group 'python)

(use-package python
  :straight (:type built-in)
  :init
  (add-to-list 'eglot-server-programs
            '((python-mode python-ts-mode)
            ))
  (push
   `((python-mode python-ts-mode) .
     ,(eglot-alternatives
       '(("basedpyright-langserver" "--stdio") ("pyright-langserver" "--stdio") "pylsp" "pyls" "jedi-language-server")))
   eglot-server-programs))

;; TODO: Switch this to emacs-pet?
;; (use-package pyvenv
;;   :straight t
;;   :after python
;;   :hook (python-base-mode . pyvenv-mode))

(use-package pet
  :straight t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(defun d-qoi/set-pyvenv-to-dirlocal ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (venv (or pyvenv-virtual-env
                  (progn (call-interactively 'pyvenv-activate)
                         pyvenv-virtual-env))))
    (add-dir-local-variable 'python-base-mode 'pyvenv-activate venv)))

(use-package python-black
  :straight t
  :if *python-black*
  :after python
  :init
  (add-hook 'python-mode-hook 'python-black-on-save-mode-enable-dwim)
  (add-hook 'python-ts-mode-hook 'python-black-on-save-mode-enable-dwim))

(use-package ipython-shell-send
  :disabled
  :straight (ipython-shell-send :repo "jackkamm/ipython-shell-send-el")
  :after python
  :if *ipython*
  :bind (:map python-base-mode-map
              ([remap python-shell-send-defun] . ipython-shell-send-defun)
              ([remap python-shell-send-buffer] . ipython-shell-send-buffer)
              ([remap python-shell-send-string] . ipython-shell-send-string)
              ([remap python-shell-send-region] . ipython-shell-send-region)
              ([remap python-shell-send-file] . ipython-shell-send-file)))

(defun d-qoi/init-prog-py-after-init ()
  (when *python-lsps-available*
    (add-hook 'python-base-mode-hook 'eglot-ensure))

  (when (and (treesit-available-p) (treesit-language-available-p 'python))
    (add-hook 'python-ts-mode-hook (lambda () (run-hooks 'python-mode-hook)))))

(add-hook 'after-init-hook 'd-qoi/init-prog-py-after-init)

(provide 'init-prog-py)
