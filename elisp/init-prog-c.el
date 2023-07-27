;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)

(defcustom *clangd*
  (executable-find "clangd")
  "Do we have clangd?"
  :group 'c)

(defcustom *clang-format*
  (executable-find "clang-format")
  "Do we have Clang-format?"
  :group 'c)

(defcustom *clang-format-elisp-file*
  (and *clang-format* (d-qoi/find-lisp-in-dirs system-emacs-dirs "clang-format"))
  "Where is the elisp file for clang-format?"
  :group 'c)

(setq c-basic-offset 4
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun d-qoi/c-initialization-hook ()
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (c-toggle-electric-state 1))
;;(add-hook 'c-initialization-hook 'd-qoi/c-initialization-hook)


(when (treesit-available-p)
  (when (treesit-language-available-p 'c)
    (add-hook 'c-ts-mode-hook (lambda () (run-hooks 'c-mode-hook)))
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(c-ts-mode . semantic-default-c-setup)))

  (when (treesit-language-available-p 'cpp)
    (add-hook 'c++-ts-mode-hook (lambda () (run-hooks 'c++-mode-hook)))
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(c++-ts-mode . semantic-default-c-setup))))

(defun d-qoi/init-prog-c-after-init ()
  (when *clangd*
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode)
                   . ("clangd"
                      "-j=8"
                      "--pretty"
                      "--log=verbose"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--pch-storage=memory"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0")))
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'c-mode-hook 'eglot-ensure))

  ;; Pulled into after-init function so *clang-format* can be set in customise
  ;; pull from melpa if needed
  (use-package clang-format
    :if (and *clang-format* (not *clang-format-elisp-file*))
    :straight t
    :bind (:map c-mode-base-map
                ("C-M-TAB" . clang-format-region))
    :init
    (message "Clang-format pulled from Melpa"))

  ;; pull from melpa if needed
  (use-package clang-format
    :if (and *clang-format* *clang-format-elisp-file*)
    :straight (:type built-in)
    :bind (:map c-mode-base-map
                ("C-M-TAB" . clang-format-region))
    :init
    (push (f-dirname (car *clang-format-elisp-file*)) load-path)
    (message "Clang-format found locally")))

(add-hook 'after-init-hook 'd-qoi/init-prog-c-after-init)

;; TODO Pull cpplint into its own file.

(use-package flymake-google-cpplint
  :if (executable-find "cpplint")
  :straight (:host github :repo "d-qoi/flymake-google-cpplint")
  :hook (eglot-managed-mode . cpplint-hook-flymake-diag-function))

(provide 'init-prog-c)
