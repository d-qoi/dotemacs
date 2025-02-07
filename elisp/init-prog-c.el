;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)
(require 'ansi-color)

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

(defcustom *ctags*
  (executable-find "ctags")
  "Do we have Universal Ctags?"
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
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-format-buffer-on-save)
    (add-hook 'c-mode-hook 'eglot-format-buffer-on-save))

  ;; Pulled into after-init function so *clang-format* can be set in customise
  ;; Don't ask why I needed to do this. There's probably an easier way.

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

;; Colorized compilation -- Maybe it should be moved to global?
(defun d-qoi/colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'd-qoi/colorize-compilation-buffer)

(use-package flymake-google-cpplint
  :if (executable-find "cpplint")
  :straight (:host github :repo "d-qoi/flymake-google-cpplint")
  :hook (eglot-managed-mode . cpplint-hook-flymake-diag-function))

(defun d-qoi/citre-peek-increase-context ()
  "Increase citre-peek-file-content by 1 and rerender the node"
  (interactive)
  (setq citre-peek-file-content-height
        (1+ citre-peek-file-content-height))
  (message "Citre Peek Content Height: %d" citre-peek-file-content-height)
  (citre-peek--line-forward 0))

(defun d-qoi/citre-peek-decrease-context ()
  "Increase citre-peek-file-content by 1 and rerender the node"
  (interactive)
  (setq citre-peek-file-content-height
        (1- citre-peek-file-content-height))
  (message "Citre Peek Content Height: %d" citre-peek-file-content-height)
  (citre-peek--line-forward 0))

(use-package citre
  :if *ctags*
  :straight (:host github :repo "universal-ctags/citre")
  :bind (:map citre-mode-map
         ("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back)
         ("C-c c p" . citre-peek)
         ("C-c c P" . citre-ace-peek)
         ("C-c c U" . citre-update-this-tags-file)
         :map citre-peek-keymap
         ("M-+" . d-qoi/citre-peek-increase-context)
         ("M-_" . d-qoi/citre-peek-decrease-context))
  :config
  (require 'citre-config))

(provide 'init-prog-c)
