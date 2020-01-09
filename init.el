;;; init.el --- -*- lexical-binding: t -*-

(setq debug-on-error t)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/site-elisp")

(setq better-gc-cons-threshold 134217728)
(setq gc-cons-threshold better-gc-cons-threshold)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold better-gc-cons-threshold)
;;             (setq file-name-handler-alist file-name-handler-alist-original)
;;             (makunbound 'file-name-handler-alist-original)))
;; ;; -BetterGC

;; ;; AutoGC
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (if (boundp 'after-focus-change-function)
;;                 (add-function :after after-focus-change-function
;;                               (lambda ()
;;                                 (unless (frame-focus-state)
;;                                   (garbage-collect))))
;;               (add-hook 'after-focus-change-function 'garbage-collect))
;;             (defun gc-minibuffer-setup-hook ()
;;               (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

;;             (defun gc-minibuffer-exit-hook ()
;;               (garbage-collect)
;;               (setq gc-cons-threshold better-gc-cons-threshold))

;;             (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
;;             (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; constant definitions, helpful to have
(require 'constants)

;; package setup
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq package-check-signature nil)
  (package-install 'gnu-elpa-keyring-update)
  (setq package-check-signature 'allow-unsigned))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(use-package diminish
  :ensure t)

;; Emacs customizations that do not require installations
(require 'init-defaults)
(require 'custom-functions)

(require 'init-dired)
(require 'dired+) ;; better dired

(require 'init-linum)

;; packages that require config
(require 'init-ivy)
(require 'init-whichkey)
(require 'init-killring-undo)
(require 'init-shells)

(require 'init-editing)
(require 'init-flycheck)
(require 'init-lsp)

(require 'init-magit)

;; submodules that may or may not be loaded


;; One of the last things to do.
(load custom-file)

;; and a prompt to save all unsaved customizations
(add-hook 'kill-emacs-query-functions
	  'custom-prompt-customize-unsaved-options)