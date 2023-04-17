;;; init.el --- -*- lexical-binding: t -*-

(setq custom-file "~/.emacs.d/emacs-custom.el")
(add-to-list 'load-path "~/.emacs.d/elisp")

;; package setup

;; use-package setup



;; Emacs customizations that do not require installations


;; submodules that may or may not be loaded

;; One of the last things to do.
(if (file-exists-p custom-file)
    (load custom-file))

;; and a prompt to save all unsaved customizations
(add-hook 'kill-emacs-query-functions
	  'custom-prompt-customize-unsaved-options)
