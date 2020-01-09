;;; init-defaults.el --- -*- lexical-binding: t -*-

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; simpler aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; no bell
(setq visible-bell t)

;; for auto reloading
(setq global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 10)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; truncating liens
(setq-default truncate-lines t)

;; completion
(setq-default compilation-always-kill t)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; always helpful
(display-time)

;; useful modes
(show-paren-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; binding functions
(global-set-key (kbd "C-x C-k") 'delete-other-windows)
(global-set-key (kbd "C-x M-s") 'save-buffers-kill-emacs)

;; ibuffer buffer list
(global-set-key (kbd "C-x B") 'ibuffer-list-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; paragraph navigation
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "M-{"))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; c-z will be used as an entry point to many functions, still want c-x c-z for suspend frame
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-z"))

(provide 'init-defaults)
;;; setup-general.el ends here