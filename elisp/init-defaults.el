;;; init-defaults.el --- -*- lexical-binding: t -*-

(require 'windmove)
(require 'framemove)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq x-alt-keysym 'meta)

(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; simpler aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; no bell
(setq visible-bell t)

;; tmm menues
(setq tty-menu-open-use-tmm t)

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

;; Minibuffer
(setq enable-recursive-minibuffers t)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(winner-mode 1)

;; always helpful
(display-time)

;; useful modes
(show-paren-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-c C-r") 'recentf-open-files)

;; binding functions
(global-set-key (kbd "C-x M-s") 'save-buffers-kill-emacs)

;; ibuffer buffer list
(global-set-key (kbd "C-x B") 'ibuffer-list-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; paragraph navigation
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "M-{"))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; c-z will be used as an entry point to many functions as a personal keymap, still want c-x c-z for suspend frame
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z z") 'suspend-frame)
(global-unset-key (kbd "M-z"))

;; multiple frames
(global-set-key (kbd "C-x O") 'other-frame)
(global-set-key (kbd "C-z f") 'make-frame)

(defun d-qoi/push-mark ()
  (interactive)
  (push-mark))

;; mark
(global-set-key (kbd "C-z SPC") 'd-qoi/push-mark)

;; revert buffer
(global-set-key (kbd "C-z r") 'revert-buffer)

;; ttm menu bind again
(global-set-key (kbd "C-z t") 'tmm-menubar)

(require 'tab-bar)
(keymap-set tab-prefix-map "TAB" #'tab-next)

(require 'proced)
(add-hook 'proced-mode-hook (lambda () (proced-toggle-auto-update)))

(require 'timeclock)
(setq timeclock-file "~/timeclock.log")

(provide 'init-defaults)
;;; setup-general.el ends here
