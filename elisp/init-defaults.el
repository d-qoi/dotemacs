(menu-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; simpler aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; no bell
(setq visible-bell t)

;; for auto reloading
(setq global-auto-revert-mode 1)

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

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; always helpful
(display-time)

;; binding functions
(global-set-key (kbd "C-x C-k") 'delete-other-windows)
(global-set-key (kbd "C-x M-s") 'save-buffers-kill-emacs)

(message "init-defaults loaded")
(provide 'init-defaults)
;;; setup-general.el ends here
