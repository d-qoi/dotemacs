;; -*- lexical-binding: t -*-

(require 'use-package)
(require 'dired+)
(require 'find-dired+)

(use-package journalctl-mode
  :straight t)

(use-package rg
  :straight t
  :bind
  (("C-c s" . rg-menu)))

(use-package dired
  :straight (:type built-in)
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  (diredp-hide-details-initially-flag nil)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t))

(use-package dired-async
  :straight (emacs-async :repo "jwiegley/emacs-async")
  :config
  (dired-async-mode 1))

(require 'eshell)
(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :hook (eshell-load . #'eat-eshell-visual-command-mode)
  :bind
  ("C-$" . eat)
  :custom
  ((eat-kill-buffer-on-exit t))
  :config
  (add-to-list 'tramp-remote-process-environment "TERM=xterm-256color")
  (add-to-list 'tramp-remote-process-environment "TERMINFO=''"))

(use-package docker
  :straight t)

(defun d-qoi/neotree-here ()
  (interactive)
  (neotree-dir default-directory))

(use-package neotree
  :straight t
  :bind
  ([f8] . neotree-toggle)
  ("C-<f8>" . d-qoi/neotree-here))

(use-package envrc
  :straight t
  :if (executable-find "envrc")
  :hook (after-init . envrc-global-mode))

(provide 'init-nav)
