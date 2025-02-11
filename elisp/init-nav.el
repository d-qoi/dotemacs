;; -*- lexical-binding: t -*-

(require 'use-package)
(require 'dired+)
(require 'find-dired+)

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

(provide 'init-nav)
