;; -*- lexical-binding: t -*-

(require 'use-package)
(require 'dired+)

(use-package dired
  :straight (:type built-in)
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  (dired-dwim-target t))

(use-package dired-async
  :straight (emacs-async :repo "jwiegley/emacs-async")
  :config
  (dired-async-mode 1))

(defun d-qoi/term-toggle-line-char ()
  "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
  (interactive)
  (cond
   ((term-in-line-mode)
    (term-char-mode)
    (hl-line-mode -1))
   ((term-in-char-mode)
    (term-line-mode)
    (hl-line-mode 1))))

(use-package multi-term
  :straight t
  :disabled
  :commands (multi-term)
  :bind
  ((:map term-raw-map
         ("C->" . multi-term-next)
         ("C-<" . multi-term-prev)
         ("C-t" . d-qoi/term-toggle-line-char))
   (:map term-mode-map
         ("C-t" . d-qoi/term-toggle-line-char))))

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
  :bind
  ("C-$" . eat)
  :custom
  ((eat-kill-buffer-on-exit t)))

(use-package docker
  :straight t)

(provide 'init-nav)
