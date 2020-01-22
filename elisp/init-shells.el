;;; init-shells.el --- -*- lexical-binding: t -*-

(use-package aweshell
  :load-path (lambda () (expand-file-name "site-elisp/aweshell" user-emacs-directory))
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("M-#" . aweshell-dedicated-toggle)))

(use-package shell-here
  :ensure t
  :bind ("M-~" . shell-here)
  :config
  (when *sys/linux*
    (setq explicit-shell-file-name "/bin/bash")))

(use-package term-keys
  :if (not *sys/gui*)
  :config (term-keys-mode t)
  :custom
  (define-key term-mode-map (kbd "C-c C-j") 'term-line-mode))

(use-package multi-term
  :load-path (lambda () (expand-file-name "site-elisp" user-emacs-directory))
  :commands (multi-term)
  :bind
  (("M-$" . multi-term)
   (:map dired-mode-map ("M-$" . multi-term))
   (:map term-raw-map
         ("C->" . multi-term-next)
         ("C-<" . multi-term-prev)))
  :custom
  (multi-term-program (executable-find "bash")))

(provide 'init-shells)
