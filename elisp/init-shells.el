;;; init-shells.el --- -*- lexical-binding: t -*-

(use-package aweshell
  :load-path (lambda () (expand-file-name "site-elisp/aweshell" user-emacs-directory))
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("M-#" . aweshell-dedicated-toggle)))

(use-package shell-here
  :disabled
  :ensure t
  :bind ("M-~" . shell-here)
  :config
  (when *sys/linux*
    (setq explicit-shell-file-name "/bin/bash")))

(use-package term-keys
  :disabled
  :ensure t
  :if (not *sys/gui*)
  :config (term-keys-mode t)
  :custom
  (define-key term-mode-map (kbd "C-c C-j") 'term-line-mode))

(use-package multi-term
  :load-path (lambda () (expand-file-name "site-elisp" user-emacs-directory))
  :commands (multi-term)
  :bind
  (("M-$" . multi-term)
   (:map term-raw-map
         ("C->" . multi-term-next)
         ("C-<" . multi-term-prev)
         ("C-t" . d-qoi/term-toggle-line-char))
   (:map term-mode-map
         ("C-t" . d-qoi/term-toggle-line-char)))
  ;; :custom
  ;; (add-to-list term-bind-key-alist '("C-c C-t" . d-qoi/term-toggle-line-char))
  ;; (add-to-list term-bind-key-alist '("C-c C-k" . term-char-mode))

  ;; (add-hook 'term-mode-hook (lambda ()
  ;;                             (yas-minor-mode -1)
  ;;                             (toggle-truncate-lines 1)
  ;;                             (define-key term-raw-map (kbd "C-t") 'd-qoi/term-toggle-line-char)
  ;;                             (define-key term-mode-map (kbd "C-t") 'd-qoi/term-toggle-line-char)
  ;;                             (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
  ;;                             (define-key term-raw-map (kbd "C-y") 'term-paste)
  ;;                             (define-key term-raw-map (kbd "C-c C-e") 'term-send-esc)
  ;;                             ))

  :init
  (defun d-qoi/term-toggle-line-char ()
    "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
    (interactive)
   (cond
    ((term-in-line-mode)
     (term-char-mode)
     (hl-line-mode -1))
    ((term-in-char-mode)
     (term-line-mode)
     (hl-line-mode 1)))))

;; (global-key-binding (kbd "M-$") 'ansi-term)

(provide 'init-shells)
