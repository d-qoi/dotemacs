;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package eglot)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package yasnippet
  :straight (yasnippet :host github :repo "joaotavora/yasnippet")
  :demand t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight (yasnippet-snippets :host github :repo "AndreaCrotti/yasnippet-snippets")
  :demand t
  :after yasnippet)

(use-package company
  :straight t
  :demand t
  :after orderless
  :bind
  (("C-c C-/" . company-other-backend)
   ("C-c Y" . company-yasnippet))
  :config
  (setq company-tooltip-flip-when-above t)
  (setq company-backends '((company-capf
                            company-semantic
                            company-yasnippet
                            company-dabbrev-code
                            company-files
                            company-keywords)))
  (global-company-mode 1))

(use-package semantic
  :hook (after-init-hook . semantic-mode))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
;; (defun company-completion-styles (capf-fn &rest args)
;;   (let ((completion-styles '(basic partial-completion)))
;;     (apply capf-fn args))

;; (advice-add 'company-capf :around #'company-completion-styles)

(use-package consult-eglot
  :straight t
  :demand t
  :after (:all consult eglot))


(use-package idle-highlight-mode
  :straight t
  :hook (prog-mode-hook . idle-highlight-mode))

(provide 'init-prog)
