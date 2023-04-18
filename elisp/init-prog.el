;; -*- lexical-binding: t -*-

(require 'use-package)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package yasnippet
  :straight (yasnippet :host github :repo "joaotavora/yasnippet")
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight (yasnippet-snippets :host github :repo "AndreaCrotti/yasnippet-snippets")
  :after yasnippet)

(use-package company
  :straight t
  :after orderless
  :hook (after-init-hook . global-company-mode)
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
                            company-keywords))))

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
  :after (:all consult eglot))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=8"
                    "--log=error"
                    "--malloc-trim"
                    "--background-index"
                    "--clang-tidy"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))))

(provide 'init-prog)
