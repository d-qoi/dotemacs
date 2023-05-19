;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package yaml-mode
  :straight (:repo "yoshiki/yaml-mode")
  :mode "\\.yml\\'"
  :bind (:map yaml-mode-map
              ("RET" . newline-and-indent)))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(provide 'init-prog-markup)
