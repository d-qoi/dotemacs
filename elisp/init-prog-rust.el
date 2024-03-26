;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)
(require 'project)

(unless (require 'treesit nil t)
  (defun treesit-available-p () nil))

(defconst *rustup*
  (executable-find "rustup")
  "Is rustup installed?")

(defconst *rustc*
  (executable-find "rustc")
  "Where is rustc")

(defconst *rust-analyzer*
  (executable-find "rust-analyzer")
  "Is the LSP installed")

(use-package rustic
  :straight t
  :custom
  (rustic-lsp-client 'eglot)
  :config
  (add-hook 'rustic-mode-hook (lambda () (flymake-mode -1))))

(when (and (treesit-available-p) (treesit-language-available-p 'rust))
  (add-hook 'rust-ts-mode-hook (lambda () (run-hooks 'rustic-mode-hook))))

(provide 'init-prog-rust)
