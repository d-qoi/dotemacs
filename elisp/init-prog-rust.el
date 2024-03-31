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
  (define-derived-mode rustic-ts-mode rust-ts-mode "Rustic"
    "Major mode for Rust code, based on rust-ts-mode

  \\{rustic-mode-map}"
    :group 'rustic

    (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
      (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t)))

  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'rust
                :ts-mode 'rustic-ts-mode
                :remap 'rustic-mode
                :url "https://github.com/tree-sitter/tree-sitter-rust"
                :ext "\\.rs\\'"))

  (add-hook 'rust-ts-mode-hook (lambda () (run-hooks 'rustic-mode-hook))))

(provide 'init-prog-rust)
