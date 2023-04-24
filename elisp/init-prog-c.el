;; -*- lexical-binding -*-

(require 'use-package)
(require 'eglot)

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
                  "--header-insertion-decorators=0")))

(setq c-basic-offset 4
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun d-qoi/c-initialization-hook ()
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (c-toggle-electric-state 1))
(add-hook 'c-initialization-hook 'd-qoi/c-initialization-hook)

(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)

(provide 'init-prog-c)
