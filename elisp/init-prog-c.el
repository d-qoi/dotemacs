;; -*- lexical-binding: t -*-

(require 'eglot)

(defconst *clangd*
  (executable-find "clangd")
  "Do we have clangd?")

(when *clangd*
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=8"
                    "--pretty"
                    "--enable-config"
                    "--log=verbose"
                    "--background-index"
                    "--malloc-trim"
                    "--clang-tidy"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0")))
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure))

(setq c-basic-offset 4
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun d-qoi/c-initialization-hook ()
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (c-toggle-electric-state 1))
;;(add-hook 'c-initialization-hook 'd-qoi/c-initialization-hook)


(when (treesit-available-p)
  (when (treesit-language-available-p 'c)
    (add-hook 'c-ts-mode-hook (lambda () (run-hooks 'c-mode-hook)))
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(c-ts-mode . semantic-default-c-setup)))

  (when (treesit-language-available-p 'cpp)
    (add-hook 'c++-ts-mode-hook (lambda () (run-hooks 'c++-mode-hook)))
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(c++-ts-mode . semantic-default-c-setup))))


(provide 'init-prog-c)
