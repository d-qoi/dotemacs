;;; init-prog-markup-langs.el --- -*- lexical-binding: t -*-

(use-package jinja2-mode
  :load-path (lambda () (expand-file-name "site-elisp/jinja2-mode" user-emacs-directory))
  :mode "\\.j2\\'")

(use-package yaml-mode
  :load-path (lambda () (expand-file-name "site-elisp/yaml-mode" user-emacs-directory))
  :mode "\\.yml\\'")

(use-package json-mode
  :ensure t)

(provide 'init-prog-markup-langs)
