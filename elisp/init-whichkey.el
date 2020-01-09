;;; init-whichkey.el --- -*- lexical-binding: t -*-

(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(provide 'init-whichkey)
