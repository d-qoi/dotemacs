;;; init-editing.el --- -*- lexical-binding: t -*-

(use-package iedit
  :ensure t
  :bind ("C-z ," . iedit-mode)
  :diminish)

(use-package iy-go-to-char
  :load-path (lambda () (expand-file-name "site-elisp" user-emacs-directory))
  :bind (("M-z g f" . iy-go-to-char)
         ("M-z g F" . iy-go-to-char-backward)
         ("M-z g ;" . iy-go-to-or-up-to-continue)
         ("M-z g ," . iy-go-to-or-up-to-continue-backward)))

(use-package highlight
  :ensure t)


(provide 'init-editing)
