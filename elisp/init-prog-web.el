;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)

(defcustom *typescript-language-server*
  (executable-find "typescript-language-server")
  "Do we have the typescript language server?"
  :group 'javascript
  :group 'typescript)

(use-package emmet-mode
  :straight t
  :after web-mode
  :hook (css-mode . emmet-mode)
  :hook (sgml-mode . emmet-mode)
  :hook (web-mode . emmet-mode)
  :hook (js-base-mode . emmet-mode)
  :custom
  (emmet-move-cursor-between-quotes t)
  :config
  (add-to-list 'emmet-jsx-major-modes 'js-ts-mode)
  (add-to-list 'emmet-jsx-major-modes 'js-mode)
  (add-to-list 'emmet-jsx-major-modes 'typescript-ts-mode)
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
  (add-hook 'tsx-ts-mode-hook 'emmet-mode))

(use-package web-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

(when (treesit-available-p)
  (when (or (treesit-language-available-p 'typescript)
            (treesit-language-available-p 'tsx)
            (treesit-language-available-p 'javascript))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))))

(when *typescript-language-server*
  (add-hook 'js-base-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-base-mode-hook 'eglot-ensure))

(provide 'init-prog-web)
