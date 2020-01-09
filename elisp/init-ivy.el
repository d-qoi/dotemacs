;; init-ivy.el --- -*- lexical-binding: t -*-

(use-package ivy
  :ensure t
  :diminish
  :bind
  ((:map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history)
         ("M-RET" . ivy-immediate-done)))
  :custom
  (ivy-mode 1)
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t))

(use-package counsel
  :ensure t
  :requires ivy
  :diminish
  :bind
  (("C-z s" . counsel-ag)
   ("C-z C-b" . counsel-ibuffer)
   ("C-z b" . counsel-buffer-or-recentf)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   (:map counsel-find-file-map
         ("C-~" . counsel-goto-local-home)))
  :config
  (counsel-mode 1)
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
      (ivy--cd "~/")))

(use-package swiper
  :ensure t
  :requires ivy
  :bind (("C-s" . swiper-isearch)))


(use-package amx
  :ensure t
  :requires ivy
  :disabled
  :defer t)

(provide 'init-ivy)
