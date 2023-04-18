;; -*- lexical-binding -*-

(use-package iedit
  :straight t
  :bind ("C-z ," . iedit-mode)
  :diminish)

(use-package multiple-cursors
  :straight t
  :bind (("M-z e e" . mc/edit-lines)
         ("M-z e b" . mc/edit-beginnings-of-lines)
         ("M-z e n" . mc/edit-ends-of-lines)
         ("M-z a a" . mc/mark-all-like-this)
         ("M-z a d" . mc/mark-all-dwim)
         ("M-z a r" . mc/mark-all-in-region)
         ("M-z n" . mc/mark-next-like-this)
         ("M-z w n" . mc/mark-next-list-this-word)
         ("M-z s n" . mc/mark-next-list-this-symbol)
         ("M-z p" . mc/mark-previous-like-this)
         ("M-z w p" . mc/mark-previous-like-this-word)
         ("M-z s p" . mc/mark-previous-like-this-symbol)
         ("M-z m" . mc/mark-pop)
         ("M-z u n" . mc/unmark-next-like-this)
         ("M-z u p" . mc/unmark=previous-like-this)))

(use-package anzu
  :straight t
  :diminish
  :bind
  (("C-z q r" . anzu-query-replace)
   ("C-z q R" . anzu-query-replace-regexp)
   ("C-z q ." . anzu-replace-at-cursor-thing))
  :config
  (global-anzu-mode 1))

(provide 'init-edit)
