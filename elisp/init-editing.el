;; -*- lexical-binding: t -*-

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

(use-package wgrep
  :straight (:repo "mhayashi1120/Emacs-wgrep"))

(use-package isearch
  :straight (:type built-in)
  :demand
  :bind
  (:map isearch-mode-map
   ("M-s M-s" . d-qoi/isearch-transient-menu))
  :init
  (transient-define-prefix d-qoi/isearch-transient-menu ()
    "ISearch Assist functions, in Transient Form!"
    [["Edit String Search"
      ("e" "Edit the search string (recursive)" isearch-edit-string)
      ("w" "Pull next word or char word from buffer" isearch-yank-word-or-char)
      ("s" "Pull next symbol or char from buffer" isearch-yank-symbol-or-char)
      ("l" "Pull rest of line from buffer" isearch-yank-line)
      ("y" "Pull string from kill ring" isearch-yank-kill)
      ("t" "Pull thing at point from buffer" isearch-forward-thing-at-point)
      ]
     ["Replace"
      ("q" "Start 'query-replace'" anzu-isearch-query-replace :if-nil buffer-read-only)
      ("x" "start 'query-replace-regex'" anzu-isearch-query-replace-regexp :if-nil buffer-read-only)
      ]]
    [["Toggle"
      ("X" "Toggle regexp searching" isearch-toggle-regexp)
      ("S" "Toggle symbol searching" isearch-toggle-symbol)
      ("W" "Toggle case fold" isearch-toggle-case-fold)
      ("L" "Toggle lax whitespace" isearch-toggle-lax-whitespace)]
     ["Misc"
      ("o" "Occur" isearch-occur)
      ]]))

(provide 'init-editing)
