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
         ("M-z u p" . mc/unmark=previous-like-this)
         :repeat-map d-qoi/mc/repeat-mark-map
         ("n" . mc/mark-next-like-this)
         ("p" . mc/mark-previous-like-this)
         ("m" . mc/mark-pop)))

(use-package visual-replace
  :straight (:host github :repo "szermatt/visual-replace")
  :diminish
  :config
  (define-key query-replace-map "p" 'backup)
  (define-key query-replace-map "P" 'backup)
  (visual-replace-global-mode 1))

(use-package wgrep
  :straight (:repo "mhayashi1120/Emacs-wgrep"))

(use-package isearch
  :straight (:type built-in)
  :after avy
  :demand
  :bind
  (:map isearch-mode-map
        ("M-s M-s" . d-qoi/isearch-transient-menu)
        ("M-j" . avy-isearch))
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
      ("r" "Start visual-replace'" visual-replace-from-isearch :if-nil buffer-read-only)
      ;("q" "Start 'query-replace'" anzu-isearch-query-replace :if-nil buffer-read-only)
      ;("x" "start 'query-replace-regex'" anzu-isearch-query-replace-regexp :if-nil buffer-read-only)
      ]]
    [["Toggle"
      ("X" "Toggle regexp searching" isearch-toggle-regexp)
      ("S" "Toggle symbol searching" isearch-toggle-symbol)
      ("W" "Toggle case fold" isearch-toggle-case-fold)
      ("L" "Toggle lax whitespace" isearch-toggle-lax-whitespace)]
     ["Misc"
      ("o" "Occur" isearch-occur)
      ]]))


(use-package crux
  :straight t
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-c o" . crux-open-with)
   ("s-r" . crux-recentf-find-file)
   ("C-<backspace>" . crux-kill-line-backwards)))

(provide 'init-editing)
