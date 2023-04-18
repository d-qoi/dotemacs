;; -*- lexical-binding -*-

(use-package dired
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  (dired-dwim-target t))

(defun d-qoi/term-toggle-line-char ()
  "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
  (interactive)
  (cond
   ((term-in-line-mode)
    (term-char-mode)
    (hl-line-mode -1))
   ((term-in-char-mode)
    (term-line-mode)
    (hl-line-mode 1))))

(use-package multi-term
  :commands (multi-term)
  :bind
  (("M-$" . multi-term)
   (:map term-raw-map
         ("C->" . multi-term-next)
         ("C-<" . multi-term-prev)
         ("C-t" . d-qoi/term-toggle-line-char))
   (:map term-mode-map
         ("C-t" . d-qoi/term-toggle-line-char))))

(provide 'init-nav)