;;; init-org.el --- -*- lexical-binding: t -*-

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ;; ("C-h" . org-delete-backward-char)
         ("C-c !" . org-time-stamp-inactive)
         ("C-c $" . ispell-buffer)
         ("C-c 4" . ispell-word))
  :mode ("\\.org$" . org-mode)
  :custom
  (org-log-done t)
  (org-return-follows-link t)
  :config
  (require 'org-id)
  (setq org-agenda-files (list "~/org/tasks.org")))

(use-package org-novelist
  :load-path "site-elisp/org-novelist/"
  :demand
  :bind
  (:map org-mode-map
        ("C-z n c" . org-novelist-new-character)
        ("C-z n C" . org-novelist-new-chapter)
        ("C-z n p" . org-novelist-new-prop)
        ("C-z n l" . org-novelist-new-place))
  :custom
  (org-novelist-language-tag "en-GB")  ; The interface language for Org Novelist to use. It defaults to 'en-GB' when not set
  (org-novelist-author "Alex Hirschfeld")  ; The default author name to use when exporting a story. Each story can also override this setting
  (org-novelist-author-email "alex@d-qoi.com")  ; The default author contact email to use when exporting a story. Each story can also override this setting
  (org-novelist-automatic-referencing-p t)) ; Set this variable to 't' if you want Org Novelist to always keep note links up to date. This may slow down some systems when operating on complex stories. It defaults to 'nil' when not set

(provide 'init-org)
