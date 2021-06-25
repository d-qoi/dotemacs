;;; init-org.el --- -*- lexical-binding: t -*-

(use-package org
  :ensure t
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ;; ("C-h" . org-delete-backward-char)
         ("C-c !" . org-time-stamp-inactive))
  :mode ("\\.org$" . org-mode)
  :custom
  (org-log-done t)
  :config
  (require 'org-id)
  (setq org-agenda-files (list "~/org/tasks.org")))

(provide 'init-org)
