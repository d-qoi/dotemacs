;; -*- lexical-binding: t -*-

(require 'use-package)

(defcustom *org-agenda-file-listing* "~/.org_agenda_files"
  "The file list default for org-agenda-files,
   creates it if not created,
   propogates it with org-agenda-file-default"
  :type '(file))
(defcustom *org-agenda-file-default* "~/todo.org"
  "Default initial file for org-mode"
  :type '(file))

(use-package org-mode
  :mode "\\.org$"
  :bind
  ("C-c a a" . org-agenda)
  ("C-c a l" . org-store-link)
  ("C-c a c" . org-capture)
  :init
  (unless (file-exists-p *org-agenda-file-default*)
    (make-empty-file *org-agenda-file-default*))
  (unless (file-exists-p *org-agenda-file-listing*)
    (make-empty-file *org-agenda-file-listing*)
    (with-temp-file *org-agenda-file-listing*
	  (insert *org-agenda-file-default* "\n")))
  :custom
  (org-return-follows-link t)
  (org-agenda-files *org-agenda-file-listing*)
  (org-capture-templates
        '(("s" "Slipbox" plain
          (file "~/org/slipbox.org")
          "* %?\n"
          :empty-lines 1)
          ("t" "Todo" plain
          (file *org-agenda-file-default*)
          "* TODO %?\n"))))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*"))
  :custom
  (org-roam-directory (file-truename "~/org/"))
  (org-roam-capture-templates
   '(("c" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "main" plain "%?"
      :if-new (file+head "main/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t
      :empty-lines 1)
     ("r" "references" plain "%?"
      :if-new (file+head "refs/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t
      :empty-lines 1)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-novelist
  :straight (:host github :repo "sympodius/org-novelist")
  :bind
  (:map org-novelist-mode-map
        ("C-c N p" . org-novelist-new-prop)
        ("C-c N c" . org-novelist-new-character)
        ("C-c N l" . org-novelist-new-place)
        ("C-c N C" . org-novelist-new-chapter)
        ("C-c N u" . org-novelist-update-references))
  :custom
  (org-novelist-language-tag "en-GB")
  (org-novelist-automatic-referencing-p nil))

(use-package powerthesaurus
  :straight t
  :bind
  (:map text-mode-map
        ("C-c p" . powerthesaurus-lookup-dwim)))

(provide 'init-org)
