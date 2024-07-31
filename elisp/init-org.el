;; -*- lexical-binding: t -*-

(require 'use-package)

(defcustom *org-agenda-file-listing* (list "~/tasks.org")
  "The file list default for org-agenda-files,
   creates it if not created,
   propogates it with org-agenda-file-default"
  :type '(repeat file)
  :group 'org-agenda)
(defcustom *org-agenda-file-default* "~/todo.org"
  "Default initial file for org-mode"
  :type '(file)
  :group 'org-agenda)

(use-package org
  :straight (:type built-in)
  :mode ("\\.org$" . org-mode)
  :demand t
  :bind
  ("C-c a a" . org-agenda)
  ("C-c a l" . org-store-link)
  ("C-c a c" . org-capture)
  :init
  (unless (file-exists-p *org-agenda-file-default*)
    (make-empty-file *org-agenda-file-default*))
  (dolist (*org-agenda-file*  *org-agenda-file-listing*)
    (unless (file-exists-p *org-agenda-file*)
      (make-empty-file *org-agenda-file*)
      (with-temp-file *org-agenda-file*
	    (insert *org-agenda-file-default* "\n"))))
  (setq org-agenda-files *org-agenda-file-listing*)
  :custom
  (org-log-done t)
  (org-babel-python-command "python3")
  (org-return-follows-link t)
  (org-capture-templates
        '(("s" "Slipbox" plain
          (file "~/org/slipbox.org")
          "* %?\n"
          :empty-lines 1)
          ("t" "Todo" plain
          (file *org-agenda-file-default*)
          "* TODO %?\n")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(use-package org-novelist
  :straight (:host github :repo "sympodius/org-novelist")
  :after org
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
  :disabled
  :bind
  (:map text-mode-map
        ("C-c p" . powerthesaurus-lookup-dwim)))

(use-package denote
  :straight t
  :after org
  :bind (("C-c n n" . denote)
         ("C-c n c" . denote-region) ; "contents" mnemonic
         ("C-c n N" . denote-type)
         ("C-c n d" . denote-date)
         ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
         ("C-c n s" . denote-subdirectory)
         ("C-c n t" . denote-template)
         ("C-c n i" . denote-link) ; "insert" mnemonic
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n f f" . denote-find-link)
         ("C-c n f b" . denote-find-backlink)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         :map dired-mode-map
         ("C-c C-d C-i" . denote-link-dired-marked-notes)
         ("C-c C-d C-r" . denote-dired-rename-marked-files)
         ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :init
  (setq denote-directory "~/Documents/Notes/")
  (setq denote-known-keywords '("notes" "ideas"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-date-format nil)

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t))

;; (defun denote--default-directory-is-silo-p ()
;;   "Return path to silo if `default-directory' is a silo."
;;   (when-let ((dir-locals (dir-locals-find-file default-directory))
;;              (local-val (alist-get 'denote-directory dir-local-variables-alist)))
;;     (cond
;;      ((and (stringp local-val)
;;            (file-directory-p local-val))
;;       local-val)
;;       ((listp dir-locals)
;;        (car dir-locals))
;;       ((stringp dir-locals)
;;        dir-locals))))

(provide 'init-org)
