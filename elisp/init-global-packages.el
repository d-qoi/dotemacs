;; -*- lexical-binding: t -*-
;;; init-global-packages.el ---

(require 'use-package)

(use-package diminish
  :straight t)

(use-package f
  :straight t)

(use-package dash
  :straight t)

(use-package s
  :straight t)

(use-package transient
  :straight t)

(use-package transient-dwim
  :straight t
  :after (:all (transient magit)))

(use-package which-key
  :straight t
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :demand t
  :bind
  (:map vertico-map
        ("?" . minibuffer-completion-help)
        ("M-RET" . minibuffer-force-complete-and-exit)
        ("M-TAB" . minibuffer-complete)
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :init
  (setq vertico-multiform-commands
        '((consult-imenu . (buffer indexed))
          (consult-line buffer)
          (execute-extended-command flat)
          (describe-symbol (vertico-sort-function . vertico-sort-alpha))))
  (setq vertico-multiform-categories
        '((file . (buffer indexed))
          (imenu . (:not indexed mouse))
          (describe-symbol (vertico-sort-function . vertico-sort-alpha))))
  :config
  (vertico-mode 1)
  (vertico-multiform-mode))

(use-package consult
  ;; Copied from github initially and modified for my own use
  :straight t
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind (("M-s n" . consult-notes))
  :config
  ;; Not setting consult-notes-file-dir-sources as I'm only using denote
  ;; (setq consult-notes-file-dir-sources '(("Name"  ?key  "path/to/dir"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files)))

(use-package orderless
  :straight t
  :demand t
  :config
  (add-to-list 'orderless-affix-dispatch-alist
               '(?> . substring))  ;; 62 is '>'
  (add-to-list 'orderless-affix-dispatch-alist
               '(?< . orderless-prefixes))
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(use-package marginalia
  :straight t
  :demand t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package embark
  :straight t
  :demand t
  :after marginalia
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-c e a" . embark-act)
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-c e d" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternativ efor `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions 'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :demand t
  :after (:all embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(defun avy-action-kill-line (pt)
  (save-excursion
    (goto pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-copy-line (pt)
  (save-excursion
    (goto pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-line (pt)
  (avy-action-kill-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(use-package avy
  :straight (:host github :repo "abo-abo/avy")
  :demand t
  :config
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-line
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-line
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark
        )
  :bind
  (("C-:" . avy-goto-char-timer)))

(use-package activities
  :straight (:host github :repo "alphapapa/activities.el")
  :demand t
  :init
  (activities-mode)
  (activities-tabs-mode)

  :bind
  (("C-x t C-n" . activities-new)
   ("C-x t C-d" . activities-define)
   ("C-x t C-a" . activities-resume)
   ("C-x t C-s" . activities-suspend)
   ("C-x t C-k" . activities-kill)
   ("C-x t RET" . activities-switch)
   ("C-x t C-b" . activities-switch-buffer)
   ("C-x t g" . activities-revert)
   ("C-x t l" . activities-list)))

(defun d-qoi/devil-find-special-advice (retval)
  "Hide which key popup when devil find special is activated"
  (when retval
    (which-key--hide-popup))
  retval)

(defun d-qoi/devil-find-regular-advice (key)
  "This allows which-key to search devil-mode partial maps."
  (let* ((translated-key (devil--translate key))
         (parsed-key (ignore-errors (kbd translated-key)))
         (binding (when parsed-key (key-binding parsed-key))))
    (if (and binding (keymapp binding))
        (which-key--show-keymap parsed-key binding nil nil t)
      (which-key--hide-popup))))

(use-package devil
  :straight t
  :demand t
  :config
  (setq devil-lighter " \U0001F608")
  (setq devil-prompt "\U0001F608 %t")
  (global-set-key (kbd "C-|") 'global-devil-mode)
  (advice-add 'devil--find-special-command :filter-return #'d-qoi/devil-find-special-advice)
  (advice-add 'devil--find-regular-command :after #'d-qoi/devil-find-regular-advice))

(provide 'init-global-packages)
