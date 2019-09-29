;; HELM - the beast
(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("M-Y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-h SPC" . helm-all-mark-rings)
         ([remap find-tag] . helm-etags-select)
         ([remap list-buffers] . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-grep-mode-map
         ("<return>" . helm-grep-mode-jump-other-window)
         ("n" . helm-grep-mode-jump-other-window-forward)
         ("p" . helm-grep-mode-jump-other-window-backwards)
         :map minibuffer-local-map
         ("M-p" . helm-minibuffer-history)
         ("M-n" . helm-minibuffer-history))
  :init ;; before everything is loaded
  (progn
    (setq helm-command-prefix-key "C-c h")
    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil)))))
  :config
  (require 'helm-config)
  (require 'helm-grep)
  (require 'helm-eshell)
  (setq helm-google-suggest-use-curl-p t
        helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
        ;; helm-quick-update t ; do not display invisible candidates
        helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
        ;; you can customize helm-do-grep to execute ack-grep
        ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
        ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
        helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
        helm-echo-input-in-header-line t
        ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
        helm-ff-file-name-history-use-recentf t
        helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
        helm-buffer-skip-remote-checking t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                      ; useful in helm-mini that lists buffers
        helm-org-headings-fontify t
        ;; helm-find-files-sort-directories t
        ;; ido-use-virtual-buffers t
        helm-semantic-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        ;; helm-apropos-fuzzy-match t
        helm-buffer-skip-remote-checking t
        helm-locate-fuzzy-match t
        helm-display-header-line nil)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)
  (global-set-key (kbd "C-c h x") 'helm-register)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (add-hook 'eshell-mode-hook #'(lambda () (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (helm-mode 1))

(use-package helm-swoop
  :after (helm)
  :ensure t
  :bind (("C-c s" . helm-multi-swoop-all)
         ("C-c h s" . helm-swoop)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop))
  :config
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows t)

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t))

(use-package helm-ag
  :if (executable-find "ag")
  :after (helm)
  :ensure t
  :init
  (global-set-key (kbd "C-c a g") 'helm-do-ag))

(provide 'init-helm)
