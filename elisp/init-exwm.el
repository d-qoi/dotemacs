(require 'cl-generic)

(if (and (file-directory-p "~/.emacs.d/exwm")
         (file-directory-p "~/.emacs.d/xelb"))
    (progn
      (add-to-list 'load-path "~/.emacs.d/exwm")
      (add-to-list 'load-path "~/.emacs.d/xelb"))
  (error "exwm and xelb not found, initialize submodules"))

(use-package exwm
  :config
  (require 'exwm-config)
  (setq exwm-workspace-number 9)
  
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-d': Launch application
  (exwm-input-set-key (kbd "s-d")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (require 'exwm-randr)
  (defvar exwm-randr-workspace-output-plist)
  (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  (add-hook 'exwm-randr-screen-change-hood
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  (exwm-randr-enable)

  ;; Other configurations
  (exwm-config-misc))

(use-package exwm-edit
  :ensure t
  :after (exwm)
  :config
  (require 'markdown-mode)
  (defun on-exwm-edit-compose ()
    (markdown-mode))

  (add-hook 'exwm-edit-compose-hook 'on-exwm-edit-compose))

(use-package gpastel
  :after (exwm)
  :ensure t)

(provide 'init-exwm)
