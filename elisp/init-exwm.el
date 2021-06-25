(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (setq exwm-workspace-number 9)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))


  ;; 's-N': Switch to certain workspace
  ;; (dotimes (i 10)
  ;;   (exwm-input-set-key (kbd (format "s-%d" i))
  ;;                       `(lambda ()
  ;;                          (interactive)
  ;;                          (exwm-workspace-switch-create ,i))))

  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)
          ;; 's-w': Switch workspace.
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-d': Launch application.
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-l] . (lambda () (interactive)(start-process "" nil "loginctl" "lock-session")))
           ;; 's-N': Switch to certain workspace.
           ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                         (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
                     (number-sequence 0 9))))
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
  (add-hook 'exwm-randr-screen-change-hook
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
