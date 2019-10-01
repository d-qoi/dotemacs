;; Linum mode
(require 'linum)
(global-linum-mode t)
(show-paren-mode 1)
(setq column-number-mode t)
(setq linum-format "%d|")

(defvar linum-disabled-modes
  '(term-mode help-mode exwm-mode image-mode))

(defun d-qoi/linum-disable-function ()
  "Function for the hook for after major mode change."
  (defun recloop (val lst)
    ;;(message "recloop %s %s" val (car lst))
    (cond
     ((equal '() lst) 1)
     ((equal val (car lst)) 0)
     (t (recloop val (cdr lst)))))
  (linum-mode (recloop major-mode linum-disabled-modes)))

(add-hook 'after-change-major-mode-hook 'd-qoi/linum-disable-function)



;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; laptop handling, battery mode determins if battery info will be displayed or not
(require 'battery)
(when (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B" (funcall battery-status-function)))))
  (display-battery-mode 1))

(provide 'init-default-packages)
