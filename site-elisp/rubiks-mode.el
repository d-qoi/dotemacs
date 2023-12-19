(defvar rubiks-moves '("U" "U'" "D" "D'" "L" "L'" "R" "R'" "F" "F'" "B" "B'"))
(defvar rubiks-times-list nil
  "List of recorded times.")

(defvar rubiks-timer-start-time nil
  "Start time for the timer.")

;;;###autoload
(define-minor-mode rubiks-timer-mode
  "A Rubik's cube timer for Emacs."
  :lighter " Rubiks[S]"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'rubiks-timer-toggle)
            (define-key map (kbd "r") 'rubiks-timer-reset)
            (define-key map (kbd "q") 'rubiks-timer-quit)
            map)
  (if rubiks-timer-mode
      (progn
        (switch-to-buffer "*Rubik's Timer*")
        (rubiks-print "Initial scramble: %s" (rubiks-timer-gen-scramble)))
    (kill-buffer "*Rubik's Timer*")))

(defun rubiks-timer-update-lighter (text)
  "Update the rubiks-timer-mode lighter with TEXT."
  (setq rubiks-timer-mode-lighter (format " Rubiks[%s]" text))
  (force-mode-line-update))

(defun rubiks-print (&rest args)
  "Print ARGS to *Rubik's Timer* buffer."
  (let ((message-string (apply 'format args)))
    (with-current-buffer "*Rubik's Timer*"
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert message-string "\n")
      (setq buffer-read-only t))))


(defun rubiks-average (times n)
  "Compute the average of the last N times from TIMES list."
  (if (>= (length times) n)
      (/ (apply '+ (cl-subseq times 0 n)) n)
    nil))

(defun rubiks-print-average (times n)
  "Print the average of the last N times from TIMES list."
  (let ((avg (rubiks-average times n)))
    (when avg
      (rubiks-print "Ao%d: %f seconds" n avg))))

(defun rubiks-timer-quit ()
  "Quit rubiks-timer-mode and kill the buffer."
  (interactive)
  (rubiks-timer-mode -1))

(defun rubiks-timer-toggle ()
  "Toggle between start and stop timer"
  (interactive)
  (if rubiks-timer-start-time
      (rubiks-timer-stop)
    (rubiks-timer-start)))

(defun rubiks-timer-start ()
  (interactive)
  (setq rubiks-timer-start-time (current-time))
  (rubiks-print "Timer started on attempt %d..." (length rubiks-times-list))
  (rubiks-timer-update-lighter "R"))

(defun rubiks-timer-stop ()
  (interactive)
  (let ((elapsed (float-time (time-subtract (current-time) rubiks-timer-start-time))))
    (push elapsed rubiks-times-list))
  (setq rubiks-timer-start-time nil)
  (rubiks-print "Time: %s seconds" (car rubiks-times-list))
  (rubiks-print-average rubiks-times-list 5)
  (rubiks-print-average rubiks-times-list 12)
  (rubiks-print "")
  (rubiks-print "Next scramble: %s" (rubiks-timer-gen-scramble))
  (rubiks-print "")
  (rubiks-timer-update-lighter "S")
  )

(defun rubiks-timer-reset ()
  (interactive)
  (setq rubiks-timer-start-time nil)
  (setq rubiks-times-list nil)
  (rubiks-print "--------------------Timer reset--------------------")
  (rubiks-print ""))


(defun rubiks-timer-gen-scramble ()
  "Generate a Rubik's cube scramble."
  (let ((scramble '())
        (last-move nil))
    (dotimes (_ 20)
      (let ((move (nth (random (length rubiks-moves)) rubiks-moves)))
        ;; Ensure the move is not the inverse or the same as the last move.
        (while (or (and last-move (string= move (rubiks-inverse-move last-move)))
                   (and last-move (string= move last-move)))
          (setq move (nth (random (length rubiks-moves)) rubiks-moves)))
        (push move scramble)
        (setq last-move move)))
    (mapconcat 'identity (nreverse scramble) " ")))

(defun rubiks-inverse-move (move)
  "Get the inverse of a Rubik's move."
  (cond
   ((string= move "U") "U'")
   ((string= move "U'") "U")
   ((string= move "D") "D'")
   ((string= move "D'") "D")
   ((string= move "L") "L'")
   ((string= move "L'") "L")
   ((string= move "R") "R'")
   ((string= move "R'") "R")
   ((string= move "F") "F'")
   ((string= move "F'") "F")
   ((string= move "B") "B'")
   ((string= move "B'") "B")
   (t move))) ; return the move unchanged if not matched
