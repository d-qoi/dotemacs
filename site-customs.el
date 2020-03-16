(defun d-qoi/clean-cscope-files ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (flush-lines " ")
    (flush-lines "_out/")))

(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)
