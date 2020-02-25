(defun d-qoi/clean-cscope-files ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (flush-lines " ")
    (flush-lines "_out/")))
