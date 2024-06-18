
(defun eaw-init ()
  (when (>= emacs-major-version 23)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (mapc (lambda (range) (set-char-table-range table range 1))
            code-half)
      (mapc (lambda (range) (set-char-table-range table range 2))
            code-wide)
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(eaw-init)

;;; eaw.el ends here
