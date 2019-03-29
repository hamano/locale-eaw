
(defun eaw-set-width (width)
  (when (= emacs-major-version 22)
    (utf-translate-cjk-set-unicode-range east-asian-ambiguous))
  (when (>= emacs-major-version 23)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (mapc (lambda (range) (set-char-table-range table range width))
            east-asian-ambiguous)
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(defun eaw-fullwidth ()
  (eaw-set-width 2))

(provide 'eaw)

;;; eaw.el ends here
