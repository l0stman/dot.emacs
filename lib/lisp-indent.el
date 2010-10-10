(eval-when-compile
  (require 'cl))

(declare-function paredit-forward "paredit" nil)
(declare-function paredit-close-round "paredit" nil)

(defun simple-parse-assign (beg-sexp end-sexp)
  (save-excursion
    (down-list)
    (let ((maxlen 0)
          (argc 0)
          (res nil))
      (flet ((even? (n) (eq (logand n 1) 0)))
        (loop do
              (let ((beg (progn (skip-chars-forward " \n") (point)))
                    (end (progn (paredit-forward) (point))))
                (when (>= end end-sexp)
                  (if (even? argc)
                      (error "odd number of args in %s"
                             (buffer-substring-no-properties beg-sexp
                                                             end-sexp))
                    (return (values (nreverse res)
                                    (make-string (1+ maxlen) ?\ )))))
                (incf argc)
                (let* ((exp (buffer-substring-no-properties beg end))
                       (len (length exp)))
                  (push exp res)
                  (when (and (even? argc) (> len maxlen))
                    (setq maxlen len)))))))))

(defun new-paredit-close-round ()
  "Just like `paredit-close-round' except that arguments to setq
and setf are aligned automatically."
  (interactive "*")
  (let ((orig (point)))
    (condition-case err
        (progn
          (backward-up-list)
          (let* ((beg (point))
                 (end (scan-sexps beg 1)))
            (if (looking-at "(set[qf]")
                (multiple-value-bind (sexp pad) (simple-parse-assign beg end)
                  (delete-region beg end)
                  (insert-parentheses)
                  (insert (car sexp) ?\ )
                  (loop for (var val) on (cdr sexp) by #'cddr do
                        (insert
                         (format "%s%s%s"
                                 var
                                 (substring pad (length var))
                                 val))
                        (reindent-then-newline-and-indent))
                  (paredit-close-round))
              (forward-sexp))))
      (error
       (goto-char orig)
       (message "%s" (error-message-string err))))))

(provide 'lisp-indent)