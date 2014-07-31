(eval-when-compile
  (require 'cl))

(declare-function paredit-forward "paredit" nil)
(declare-function paredit-close-round "paredit" nil)
(declare-function paredit-reindent-defun "paredit" nil)

(defun simple-parse-assign (beg-sexp end-sexp)
  (save-excursion
    (down-list)
    (flet ((even? (n) (eq (logand n 1) 0)))
      (loop with maxlen = 0
            with argc = 0
            with res = nil
            do (let ((beg (progn (skip-chars-forward " \n") (point)))
                     (end (progn
                            (paredit-forward)
                            (when (and (even? argc)
                                       (looking-at "[ \t]*;"))
                              (move-end-of-line 1))
                            (point))))
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
                     (setq maxlen len))))))))

(defun paredit-lisp-indent-assign ()
  "Close parenthesis and align the values in a lisp assignment automatically.
\(setf (car x) 0 |b 3 c 4)
-->
\(setf (car x) 0
      b       3
      c       4)|"
  (interactive "*")
  (let ((orig (point)))
    (condition-case err
        (progn
          (backward-up-list)
          (let* ((beg (point))
                 (end (scan-sexps beg 1)))
            (multiple-value-bind (sexp pad) (simple-parse-assign beg end)
              (delete-region beg end)
              (insert-parentheses)
              (insert (car sexp) ?\ )
              (loop for (var val) on (cdr sexp) by #'cddr do
                    (insert var (substring pad (length var)) val)
                    (reindent-then-newline-and-indent))
              (paredit-close-round)
              (paredit-reindent-defun))))
      (error
       (goto-char orig)
       (message "%s" (error-message-string err))))))

(provide 'lisp-indent)