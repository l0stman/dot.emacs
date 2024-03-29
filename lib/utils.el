(require 'cl-lib)				; Common Lisp library

(defmacro defkeys (map &rest bindings)
  "Define the bindings represented as property list of keys and
functions in the key mapping map (the global one if null)."
  (eval-after-load "cl-lib"
    '`(progn
	,@(cl-loop for (key fn) on bindings by #'cddr
		collect (if map
                            `(define-key ,map (kbd ,key) ',fn)
                          `(global-set-key (kbd ,key) ',fn))))))

(defmacro subskeys (map &rest funcs)
  "Replace in the keymap map the old definitions of functions
with new ones represented as property list."
  (eval-after-load "cl-lib"
    '`(progn
        ,@(cl-loop for (old new) on funcs by #'cddr
                collect `(substitute-key-definition ',old ',new ,map)))))

(defun symb (&rest args)
  "Produce a symbol from the lisp objects."
  (intern
   (with-output-to-string
     (dolist (o args) (princ o)))))

(defmacro add-to-alist (alist &rest bindings)
  "Add the entries to an association list if they aren't there
yet.  Otherwise update the corresponding entries."
  (let ((entry (cl-gensym)))
   `(progn
      ,@(mapcar #'(lambda (bind)
                    `(let ((,entry (assq ',(car bind) ,alist)))
                       (if ,entry
                           (setf (cdr ,entry) ',(cdr bind))
                         (push ',bind ,alist))))
                bindings))))

(defun join-next-line ()
  "Join the current line with the next one."
  (interactive "*")
  (join-line 1))

(defun compose-french ()
  "Compose a text in French."
  (interactive "*")
  (set-input-method 'latin-1-prefix)
  (ispell-change-dictionary "francais"))

(defun reply-mail (arg)
  "Insert an empty space between the quoted email you're replying
to and your answer.  With a prefix argument, compose in French."
  (interactive "*P")
  (goto-char (point-min))
  (save-excursion
    (let ((start (point))
          (end (progn (skip-chars-forward " \t\n") (point))))
      (when (and (< (count-lines start end) 3)
                 (/= start (point-max)))
        (delete-region start end)
        (insert "\n\n"))))
  (text-mode)
  (when arg
    (compose-french)))

(provide 'utils)
