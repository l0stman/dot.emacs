(require 'cl)				; Common Lisp library

(defmacro defkeys (map &rest bindings)
  "Define the bindings represented as property list of keys and
functions in the key mapping map (the global one if null)."
  (eval-after-load "cl"
    '`(progn
	,@(loop for plist on bindings by #'cddr
		collect (let ((key (car plist)) (fn (cadr plist)))
			  (if map
			      `(define-key ,map (kbd ,key) ',fn)
			    `(global-set-key (kbd ,key) ',fn)))))))

(defmacro subskeys (map &rest funcs)
  "Replace in the keymap map the old definitions of functions
with new ones represented as property list."
  (eval-after-load "cl"
    '`(progn
        ,@(loop for plist on funcs by #'cddr
                collect `(substitute-key-definition
                          ',(car plist) ',(cadr plist) ,map)))))

(defun symb (&rest args)
  "Produce a symbol from the lisp objects."
  (intern
   (with-output-to-string
     (dolist (o args) (princ o)))))

(defmacro add-to-alist (alist &rest bindings)
  "Add the entries to an association list if they aren't there
yet.  Otherwise update the corresponding entries."
  `(progn
     ,@(mapcar #'(lambda (bind)
                   `(let ((entry (assq ',(car bind) ,alist)))
                      (if entry
                          (setf (cdr entry) ',(cdr bind))
                        (push ',bind ,alist))))
               bindings)))

(provide 'utils)