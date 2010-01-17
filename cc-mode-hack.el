(substitute-key-definition 'c-electric-brace 'c-hack-electric-brace
                           c-mode-base-map)

(defun c-hack-balance-brace ()
  "Insert a corresponding closing brace."
  (interactive "*")
  (save-excursion
    (let ((p (point)))
      (insert ?\;)
      (c-newline-and-indent)
      (insert ?\})
      (c-indent-line-or-region)
      (goto-char p)
      (delete-char 1))))

(defun c-hack-move-past-brace ()
  "Delete the trailing blank lines before the closing brace and move
past it."
  (interactive "*")
  (backward-up-list -1)
  (save-excursion
    (beginning-of-line)
    (delete-region (point)
                   (if (re-search-backward "[^ \t\n\\]" nil t)
                       (progn (forward-line 1) (point))
                     (point-min)))))

(defun c-hack-electric-brace (arg)
  "Insert a brace.

If `c-electric-flag' is non-nil, the brace is not inside a literal and a
numeric ARG hasn't been supplied, the command performs several electric
actions:

\(a) If the auto-newline feature is turned on (indicated by \"/la\" on
the mode line) newlines are inserted before and after the brace as
directed by the settings in `c-hanging-braces-alist'.

\(b) Any auto-newlines are indented.  The original line is also
reindented unless `c-syntactic-indentation' is nil.

\(c) If auto-newline is turned on, various newline cleanups based on the
settings of `c-cleanup-list' are done."

  (interactive "*P")
  (let (safepos literal
                ;; We want to inhibit blinking the paren since this would be
                ;; most disruptive.  We'll blink it ourselves later on.
                (old-blink-paren blink-paren-function)
                blink-paren-function)

    (c-save-buffer-state ()
      (setq safepos (c-safe-position (point) (c-parse-state))
	    literal (c-in-literal safepos)))

    ;; Insert the brace.  Note that expand-abbrev might reindent
    ;; the line here if there's a preceding "else" or something.
    (if (or literal
            (eq last-command-event ?\{))
        (self-insert-command (prefix-numeric-value arg))
      (c-hack-move-past-brace))

    (when (and c-electric-flag (not literal) (not arg))
      (if (not (looking-at "[ \t]*\\\\?$"))
	  (if c-syntactic-indentation
	      (indent-according-to-mode))

	(let ( ;; shut this up too
	      (c-echo-syntactic-information-p nil)
	      newlines
	      ln-syntax br-syntax syntax) ; Syntactic context of the original line,
                                        ; of the brace itself, of the line the brace ends up on.
	  (c-save-buffer-state ((c-syntactic-indentation-in-macros t)
				(c-auto-newline-analysis t))
	    (setq ln-syntax (c-guess-basic-syntax)))
	  (if c-syntactic-indentation
	      (c-indent-line ln-syntax))

	  (when c-auto-newline
	    (backward-char)
	    (setq br-syntax (c-point-syntax)
		  newlines (c-brace-newlines br-syntax))

	    ;; Insert the BEFORE newline, if wanted, and reindent the newline.
	    (if (and (memq 'before newlines)
		     (> (current-column) (current-indentation)))
		(if c-syntactic-indentation
		    ;; Only a plain newline for now - it's indented
		    ;; after the cleanups when the line has its final
		    ;; appearance.
		    (newline)
		  (c-newline-and-indent)))
	    (forward-char)

	    ;; `syntax' is the syntactic context of the line which ends up
	    ;; with the brace on it.
	    (setq syntax (if (memq 'before newlines) br-syntax ln-syntax))

	    ;; Do all appropriate clean ups
	    (let ((here (point))
		  (pos (- (point-max) (point)))
		  mbeg mend
		  )

	      ;; `}': clean up empty defun braces
	      (when (c-save-buffer-state ()
		      (and (memq 'empty-defun-braces c-cleanup-list)
			   (eq last-command-event ?\})
			   (c-intersect-lists '(defun-close class-close inline-close)
					      syntax)
			   (progn
			     (forward-char -1)
			     (c-skip-ws-backward)
			     (eq (char-before) ?\{))
			   ;; make sure matching open brace isn't in a comment
			   (not (c-in-literal))))
		(delete-region (point) (1- here))
		(setq here (- (point-max) pos)))
	      (goto-char here)

	      ;; `}': compact to a one-liner defun?
	      (save-match-data
		(when
		    (and (eq last-command-event ?\})
			 (memq 'one-liner-defun c-cleanup-list)
			 (c-intersect-lists '(defun-close) syntax)
			 (c-try-one-liner))
		  (setq here (- (point-max) pos))))

	      ;; `{': clean up brace-else-brace and brace-elseif-brace
	      (when (eq last-command-event ?\{)
		(cond
		 ((and (memq 'brace-else-brace c-cleanup-list)
		       (re-search-backward
			(concat "}"
				"\\([ \t\n]\\|\\\\\n\\)*"
				"else"
				"\\([ \t\n]\\|\\\\\n\\)*"
				"{"
				"\\=")
			nil t))
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert-and-inherit "} else {"))
		 ((and (memq 'brace-elseif-brace c-cleanup-list)
		       (progn
			 (goto-char (1- here))
			 (setq mend (point))
			 (c-skip-ws-backward)
			 (setq mbeg (point))
			 (eq (char-before) ?\)))
		       (zerop (c-save-buffer-state nil (c-backward-token-2 1 t)))
		       (eq (char-after) ?\()
                                        ; (progn
                                        ; (setq tmp (point))
                       (re-search-backward
                        (concat "}"
                                "\\([ \t\n]\\|\\\\\n\\)*"
                                "else"
                                "\\([ \t\n]\\|\\\\\n\\)+"
                                "if"
                                "\\([ \t\n]\\|\\\\\n\\)*"
                                "\\=")
                        nil t)          ;)
                                        ;(eq (match-end 0) tmp);
                       )
		  (delete-region mbeg mend)
		  (goto-char mbeg)
		  (insert ?\ ))))

	      (goto-char (- (point-max) pos))

	      ;; Indent the line after the cleanups since it might
	      ;; very well indent differently due to them, e.g. if
	      ;; c-indent-one-line-block is used together with the
	      ;; one-liner-defun cleanup.
	      (when c-syntactic-indentation
		(c-indent-line)))

	    ;; does a newline go after the brace?
	    (if (memq 'after newlines)
		(c-newline-and-indent))
	    ))))

    ;; blink the paren
    (and (eq last-command-event ?\})
	 (not executing-kbd-macro)
	 old-blink-paren
	 (save-excursion
	   (c-save-buffer-state nil
	     (c-backward-syntactic-ws safepos))
	   (funcall old-blink-paren)))

    ;; Add a closing brace corresponding to an open one.
    (when (and (eq last-command-event ?\{)
               (not literal))
      (c-hack-balance-brace))))