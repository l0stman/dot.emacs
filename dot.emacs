;;;; -*-Emacs-Lisp-*-

(defvar *emacs-dir* (expand-file-name "~/.emacs.d")
  "Emacs personal root directory.")

(defsubst full-path (file)
  (expand-file-name file *emacs-dir*))

(defsubst add-to-list* (lst &rest args)
  (dolist (fn args) (add-to-list lst fn)))

(add-to-list* 'load-path
	      (full-path "lib")
	      (expand-file-name "~/hacks/slime"))

(require 'os-params)               ; OS specific parameters
(require 'utils)                   ; some utility macros and functions

;;;; Global key bindings
(defkeys nil
  ;; Function keys
  "<f1>" info
  "<f2>" help-command
  "<f3>" compile
  "<f4>" repeat-complex-command
  "<f5>" slime-selector

  ;; Meta
  "M-g" goto-line
  "M-n" next-error
  "M-p" previous-error

  ;; Hyper
  "H-h" help-command
  "H-s" slime-selector
  "H-r" query-replace
  "H-v" scroll-other-window-down

  ;; Super
  "s-k" kmacro-keymap

  ;; Misc
  "C-m" reindent-then-newline-and-indent
  "C-w" backward-kill-word
  "C-x C-k" kill-region
  "C-x g" magit-status
  "C-h" backward-delete-char-untabify
  "C-c k" join-line
  "C-c j" join-next-line
  "C-c f" find-library
  "C-c c" compose-french)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Custom theme
(load-theme 'dream t)

;;; Configure package.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Company.
(require 'company)
(global-company-mode)

;;; Ido.
(require 'ido)
(ido-mode t)

;;;; SLIME configuration for Common Lisp
(defvar *slime-dir* (file-name-directory (locate-library "slime")))
(defvar *sbcl-core* (full-path "sbcl.core-with-swank"))
(defvar *sbcl-dev-core* (full-path "sbcl-devel.core-with-swank"))
(defvar *fasls-dir* "/tmp/slime-fasls/")

(make-directory *fasls-dir* t)

(setq slime-net-coding-system        'utf-8-unix
      common-lisp-hyperspec-root     *hyperspec-dir*
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-compile-file-options     `(:fasl-directory ,*fasls-dir*)
      slime-lisp-implementations
      `((sbcl (,*sbcl-exec* "--noinform" "--core" ,*sbcl-core*)
              :init (lambda (port-file enc)
                      (format "(swank:start-server %S)\n" port-file))
              :env ((concat "SBCL_HOME=" *sbcl-home*)))
        (sbcl-devel (,(expand-file-name "~/bin/sbcl") "--noinform" "--core"
                     ,*sbcl-dev-core*)
                    :init (lambda (port-file enc)
                            (format "(swank:start-server %S)\n" port-file))
                    :env (,(concat "SBCL_HOME="
                                   (expand-file-name "~/lib/sbcl"))))
        (ccl ("ccl" "--terminal-encoding" "utf-8"))))

(add-to-list* 'load-path
              *slime-dir*
              (expand-file-name "contrib" *slime-dir*))
(require 'slime-autoloads)
(slime-setup '(slime-repl slime-autodoc slime-fuzzy slime-fancy-inspector
                          slime-indentation slime-presentations))

;;; Flycheck.
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;; For editing scheme code.
(require 'quack)

;;;; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(require 'lisp-indent)
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "H-)") 'paredit-lisp-indent-assign))

;;; Yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(defvar *paredit-mode-list*
  '(lisp scheme emacs-lisp lisp-interaction slime-repl inferior-scheme)
  "List of major modes using paredit.")

;;; Toggle paredit mode and bind <tab> to symbol completion.
(dolist (name *paredit-mode-list*)
  (add-hook (symb name '-mode-hook)
	    `(lambda ()
	       (enable-paredit-mode)
               (defkeys ,(symb name '-mode-map) "C-h" paredit-backward-delete)
               ,(let ((map (symb name '-mode-map)))
                  (cl-case name
                    ((emacs-lisp lisp-interaction)
                     `(defkeys ,map "<tab>" completion-at-point))
                    (lisp
                     `(defkeys ,map "<tab>" slime-complete-symbol)))))))

;;;; Adding some hooks.
;;; Make it easy to navigate by expression for programming mode
;;; and turn on flyspell.
(defvar *prog-mode-list*
  '(paredit c c++ sh awk mixal octave inferior-octave python inferior-python
            java js2)
  "List of programming mode names.")

(dolist (name *prog-mode-list*)
  (add-hook (symb name '-mode-hook)
	    `(lambda ()
	       (defkeys ,(symb name '-mode-map)
		 "C-t" transpose-sexps
		 "C-M-t" transpose-chars
		 "C-f" paredit-forward
		 "C-M-f" forward-char
		 "C-b" paredit-backward
		 "C-M-b" backward-char
		 "C-k" kill-sexp
		 "C-M-k" paredit-kill
		 "C-<backspace>" backward-kill-sexp)
               (flyspell-prog-mode))))

;;; Text mode
(add-hook 'text-mode-hook
          '(lambda ()
             (refill-mode 1)
             (flyspell-mode 1)))

;;; Latex and HTML mode
(dolist (name '(latex html))
  (add-hook (symb name '-mode-hook)
            '(lambda ()
               (refill-mode -1)
               (auto-fill-mode 1))))

;;; CC mode
(autoload 'plunder-mode "plunder"
  "Minor mode for structurally editing C code."
  t)

(add-hook 'c-initialization-hook
          '(lambda ()
             (defkeys c-mode-base-map
               "C-m" c-context-line-break
               "C-c RET" c-macro-expand)))

(defun new-c-snug-do-while (syntax pos)
  "This function is a modified version of `c-snug-do-while' that
works with macros."
  (save-excursion
    (if (and (eq syntax 'block-close)
             (progn (backward-up-list)
                    (c-forward-sexp -1)
                    (looking-at "\\<do\\>[^_]")))
        '(before)
      '(before after))))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "bsd")
             (c-toggle-auto-newline)
             (plunder-mode +1)
             (add-to-list* 'c-cleanup-list
                           'comment-close-slash
                           'brace-else-brace
                           'brace-elseif-brace)
             (add-to-alist c-hanging-braces-alist
                           (class-open after)
                           (class-close before)
                           (brace-list-close before)
                           (block-close . new-c-snug-do-while))))

;;; C++ mode.
(add-hook 'c++-mode-hook
          '(lambda ()
             (add-to-alist c-offsets-alist (innamespace . 0))))

;;; PHP mode.
(c-add-style "php"
             `("linux" (c-basic-offset . 4)))
(add-hook 'php-mode-hook
          '(lambda ()
             (c-set-style "php")
             (setq flycheck-phpcs-standard "PSR2")
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close '0)))

;;; Interaction Lisp mode.
(add-hook 'lisp-interaction-mode-hook
	  '(lambda ()
	     (defkeys lisp-interaction-mode-map "C-m" eval-print-last-sexp)))

;;; Make SLIME connect to lisp when opening a lisp file.
(add-hook 'slime-mode-hook
	  '(lambda ()
	     (unless (slime-connected-p)
	       (save-excursion (slime)))
             (common-lisp-set-style 'modern)
             (defkeys slime-mode-map "C-c M-i" slime-inspect-definition)))

;;; SLIME repl mode
(add-hook 'slime-repl-mode-hook
	  '(lambda ()
	     (defkeys slime-repl-mode-map
	       "C-c s" slime-repl-next-matching-input
	       "C-c r" slime-repl-previous-matching-input
               "C-c M-i" slime-inspect-definition)))

;;; MIXAL mode
(require 'mixvm)

(add-hook 'mixal-mode-hook
          '(lambda ()
             (defkeys mixal-mode-map
               "C-h" backward-delete-char-untabify
               "C-m" newline-and-indent
               "C-j" newline
               "H-h o" mixal-describe-operation-code)))

;;; awk mode
(add-hook 'awk-mode-hook
          '(lambda ()
             (add-to-alist c-hanging-braces-alist
                           (defun-open after))))

;;; Octave mode
(autoload 'octave-mode "octave" nil t)
(push '("\\.m$" . octave-mode) auto-mode-alist)
(add-hook 'octave-mode-hook
          '(lambda ()
             (define-key octave-mode-map
               (kbd "<tab>") 'octave-complete-symbol)))

;;; CSS mode.
(require 'rainbow-mode)
(add-hook 'css-mode-hook
          '(lambda ()
             (rainbow-mode)))

;;; Javascript mode.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook '(lambda () (electric-pair-mode)))

;;; Python mode.
(add-hook 'python-mode-hook
          '(lambda ()
             (add-to-list 'company-backends 'company-jedi)
             (electric-pair-mode +1)
             (save-selected-window
               (switch-to-buffer-other-window
                (process-buffer
                 (python-shell-get-or-create-process
                  (python-shell-parse-command)))))))

;;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-markup-indent-offset 2
                   web-mode-code-indent-offset   2)))

;;; Objective-J mode.
(require 'objj-mode)

;;; Go mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook '(lambda ()
                           (electric-pair-mode)
                           (defkeys go-mode-map
                             "C-t" transpose-sexps
                             "C-M-t" transpose-chars
                             "C-f" forward-sexp
                             "C-M-f" forward-char
                             "C-b" backward-sexp
                             "C-M-b" backward-char
                             "C-k" kill-sexp
                             "C-M-k" paredit-kill
                             "C-<backspace>" backward-kill-sexp)
                           (flyspell-prog-mode)))

;;;; Misc
(when (eq window-system 'x)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program *browser-exec-name*)
  ;; start an emacs server
  (server-start))

(setq-default indent-tabs-mode         nil ; use spaces only for indentation
              show-trailing-whitespace t)

(setq default-major-mode     'text-mode
      custom-file            (full-path "emacs-custom.el")
      backup-by-copying      t
      backup-directory-alist `((".*" . ,temporary-file-directory)))
;;; Set the default font.
(set-frame-font "Inconsolata-10" nil t)
(load custom-file)

(require 'keywiz)		    ; game to learn emacs key-bindings

