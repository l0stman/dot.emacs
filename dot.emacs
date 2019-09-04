;;;; -*-Emacs-Lisp-*-

(defvar *emacs-dir* (expand-file-name "~/.emacs.d")
  "Emacs personal root directory.")

(defsubst full-path (file)
  "Expand the FILE name into the root directory."
  (expand-file-name file *emacs-dir*))

(defsubst add-to-list* (lst &rest args)
  "Add to the list LST the elements in ARGS."
  (dolist (fn args) (add-to-list lst fn)))

(add-to-list* 'load-path
	      (full-path "lib")
	      (expand-file-name "~/hacks/slime"))

;;; OS specific parameters.
(pcase system-type
  ('gnu/linux (require 'ubuntu-init))
  ('berkeley-unix (require 'freebsd-init))
  ('windows-nt (require 'windows-init)))

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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Company.
(use-package company
  :config
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip
       ((t (:inherit default :background "#121733"))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)))

;;; Ido.
(require 'ido)
(ido-mode t)

;;;; SLIME configuration for Common Lisp.
(defvar *slime-dir* (file-name-directory (locate-library "slime")))
(defvar *sbcl-core* (full-path "sbcl.core-with-swank"))
(defvar *sbcl-dev-core* (full-path "sbcl-devel.core-with-swank"))
(defvar *fasls-dir* (concat (file-name-as-directory *tmp-dir*) "fasls-dir"))

(make-directory *fasls-dir* t)

(defsubst sbcl-implementations ()
  "Return the list of SBCL implementations."
  (unless (eq system-type 'windows-nt)
    `((sbcl (,*sbcl-exec* "--noinform" "--core" ,*sbcl-core*)
            :init (lambda (port-file enc)
                    (format "(swank:start-server %S)\n" port-file))
            :env ((concat "SBCL_HOME=" *sbcl-home*)))
      (sbcl-devel (,(expand-file-name "~/bin/sbcl") "--noinform" "--core"
                   ,*sbcl-dev-core*)
                  :init (lambda (port-file enc)
                          (format "(swank:start-server %S)\n" port-file))
                  :env (,(concat "SBCL_HOME="
                                 (expand-file-name "~/lib/sbcl")))))))

(add-to-list* 'load-path
              *slime-dir*
              (expand-file-name "contrib" *slime-dir*))

(use-package slime-company)
(use-package slime
  :hook (slime-mode . (lambda ()
                        ;; Make SLIME connect to lisp when opening a lisp file.
	                (unless (slime-connected-p)
	                  (save-excursion (slime)))
                        (common-lisp-set-style 'modern)))
  
  :bind (:map slime-mode-map
         ("C-c M-i" . slime-inspect-definition)
         :map slime-repl-mode-map
         ("C-c s" . slime-repl-next-matching-input)
	 ("C-c r" . slime-repl-previous-matching-input)
         ("C-c M-i" . slime-inspect-definition))
  :init (require 'slime-autoloads)
  :config
  (slime-setup '(slime-repl slime-autodoc slime-fuzzy slime-fancy-inspector
                            slime-indentation slime-presentations
                            slime-fancy slime-company))
  (setq slime-net-coding-system        'utf-8-unix
        common-lisp-hyperspec-root     *hyperspec-dir*
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-compile-file-options     `(:fasl-directory ,*fasls-dir*)
        slime-lisp-implementations
        `(,@(sbcl-implementations)
          (ccl (,*ccl-exec* "--terminal-encoding" "utf-8")))))

;;; Flycheck.
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (unless (eq system-type 'windows-nt)
    (use-package flycheck-clang-analyzer
      :config
      (flycheck-clang-analyzer-setup))))

;;;; For editing scheme code.
(use-package quack)

;;;; Paredit
(use-package paredit
  :commands enable-paredit-mode
  :init (require 'lisp-indent)
  :bind (:map paredit-mode-map
              ("H-)" . paredit-lisp-indent-assign)))

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

;;; Yaml
(use-package yaml-mode
  :mode (("\\.yaml$" . yaml-mode) ("\\.yml$" . yaml-mode)))

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
(use-package octave
  :mode ("\\.m$" . octave-mode)
  :bind (:map octave-mode-map ("<tab>" . octave-complete-symbol)))

;;; CSS mode.
(use-package rainbow-mode
  :hook css-mode)

;;; Javascript mode.
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . electric-pair-mode))

;;; Python mode.
(use-package python
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends
                                      '(company-jedi company-files))
                         (py-autopep8-enable-on-save)
                         (electric-pair-mode +1)
                         (save-selected-window
                           (switch-to-buffer-other-window
                            (process-buffer
                             (python-shell-get-or-create-process
                              (python-shell-parse-command)))))))
  :init
  (use-package py-autopep8
    :config (setq py-autopep8-options '("-aa")))
  (use-package company-jedi
    :config
    (setq jedi:setup-keys      t
          jedi:complete-on-dot t)
    (jedi:setup)))

;;; Web mode
(use-package web-mode
  :mode ("\\.twig\\'" "\\.jinja\\'")
  :hook (web-mode . (lambda ()
                      (setq web-mode-markup-indent-offset 2
                            web-mode-code-indent-offset   2))))

;;; Objective-J mode.
(require 'objj-mode)

;;; Go mode.
(require 'company-go)
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
(when window-system
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

(set-default-coding-systems 'utf-8)

;;; Set the default font.
(set-frame-font "Inconsolata-10" nil t)
(load custom-file)

(require 'keywiz)		    ; game to learn emacs key-bindings

