
;;; .emacs --- Max  Bozzi's GNU Emacs init file.
;;;
;;; Copyright (C) 2013-2015 Max Bozzi
;;; Version 2.1
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Just copy this file to the location of your Emacs initialization
;;; file.  When loaded, this file will prompt you to download (or
;;; attempt to download) all the referenced packages from Marmalade
;;; or ELPA, respectively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;
;;; Enable MELPA and GNU/ELPA repositories.
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

(eval-when-compile (require 'cl-lib))

;;; Put custom variables into another file
(defvar *init/customization-file-path*
  (expand-file-name "~/.emacs.d/emacs-auto-customizations.el"))
(when (file-exists-p *init/customization-file-path*)
  (load *init/customization-file-path*))

(if (file-writable-p *init/customization-file-path*)
    (setq custom-file *init/customization-file-path*)
  (warn "Insufficient permissions to write the
automatic customization file at %s" *init/customization-file-path*))

;;; Helper functions
(defun feature-available-p (feature)
  "Return t if `FEATURE' is available."
  (or (featurep feature)
      (package-installed-p feature)))

(defun prompt-fetch-feature (feature)
"Ask the user to install FEATURE from enabled repositories.
If the user complies, try to download and install.  Return t
if the package was successfully obtained, otherwise nil."
  (if (yes-or-no-p
       (format "Attempt to fetch `%S' from repositories? "
	       feature))
      (package-install feature))
  (feature-available-p feature))

(defun init/try-retrieve-and-load (feature)
  "Load FEATURE if it's available.  If it isn't, try to obtain it.
Returns t if the feature is either already present, or if it was
succesfully obtained from any of the package repositories.  If the
feature couldn't be loaded, return nil.

The following sources will be checked, in order:
  already-installed packages and
  all sources in `package-archives'."
  (if (or (feature-available-p feature)
	  (with-demoted-errors "Fetch failed: %s"
	    (prompt-fetch-feature feature)))
      feature
    (warn "Couldn't `retrieve-and-load' the package `%S'."
	  feature)))


(defmacro init/when-package-available (packages &rest body)
  "When all of PACKAGES is available to be loaded, evaluate BODY."
  `(when (cl-every #'init/try-retrieve-and-load (quote ,packages))
     ,@body))

(defmacro bind-mode-command-to-key (hook command keybind)
  "Via HOOK Bind the function COMMAND to the keys KEYBIND.

Inserts a newly-minted function with a descriptive name into the
hook.

Returns the value of the `add-hook' expression."
  `(add-hook
    ',hook
    (defun ,(intern
             (concat "bind-" (symbol-name command)))
      nil
      ,(format "Thihcs is an automatically-generated function.
Bind `%s' to keyboard %s via local-set-key."
               (symbol-name command) keybind)
      (local-unset-key (kbd ,keybind))
      (local-set-key (kbd ,keybind) (function ,command)))))

(defun bind-mode-commands (mode-hooks command-keys)
  "Bind all of the modes in MODE-HOOKS matching COMMAND-KEYS."
  (cl-loop for mode in mode-hooks do
           (cl-loop for (key command) in command-keys do
		    (if (or (not (stringp key))
			    (not (commandp command)))
			(error "Invalid command specifier"))
                    (eval
                     `(bind-mode-command-to-key ,mode ,command ,key)))))

(defun init/global-bind (key command)
  "Bind the keypresses stored in KEY to COMMAND, globally."
  (global-set-key (kbd key) command))

(defun global-bind-keys (key-commands)
  "Bind any number of commands to their associated keys.
KEY-COMMANDS is an alist matching key specifications as passed to
`kbd' with an associated command."
  (cl-loop for (key command) in key-commands do
           (init/global-bind key command)))

(defmacro defbind (name arglist key-specs &rest body)
  "Define a command NAME with arguments ARGLIST, bound to KEY-SPECS.
The function accepts arguments as defined in ARGLIST.  BODY must
contain a `interactive' subform.

  KEY-SPECS is a list composed of a list of keybindings in the
normal notation for keys in the form as would be passed to `kbd',
and, if included, a list of hooks to attach, as in the following:
\(\(key-notation...\) [\'(hook-symbol...\)]\)

  A missing hook-symbol list will result in the new method being
bound globally to key-notation(s).

  This macro invocation looks similar to `defun':

\(defbind do-clever-things \(n-times some-other-argument\)
  \(\(\"\\[forward-char]\"\) \(emacs-lisp-mode-hook\)\)
  \(interactive \"p\"\)
  \(save-excursion
    \(do-foo n-times\)
    \(do-bar n-times some-other-argument\)\)\)"
  `(progn
     (defun ,name ,arglist ,@body)
     ,(if (cdr key-specs)
          `(bind-mode-commands
	    ,(cadr key-specs)
	    (quote ,(mapcar (lambda (key)
			      (list key name))
			    (car key-specs))))
        `(global-bind-keys
          (mapcar (lambda (key)
		    (list key (quote ,name)))
		  (quote ,(car key-specs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks
;;;
(defvar lisp-mode-common-hooks '(lisp-mode-hook
                                 emacs-lisp-mode-hook
                                 lisp-interaction-mode-hook
                                 ielm-mode-hook
                                 slime-mode-hook
				 scheme-mode-hook))

(defvar c-c++-common-hooks '(c++-mode-hook
                             c-mode-hook))

(defvar dired-common-hooks '(dired-mode-hook
                             wdired-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Color themes, font, and appearance code
;;;
(with-demoted-errors "Couldn't load font: %S"
  (set-frame-font "clean"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defun disable-all-themes nil
  "Turn off all enabled color themes, leaving the default only."
  (mapc (lambda (theme)
          (disable-theme theme))
        custom-enabled-themes))

(defun switch-theme-exclusive (&optional new-theme)
  "Enable solely NEW-THEME, disabling anything already loaded.
returns t if NEW-THEME was loaded, nil otherwise."
  (if (custom-theme-name-valid-p new-theme)
      (progn
        (disable-all-themes)
        (load-theme new-theme 'dont-confirm))
    (warn "Couldn't load the theme named `%S'" new-theme)))

(defvar *init/my-theme* 'jazz)
(switch-theme-exclusive *init/my-theme*)

;;; Pretty frame transparency under X.  Perhaps there is a way to get
;;; emacs to draw transparently under the terminal as well (i.e.,
;;; launched with `-nw').  But I rarely use it like that.
(defun init/clamp-integer-to-closed-interval (integer lower upper)
  "Restrict INTEGER to the closed-interval [LOWER, UPPER].
If INTEGER is not in [LOWER, UPPER], clamp it to the closest of
the two.  It is implied that LOWER < UPPER."
  (cond ((> integer upper) upper)
        ((< integer lower) lower)
        (t                 integer)))

(defun set-frame-alpha-percent (new-alpha-value)
  "Set the selected-frame's alpha value to NEW-ALPHA-VALUE."
  (set-frame-parameter (selected-frame)
                       'alpha
                       (list new-alpha-value 100)))

(defun increase-frame-transparency (difference)
  "Increase the frame's visual transparency by DIFFERENCE percent.
If called with a negative argument, make the frame less transparent."
  (interactive "P")
  (let ((current-alpha (car (frame-parameter nil 'alpha))))
    (if current-alpha
        (set-frame-alpha-percent
         (init/clamp-integer-to-closed-interval
          (- current-alpha difference) 0 100))
      (init/clamp-integer-to-closed-interval
       (- difference 100) 0 100))))

(set-frame-alpha-percent 74)

;; Start-up quotes
(defun init/get-initial-scratch-message-list nil
  "Get my list of messages from the appropriate file, if it exists.
Return a list of strings, otherwise nil.

For the sake of simplicity, the format of the message-file is about
as easy as possible: At least two blank lines separate quotes.
There are no comments.  There are no metacharacters.  Sorry!

Maybe I'll make this better eventually."
  (let ((message-file "~/.emacs.d/startupquotes.txt"))
    (when (file-readable-p (expand-file-name message-file))
      (with-temp-buffer
        (insert-file-contents message-file)
        (split-string
         (buffer-string)
         "\n[[:space:]]*\n[[:space:]]*\n" t)))))

(setq initial-scratch-message
      (concat
       (let ((quote-list (init/get-initial-scratch-message-list)))
	 (if quote-list
	     (elt (mapcar
		   (lambda (str)
		     (format "%s%s\n\n" ";; "
			     (replace-regexp-in-string "\n" "\n;; "
						       str)))
		   quote-list)
		  (random
		   (length quote-list)))
	   ";; You suck.  This is the default message.  Copy down some \
quotes, please!\n"))
       "(progn (text-mode) (erase-buffer))\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-default mode settings
;;;

;; Icicles:
(init/when-package-available (icicles)
  (icy-mode))

;; Smart mode-line
(when nil
  (init/when-package-available (smart-mode-line)
    (declare-function sml/setup "smart-mode-line.el")
    (declare-function sml/apply-theme "smart-mode-line.el")

    (sml/setup)
    (sml/apply-theme 'dark)

    (defvar sml/replacer-regexp-list)
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/devel/cpp/at-nighttime/" ":AN:")
                 'append)
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/devel/cpp/" ":CPP:")
                 'append)
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/devel/" ":DEV:")
                 'append)
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/devel/lisp" ":LISP:")
                 'append)
  
    (smart-mode-line-enable)))
 
;; Drew Adam's Pretty-lambda
(init/when-package-available (pretty-lambdada)
  (pretty-lambda-for-modes))

;; Paredit
(init/when-package-available (paredit)
  (autoload 'paredit-mode "paredit" "enable paredit in lisp")
  (mapc (lambda (hook)
	  (add-hook
	   hook #'enable-paredit-mode))
        lisp-mode-common-hooks)
  (enable-paredit-mode))

;; Slime
(defvar *my-common-lisp-interpreter* "/usr/bin/sbcl"
  "SLIME's default Lisp.")
(init/when-package-available (slime)
  (if (not (file-executable-p *my-common-lisp-interpreter*))
      (warn "Can't run %S." *my-common-lisp-interpreter*)
    (progn
      (require 'slime-autoloads)
      
      (defvar slime-contribs)
      (defvar slime-lisp-implementations)
      (defvar slime-default-lisp)
      
      (setq slime-contribs '(slime-fancy))
      (setq slime-lisp-implementations
            `((sbcl (,*my-common-lisp-interpreter*))))
      (setq slime-default-lisp 'sbcl)
  
      (defvar inferior-lisp-program)
      (setq inferior-lisp-program *my-common-lisp-interpreter*)

      (let ((quicklisp (expand-file-name "~/quicklisp/slime-helper.el")))
        (if (file-readable-p quicklisp)
            (load quicklisp)
          (warn "Can't find QuickLisp at %S." quicklisp)))
      
      (bind-mode-commands 'slime-mode-hook
        '(("C-h f" slime-documentation)
          ("C-h M-f" describe-function)
          ("C-h v" slime-describe-symbol)
          ("C-h M-v" describe-variable))))))

;; EDE
(when nil
  (init/when-package-available (ede)
    (global-ede-mode)))

;; Auto-Complete
(init/when-package-available (auto-complete)
  (declare-function global-auto-complete-mode "auto-complete.el")
  (declare-function ac-config-default "auto-complete-config.el")
  (global-auto-complete-mode t)
  (ac-config-default)
  (setq after-save-hook
	(remove 'ac-clear-variables-after-save
		after-save-hook)))
;; VC
(setq vc-handled-backends '(Hg Git))

;; Irony
(init/when-package-available (irony)
  (defvar irony-mode-map)
  (setenv "LD_LIBRARY_PATH" (expand-file-name "/usr/include/c++/4.8.3"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/irony"))
  (add-hook 'c++-mode-hook  'irony-mode)
  (add-hook 'c-mode-hook    'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook
            (defun change-to-async-completion ()
              (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async))))

;; Flycheck
(init/when-package-available (flycheck)
  (declare-function global-flycheck-mode "flycheck.el")
  (global-flycheck-mode)

  (defvar flycheck-clang-language-standard)
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++11")))
  (add-hook 'c-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c11")))

  (defvar flycheck-gcc-language-standard)
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-hook
            (lambda () (setq flycheck-gcc-language-standard "c11")))

  (defvar flycheck-disabled-checkers nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang))
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang))

;; Yasnippet
(init/when-package-available (yasnippet)
  (defvar yas-snippet-dirs)
  (declare-function yas-global-mode "yasnippet.el")
  (when (boundp 'yas-snippet-dirs)
    (add-to-list
     'yas-snippet-dirs
     (expand-file-name "~/.emacs.d/snippets")))
  (yas-global-mode 1))

;; Auctex
(init/when-package-available (auctex reftex)
  (defvar font-latex-fontify-sectioning)
  (setq font-latex-fontify-sectioning 'color)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode))

;; Dired and Wdired
(add-hook 'wdired-mode-hook  #'auto-revert-mode)
(add-hook 'dired-mode-hook   #'auto-revert-mode)
(bind-mode-commands dired-common-hooks
  '(("C-s" dired-isearch-filenames)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings and text-editing
;;;
;; Swap the meanings of C-i and <tab> so that I can rebind C-i.
(define-key function-key-map    [tab] nil)
(define-key key-translation-map [9] [tab])
(define-key key-translation-map [tab] [9])

(mapc #'global-unset-key
      (list (kbd "M-i")
            (kbd "<tab>")
            (kbd "M-o")
            (kbd "C-M-o")))

(let ((terminal-frame-type t))
 (when (equal terminal-frame-type (framep (selected-frame)))
   (global-unset-key (kbd "C-z"))))

(global-bind-keys
 '(("C-'"     ff-find-other-file)
   ("C-c C-p" previous-buffer)
   ("C-c C-n" next-buffer)
   ("C-c p"   previous-buffer)
   ("C-c n"   next-buffer)
   ("<f9>"    previous-buffer)
   ("<f10>"   other-window)
   ("<f11>"   next-buffer)
   ("<f5>"    compile)
   ("C-s"     isearch-forward-regexp)
   ("C-r"     isearch-backward-regexp)
   ("C-c C-s" isearch-forward)
   ("C-c C-r" isearch-backward)
   ("C-x C-d" dired)))

(init/when-package-available (iedit)
  (global-bind-keys
   '(("C-;" iedit-mode))))

(defbind go-to-scratch-buffer nil (("<f6>"))
  "Jump instantly to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defbind insert-std nil (("C-M-;") c-c++-common-hooks)
  "Insert the string `std::' at point."
  (interactive)
  (insert "std::"))

(defbind wrap-round (&optional n-sexps) (("M-(") '(c++-mode-hook))
  "Wrap N-SEXPS sexps surrounding point with parentheses."
  (interactive "p")
  (if (> n-sexps 0)
      (progn
	(insert "(")
	(save-excursion
	  (forward-sexp n-sexps)
	  (insert ")")))
    (save-excursion
      (insert ")")
      (forward-sexp n-sexps) ; n-sexps is negative here.
      (insert "("))))

(defbind remove-pair-or-electric-delete nil (("DEL") '(c-mode-common-hook))
  "Remove both paired characters or just behave normally."
  (interactive)
  (cl-flet ((match-chars (pair-string)
             (and (string-equal (string (preceding-char))
                                (substring pair-string 0 1))
                  (string-equal (string (following-char))
                                (substring pair-string 1 2)))))
    (if (or
         (match-chars "()")
         (match-chars "[]")
         (match-chars "<>")
         (match-chars "{}")
         (match-chars "\"\"")
         (match-chars "''"))
        (progn (delete-char -1) (delete-char 1))
      (progn (backward-delete-char-untabify 1)))))

(defbind insert-paired-angle-braces nil (("C-<") '(c++-mode-hook))
  "Insert two angle brackets (<>) and position the cursor between."
  (interactive)
  (insert "<>") (backward-char))

(defbind lisp-insert-lambda nil (("C-M-l") lisp-mode-common-hooks)
  "Insert the `lambda' keyword in the current buffer."
  (interactive)
  (insert "lambda "))

(defbind lisp-end-of-list nil (("C-M-;") lisp-mode-common-hooks)
  "Move point to the end of the current s-expression."
  (interactive)
  (backward-up-list)
  (forward-sexp)
  (backward-char))

(defbind lisp-down-and-end-of-list nil (("C-M-S-d") lisp-mode-common-hooks)
  "Move point forward, down and to the end of the current s-expression."
  (interactive)
  (down-list)
  (lisp-end-of-list))

(defbind lower-current-line (n-times) (("<tab>"))
  "Lower the current line to the one N-TIMES below, adding whitespace."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (newline n-times)))

(defbind raise-current-line (n-times) (("M-i"))
  "Raise the current-line N-TIMES lines up, and indent."
  (interactive "p")
  (save-excursion
   (cl-loop repeat n-times do
	    (delete-indentation)
	    (indent-for-tab-command))))

(defbind raise-next-line (n-times) (("M-o"))
  "Raise the next line to the one N-TIMES above, folding whitespace."
  (interactive "p")
  (save-excursion
    (cl-loop repeat n-times do
	     (forward-line)
	     (raise-current-line 1)
	     (delete-trailing-whitespace
	      (point)
	      (line-end-position))))
  (indent-for-tab-command))

(defbind lower-next-line (n-times) (("C-M-o"))
  "Lower the next line N-TIMES lines below, entering newlines."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line n-times)))

(defbind consume-sexp-from-inside nil (("C-S-k"))
  "Brutal.  Kill the entire sexp point is inside or before."
  (interactive)
  (mark-sexp)
  (exchange-point-and-mark)
  (set-mark (point))
  (forward-sexp (- 1))
  (kill-region (point) (mark)))

(bind-mode-command-to-key dired-mode-hook
                          wdired-change-to-wdired-mode "C-c C-w")

(declare-function wdired-exit "wdired.el")
(bind-mode-command-to-key wdired-mode-hook
                          wdired-exit "C-c C-w")

(init/when-package-available (paredit)
  (bind-mode-commands lisp-mode-common-hooks
    '(("<C-backspace>" paredit-backward-kill-word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Builtin settings:
;;;
(auto-fill-mode        t)
(set-fill-column       72)
(global-hl-line-mode   t)
(delete-selection-mode t)
(display-time-mode     t)
(show-paren-mode       t)
(fringe-mode           (cons 2 2))
(electric-pair-mode)
(column-number-mode)
(auto-fill-mode)
(electric-indent-mode  t)
(tool-bar-mode         (- 1))
(menu-bar-mode         (- 1))
(scroll-bar-mode       (- 1))
(semantic-mode t)

(declare-function global-semantic-idle-scheduler-mode "semantic/idle.el")
(global-semantic-idle-scheduler-mode t)

(defvar select-enable-clipboard t)
(setq inhibit-splash-screen    		      t
      tab-width                		      2
      indent-tabs-mode         		      nil
      user-full-name           		      "Max Bozzi"
      user-mail-address        		      "maxwelljb22@gmail.com"
      x-select-enable-clipboard-manager t
      select-enable-clipboard           t
      kept-new-versions                 4
      kept-old-versions                 2
      echo-keystrokes                   0.001
      use-dialog-box                    nil
      visible-bell                      t
      version-control                   t
      disabled-command-function         nil
      backup-by-copying                 t
      delete-old-versions               t)

(unless indicate-empty-lines
  (toggle-indicate-empty-lines))

(defvar ff-always-in-other-window t
  "Find file/other file in new windows.")

;; Backups
(let ((backup-directory (expand-file-name "~/.emacs.d/saves")))
  (if (file-writable-p backup-directory)
      (setq backup-directory-alist (list (cons "." backup-directory)))
    (warn "Backup directory %S is not writable.  Backups will not be
made." 'backup-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros.  I need a way to generate and save these symbol properties.
;;;   I'm not so sure about how to go about this.
;;;
(defvar symbol-indent-info-file
  (expand-file-name "~/.emacs.d/lisp-sym-indent-info.el"))

(defun generate-put-indent-form (symbol indent-form) 
  `(put ',symbol 'lisp-indent-function ',indent-form))

(defun specially-indent-form nil
  "Add an entry in `symbol-indent-info-file' specifying how to
indent a certain lisp form.  

The point of this interface is to keep the lists of indent
declarations for macros and such out of the init file, and to
allow new entries to be added on the fly. "
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (propertize-form
         (generate-put-indent-form (prompt-for-symbol)
                                   (prompt-for-indent-form))))
    (if (not (file-exists-p symbol-indent-info-file))
	(add-sym-info-header))
    (write-region
     (format "%S\n" propertize-form)
     nil symbol-indent-info-file 'append)
    ;; And add the property immediately, too:
    (eval propertize-form)))

(defun prompt-for-symbol nil
  "Prompt the user for a valid symbol."
  (interactive)
  (let* ((sym (or (symbol-at-point)
                  (function-called-at-point))))
    (make-symbol
     (completing-read (if sym
                          (format "Symbol (default %s): " sym)
                        "Symbol: ")
                      obarray #'symbolp 'confirm nil nil
                      (and sym (symbol-name sym))))))

(defun prompt-for-indent-form nil
  "Prompt the user for a valid form usable as a property
specifier for specially indenting a function call."
  (interactive)
  (let* ((string
	  (completing-read (format "Indent: ")
			   '("1" "2" "3" "4" "5"
			     "6" "7" "8" "9" "defun")
			   nil 'confirm))
	 (string-num (string-to-number string))
	 (string-sym (make-symbol      string))
	 (val
	  (or (if (and (integerp string-num)
		       (plusp string-num))    string-num)
	      (if (string= "defun" string)    'defun)
	      (if (functionp string-sym)      string-sym))))
    (if val
	val
      (error "Incorrect specifier.  See `lisp-indent-function'"))))

(defun add-sym-info-header nil
  "If `symbol-indent-info-file' does not exist, then try to write
the file-header, creating the file and specifying versions."
  (write-region
   (format
    (concat ";;;; This is a file generated by calls to"
	    " `specially-indent-form'.\n;;;; Be carefu"
	    "l editing it by hand, contents you add mi"
	    "ght be mangled.\n\n'(version . 1)\n\n"))
   nil symbol-indent-info-file))

(defun remove-duplicate-indent-info nil
  (let ((forms nil)
	(syms-to-forms (make-hash-table :test 'equal)))
    (ignore-errors
      (with-temp-file symbol-indent-info-file
	(insert-file-contents symbol-indent-info-file)
	(cl-loop do (push (read (current-buffer)) forms))))
    (let ((file-contents (nreverse forms)))
      (if (not (equal (car file-contents)
		      '(quote (version . 1)))) 
	  (error "Version mismatch"))
      (mapc (lambda (form)
	      (puthash (cadadr form) form syms-to-forms))
	    (cdr file-contents)) ; exclude the version tag!
      (add-sym-info-header)
      (maphash (lambda (key value)
		 (declare (ignore key))
		 (write-region
		  (format "%S\n" value)
		  nil
		  symbol-indent-info-file
		  'append))
	       syms-to-forms))))
(remove-duplicate-indent-info)

(add-hook 'lisp-mode-hook
	  (lambda nil
	    (set (make-local-variable 'lisp-indent-function)
           'common-lisp-indent-function)))

(put 'alambda 'lisp-indent-function 1)
(put 'with-gensyms 'lisp-indent-function 1)
(put 'bind-mode-commands 'lisp-indent-function 1)
(put 'init/when-package-available 'lisp-indent-function 1)
(put 'defmacro! 'lisp-indent-function 1)

;;  Missing C++ Keywords introduced in C++11:
;;  override, final, alignas, alignof, char16_t, char32_t,
;;  concept, constexpr, decltype, noexcept, nullptr, static_assert,
;;  and thread_local.
(font-lock-add-keywords
 'c++-mode
 `((,(regexp-opt '("override" "final" "alignas" "alignof" "char16_t"
                   "char32_t" "concept" "constexpr" "decltype"
                   "noexcept" "nullptr" "static_assert"
                   "thread_local")
                 'words) . font-lock-keyword-face)))

(provide '.emacs)
;;; .emacs ends here
