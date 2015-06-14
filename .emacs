;;; .emacs --- Summary:
;;  Max Bozzi's LINUX GNU Emacs init file.

;;  This file needs some serious help.

;;; Commentary:
;;
;;  My original setup used a collection of files which I'd drop into
;;  .emacs.d.  I still think that that was a decent idea; else this file
;;  might get a bit long.  Of course, though, it mitigates the problems
;;  inherent in versioning source code, and hopefully helps to reduce
;;  the impact of certain... `quirks' that I've encountered, and the
;;  number of resultant hacks.
;;
;;  I have tried to write this file such that it doesn't result in an
;;  error when this initialization file is used on a system which
;;  doesn't have access to one of the packages which I require in this
;;  file.
;;
;;  Mostly that's done by wrapping bodies of code into a predicate
;;  which tests to see whether or not a required package is available,
;;  and tries to get it if it isn't present already.

;;; Code:
;;

(eval-when-compile (require 'cl-lib))
;; Should be necessary for macros only.

(defmacro when-found (package &rest body)
  "If and only if PACKAGE is found and available, execute BODY."
  `(when (retrieve-and-load ,package) ,@body))

;;  Set custom-variables in another file.  Load them immediately.
(let ((customization-file-path
       (expand-file-name
	"~/.emacs.d/emacs-auto-customizations.el")))
  (when (file-writable-p customization-file-path)
    (setq custom-file customization-file-path)
    (when (file-exists-p customization-file-path)
      (load customization-file-path))))

;;  Enable all commands.  I ain't no newbie.
(setq disabled-command-function nil)

;;; Package auto-retrieval
;;  I want to be able to get code (packages) without having to do
;;  anything special; i.e., just unpack this file (and emacs, OFC) and
;;  run Emacs.

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

(defun get-from-cloud (package)
  "Dummy function stub.
This might end up dragging in far more code than might be
practical ATM.  While I have the code for it already written as
`edul', it may or may not be worth the trouble.

The idea is to fetch a package named PACKAGE from the cloud.

I can predicate including this method in `retrieve-and-load' on
the presence of the library with some clever hacking.  That way I
can separate the code required here from elsewhere.

In fact, I'm pretty sure it actually isn't."
  nil)

(defun retrieve-and-load (feature)
  "Load FEATURE if it's available.  If it isn't, try to obtain it.
Returns t if the feature is either already present, or if it was
succesfully obtained from any of the package repositories.  If the
feature couldn't be loaded, return nil.

The following sources will be checked, in order:
  already-installed packages,
  all sources in `package-archives', and
  my personal cloud storage, if it's enabled."
  (let ((result (or (package-installed-p feature)
                    (featurep feature)
                    (ignore-errors
                      (package-install feature)) ; Check the archives.
                    (get-from-cloud feature))))
    (when (null result)
      (warn "Was unable to `retrieve-and-load' the package `%S'.\n"
	    feature))
    result))

;;; Appearance
;;  Show me the column number.
(column-number-mode)

;;  Themes
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
    (error "Couldn't load the theme named `%S'" new-theme)))

;;  Load my theme by itself.
;;  But only when we have graphics support.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(let ((my-theme 'jazz))              ; <-- There it is!
  (if (display-graphic-p)
      (unless (ignore-errors
                (switch-theme-exclusive my-theme))
        (disable-all-themes)))

  ;;  Enable smart-mode-line prettiness:
  (when-found 'smart-mode-line
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
			   'append)))

;;  Initial scratch messages:
;;  Generate a random initial "meaningful message" from the list.
;;  If it's not there, annoy the shit out of me about it!
(defun get-initial-scratch-message-list nil
  "Get my list of messages from the appropriate file, if it exists.
Return a list of strings, otherwise nil.

For the sake of simplicity, the format of the message-file is about
as easy as possible: At least two blank lines separate quotes.
There are no comments.  There are no metacharacters.  Sorry!

Maybe I'll make this better eventually, but who cares.  Don't fix
what ain't broke."
  (let ((message-file "~/.emacs.d/startupquotes.txt"))
    (when (file-readable-p (expand-file-name message-file))
      (with-temp-buffer
        (insert-file-contents message-file)
        (split-string
         (buffer-string)
         "\n[[:space:]]*\n[[:space:]]*\n" t)))))

(setq initial-scratch-message
      (let ((quote-list (get-initial-scratch-message-list)))
        (if quote-list
            (elt
             (mapcar
              (lambda (str) 		; Comment the quote now.
                (format "%s%s\n\n" ";; "
                        (replace-regexp-in-string "\n" "\n;; " str)))
              quote-list)
             (random
              (length quote-list)))
          ";; You suck.  This is the default message.  Copy down some \
quotes, please!\n")))

;;  Miscellany
;;  No more splash screen!
(setq inhibit-splash-screen t)

;;  Show me the end of my file -- empty line indicators, always:
(unless indicate-empty-lines
  (toggle-indicate-empty-lines))

(setq-default indicate-empty-lines t)

;;  Small tabs for small laptop screens.
(setq tab-width 2)
(setq indent-tabs-mode nil)

;;  Pretty frame transparency under X.  Perhaps there is a way to get
;;  emacs to draw transparently under the terminal as well (i.e.,
;;  launched with `-nw').  But I rarely use it like that.
(defun clamp-integer-to-closed-interval (integer lower upper)
  "Restrict INTEGER to the range [LOWER, UPPER].
If INTEGER is not in [LOWER, UPPER], clamp it to the closest of
the two.  It is implied that LOWER < UPPER."
  (cond ((> integer upper) upper)
        ((< integer lower) lower)
        (t                 integer)))

(defun set-frame-alpha-percent (new-alpha-value)
  "Set the selected-frame's alpha value to NEW-ALPHA-VALUE."
  (set-frame-parameter (selected-frame)
                       'alpha
                       `(,new-alpha-value 100)))

(defun increase-frame-transparency (difference)
  "Increase the frame's visual transparency by DIFFERENCE percent.
If called with a negative argument, make the frame less transparent."
  (interactive "P")
  (let ((current-alpha (car (frame-parameter nil 'alpha))))
    (if current-alpha
        (set-frame-alpha-percent
         (clamp-integer-to-closed-interval
          (- current-alpha difference) 0 100))
      (clamp-integer-to-closed-interval
       (- difference 100) 0 100))))

(set-frame-alpha-percent 74)

;;; Override Keybindings
;;  I've had continual trouble with getting this to work...  but I want
;;  to remap navigational keybinds to C-j, C-l, C-i, and C-k, home-row,
;;  left-handed.
;;
;;  So I thought I could define a minor mode and push it to the head of
;;  the list...
;;  It worked, most of the time.  I don't know why.  Perhaps
;;  centralizing it will fix the issue.
(defconst enable-override-keybindings-initally nil
  "If nonnil, enable the override keybindings on startup.
Otherwise, leave them off until explicitly toggled on.")

(defvar override-keys-minor-mode-map
  (make-keymap) "Override-keys-minor-mode keymap.")

(define-minor-mode override-keys-minor-mode
  "The intent is that this keymap is loaded with a higher precedence
than any others, making sure that I get my navigational keys where I
want them.

More generally, this map defines navigational keys on the left-hand
home-row, as opposed to the default C-f, C-b, C-n, C-p."
  enable-override-keybindings-initally
  " ovrds" 'override-keys-minor-mode-map)

(defun force-override-keybindings nil
  "Make the override-keys-minor-mode bindings take precedence.

This is done by pushing `override-keys-minor-mode-map' to the
front of the `minor-mode-map-alist'.  This gives it the highest
precedence over other modes when it's inserted."
  (unless (equal
           (caar minor-mode-map-alist)
           'override-keys-minor-mode)
    (let ((mykeys (assq
                   'override-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all
       'override-keys-minor-mode minor-mode-map-alist)
      (add-to-list
       'minor-mode-map-alist mykeys))))

(when override-keys-minor-mode
  (defadvice load (activation
                   global-key-priority)
    "If the mode is enabled, make sure it isn't shadowed by any
  libraries loaded later on."
    (force-override-keybindings))       ; Lisp just impressed me.
  (defadvice override-keys-minor-mode (after
				       global-key-priority)
    "If the user enables the mode later, force it's precedence
    over other keybindings.")
  (ad-activate 'load t)                 ; Activate the advice.
  (ad-activate 'override-keys-minor-mode t))

(defun override-set-key (key-combo function)
  "Bind KEY-COMBO to FUNCTION in the override-keys-minor-mode."
  (define-key override-keys-minor-mode-map key-combo function))

(override-set-key (kbd "C-j") 'backward-word)
(override-set-key (kbd "M-j") 'backward-char)

(override-set-key (kbd "C-l") 'forward-word)
(override-set-key (kbd "M-l") 'forward-char)

(override-set-key (kbd "C-k") 'next-line)
(override-set-key (kbd "M-k") 'forward-paragraph)

;;  Apparently this hack is necessary for binding C-i (is terminal TAB.)
(define-key function-key-map [tab] nil) ; Don't differentiate between
                                        ; C-i, TAB
(define-key key-translation-map [9] [tab]) ; Swap the meanings.
(define-key key-translation-map [tab] [9])

(override-set-key [tab] 'previous-line) ; Bind TAB, which is now C-i.
(override-set-key (kbd "M-i") 'backward-paragraph)

;;; Navigation keybindings:
(defmacro bind-mode-command-to-key (mode-hook command keybind)
  "Via MODE-HOOK Bind the function COMMAND to the keys KEYBIND.

Inserts a newly-minted function with a descriptive name into the
hook.

Returns the value of the `add-hook' expression."
  `(add-hook
    ',mode-hook
    (defun ,(intern
             (concat "bind-" (symbol-name command)))
      nil
      ,(format "This is an automatically-generated function.
Bind `%s' to keyboard %s via local-set-key."
               (symbol-name command) keybind)
      (local-set-key (kbd ,keybind) (function ,command)))))

(defun bind-mode-commands (mode-hooks command-keys)
  "Bind all of the modes in MODE-HOOKS matching COMMAND-KEYS."
  (cl-loop for mode in mode-hooks do
    (cl-loop for (command key) in command-keys do
             (eval `(bind-mode-command-to-key mode ,command ,key)))))

;;;  Don't forget to check SLIME startup.

(defun global-bind (key command)
  "Bind the keypresses stored in KEY to COMMAND, globally."
  (global-set-key (kbd key) command))

;;  I get tired of typing std:: everywhere; add a shortcut for it.
(defun c++-insert-std nil
  "Insert the string `std::' at point."
  (interactive)
  (insert "std::"))

(defun c++-wrap-round (&optional n-sexps)
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

(defun remove-pair-or-electric-delete nil
  "Remove both paired characters or just behave normally."
  (interactive)
  (if (or
       (match-chars "()")
       (match-chars "[]")
       (match-chars "<>")
       (match-chars "{}")
       (match-chars "\"\"")
       (match-chars "''"))
      (progn (delete-char -1) (delete-char 1))
    (progn (backward-delete-char-untabify 1))))

(defun match-chars (pair-string)
  "Test to see if the two characters in PAIR-STRING are next-to point."
  (interactive)
  (and (string-equal (string (preceding-char))
                     (substring pair-string 0 1))
       (string-equal (string (following-char))
                     (substring pair-string 1 2))))

(defun c++-insert-paired-angle-braces nil
  "Insert two angle brackets (<>) and position the cursor between."
  (interactive)
  (insert "<>") (backward-char))

(defun lisp-insert-lambda nil
  "Insert the `lambda' keyword in the current buffer."
  (interactive)
  (insert "lambda "))

(defun lisp-end-of-sexp nil
  "Move point to the end of the current s-expression."
  (interactive)
  (end-of-sexp))

(defun lisp-down-and-end-of-sexp nil
  "Move point forward, down and to the end of the current s-expression."
  (interactive)
  (down-list)
  (end-of-sexp))

(defun lower-current-line (n-times)
  "Lower the current line to the one N-TIMES below, adding whitespace."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (newline n-times)))

(defun raise-current-line (n-times)
  "Raise the current-line N-TIMES lines up, and indent."
  (interactive "p")
  (cl-loop repeat n-times do
	   (delete-indentation)
	   (indent-for-tab-command)))

(defun raise-next-line (n-times)
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

(defun lower-next-line (n-times)
  "Lower the next line N-TIMES lines below, entering newlines."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line n-times)))

;;  Bind C++ commands:
(bind-mode-commands '(c++-mode-hook)
  '((c++-insert-paired-angle-braces "C-M-<")
    (c++-wrap-round "M-(")
    (remove-pair-or-electric-delete "DEL")
    (c++-insert-std "C-M-;")))

;;  Compile!
(global-bind "<f5>" #'compile)

;;  Regular expression searching by default!
(global-bind "C-s" #'isearch-forward-regexp)
(global-bind "C-r" #'isearch-backward-regexp)
(global-bind "C-c C-s" #'isearch-forward)
(global-bind "C-c C-r" #'isearch-backward)

;;  No quick-list directory --- dired instead.
(global-bind "C-x C-d" #'dired)

;;  buffer-movement:
(global-bind "C-'"     #'ff-find-other-file)
(global-bind "C-c C-p" #'previous-buffer)
(global-bind "C-c C-n" #'next-buffer)
(global-bind "C-c p"   #'previous-buffer)
(global-bind "C-c n"   #'next-buffer)

(global-bind "<f9>"  #'previous-buffer)
(global-bind "<f10>" #'other-window)
(global-bind "<f11>" #'next-buffer)

(mapc #'global-unset-key
      (list (kbd "M-i")
	    (kbd "<tab>")
	    (kbd "C-o")
	    (kbd "M-o")
	    (kbd "C-M-o")))

(global-bind "M-i"   #'raise-current-line)
(global-bind "<tab>"   #'lower-current-line)	; <tab> is C-i

(global-bind "M-o"   #'raise-next-line)
(global-bind "C-o"   #'open-line)
(global-bind "C-M-o" #'lower-next-line)

(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-z"))

(defvar *lisp-mode-common-hooks* '(lisp-mode-hook
                                   emacs-lisp-mode-hook
                                   lisp-interaction-mode-hook
                                   ielm-mode-hook
                                   slime-mode-hook)
  "A collection of hooks for Lisp modes.")
;; I do this much more than I transpose words.  I don't transpose that
;; much anyways.
(bind-mode-commands *lisp-mode-common-hooks*
  '((transpose-sexps "M-t")
    (lisp-insert-lambda "C-M-l")))


(mapc (lambda (mode-symbol)
 	(font-lock-add-keywords
 	 mode-symbol
 	 '(("[,`'.]\\|#'" . font-lock-keyword-face))))
      '(lisp-mode lisp-interaction-mode emacs-lisp-mode ielm-mode))

;;  toggle dired and wdired.
(bind-mode-command-to-key dired-mode-hook
			  wdired-change-to-wdired-mode
			  "C-c C-w")
(bind-mode-command-to-key wdired-mode-hook wdired-exit "C-c C-w")

(defun consume-sexp-from-inside nil
  "Brutal.  Kill the entire sexp point is inside or before."
  (interactive)
  (mark-sexp)
  (exchange-point-and-mark)
  (set-mark (point))
  (forward-sexp (- 1))
  (kill-region (point) (mark)))

(global-bind "C-S-k" #'consume-sexp-from-inside)
(global-bind "C-;" #'iedit-mode)

;;  Keep updating the directory listing in the case of an external
;;  change.

(add-hook 'wdired-mode-hook #'auto-revert-mode)

;;; Minor mode setups.
;;; User ID information
(setq user-full-name "Max Bozzi")
(setq user-mail-address "maxwelljb22@gmail.com")

;;; Configure the UI properly.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(electric-pair-mode)
(electric-indent-mode t)

(display-time-mode t)

(delete-selection-mode t)

(show-paren-mode t)

(ido-mode t)
(defvar ido-enable-flex-matching)
(setq ido-enable-flex-matching t)

(global-hl-line-mode t)

(set-fill-column 72)
(auto-fill-mode   t)

(when (retrieve-and-load 'pretty-lambdada)
  (pretty-lambda-for-modes)) 

(defvar ff-always-in-other-window t
  "Find-file/find-other-file should create a new window.")

;;; Produce PDFs when using aucTeX, parse it and save automatically.
(when (and (retrieve-and-load 'auctex)
           (retrieve-and-load 'reftex))
  (defvar font-latex-fontify-sectioning)
  (setq font-latex-fontify-sectioning 'color)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode))

;;; Development setup.  All sorts of bells-and-whistles.

;;  Yasnippet setup.
(when (retrieve-and-load 'yasnippet)
  (defvar yas-snippet-dirs)
  (declare-function yas-global-mode "yasnippet.el")
  (when (boundp 'yas-snippet-dirs)
    (add-to-list
     'yas-snippet-dirs
     (expand-file-name "~/.emacs.d/snippets")))
  (yas-global-mode 1))

;;  This is auto-completion stuff...
;;  Slime mode (cl REPL in emacs)

(defvar *my-common-lisp-interpreter* "/usr/bin/sbcl"
  "SLIME's default Lisp.")

(when (and (retrieve-and-load 'slime)
	   (file-readable-p *my-common-lisp-interpreter*))
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))

  (defvar slime-lisp-implementations)
  (defvar slime-default-lisp)
  (setq slime-lisp-implementations
        `((sbcl (,*my-common-lisp-interpreter*))))
  (setq slime-default-lisp 'sbcl)
  
  (defvar inferior-lisp-program)
  (setq inferior-lisp-program *my-common-lisp-interpreter*)
  
  (load (expand-file-name "~/quicklisp/slime-helper.el"))

  (bind-mode-commands '(slime-mode-hook)
    '((slime-documentation "C-h f")
			(describe-function "C-h M-f")
			(slime-describe-symbol "C-h v")
			(describe-variable "C-h M-v"))))

;;  The most wonderful Paredit...
(when (retrieve-and-load 'paredit)
  (defconst turn-on-paredit-hooks
    '(emacs-lisp-mode-hook
      ielm-mode-hook
      lisp-mode-hook
      lisp-interaction-mode-hook
      ielm-mode-hook
      scheme-mode-hook)
    "List of hooks to which `enable-paredit-mode' will be attached.")
  (autoload 'enable-paredit-mode "paredit" "enable paredit in lisp")
  (mapc (lambda (mode-hook)
	  (add-hook mode-hook #'enable-paredit-mode))
	turn-on-paredit-hooks))

;; (when (and (retrieve-and-load 'slime)
;; 	   (retrieve-and-load 'slime-repl))
;;   (add-hook 'lisp-mode-hook
;; 	    (lambda ()
;; 	      (interactive)
;; 	      (unless (ignore-errors (slime-ping))
;; 		(when (y-or-n-p "Spawn a new SLIME REPL? ") (slime))))))

;; Common Lisp macro indenting:
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (set (make-local-variable 'lisp-indent-function)
           'common-lisp-indent-function)))
(put 'alambda 'lisp-indent-function 1)
(put 'with-gensyms 'lisp-indent-function 1)
(put 'bind-mode-commands 'lisp-indent-function 1)

;;  Semantic mode
(when (retrieve-and-load 'semantic)
  (semantic-mode t)
 (declare-function global-semantic-idle-scheduler-mode "semantic.el")
 (global-semantic-idle-scheduler-mode t))

;; EDE mode
(when (retrieve-and-load 'ede)
  (global-ede-mode))

;;  Auto-complete (AC mode)
(when (retrieve-and-load 'auto-complete)
  (declare-function global-auto-complete-mode "auto-complete.el")
  (declare-function ac-config-default "auto-complete-config.el")
  (global-auto-complete-mode t)
  (ac-config-default))

;;  Irony Mode
(when (retrieve-and-load 'irony)
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

;;  Flycheck
(when (retrieve-and-load 'flycheck)
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

;;  Emacs code browser (ECB) setup.  I have this just cloned into
;;  ~/devel, installation from Marmalade/etc. doesn't work.

;;  That is, I _can't_ use `retrieve-and-load' because the ECB package
;;  is broken as of March 21, 2015.

;;  It also doesn't parse C++ functions properly (the new trailing
;;  return form at least) that's a problem with Semantic, though, not
;;  this.  So I'm turning it off for now.
(when nil
 (let ((ecb-path (expand-file-name "~/devel/ecb/")))
   (when (file-readable-p ecb-path)
     (add-to-list 'load-path ecb-path)
     (defvar ecb-examples-bufferinfo-buffer-name)
     (setq ecb-examples-bufferinfo-buffer-name nil)
     (condition-case nil (require 'ecb)))))

;;  When switching between header and code files, use the other window:
(setq ff-always-in-other-window t)

;;  Missing C++ Keywords introduced in C++11:
;;  override, final, alignas, alignof, char16_t, char32_t,
;;  concept, constexpr, decltype, noexcept, nullptr, static_assert,
;;  and thread_local.
;;
;;  I also want to add the operators.
(font-lock-add-keywords
 'c++-mode
 `((,(regexp-opt '("override" "final" "alignas" "alignof" "char16_t"
		   "char32_t" "concept" "constexpr" "decltype"
		   "noexcept" "nullptr" "static_assert"
		   "thread_local")
		 'words) . font-lock-keyword-face)))

;;; System configuration
;;  Control how emacs interacts with the system I am on.

;;  Work with the system clipboard.
(setq x-select-enable-clipboard-manager t
      x-select-enable-clipboard         t)

;;  Enable editor backups in .emacs.d/saves/
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        "~/.emacs.d/saves")))
      backup-by-copying      t
      delete-old-versions    t)

(setq kept-new-versions 6
      kept-old-versions 2)

;;  Show keystrokes in the minibuffer instantly; no dialog boxes; don't
;;  beep.
(setq echo-keystrokes 0.001 ; Can't be just 0; that disables echoing.
      use-dialog-box  nil
      visible-bell    t)

;;  Keep version control going.
(setq version-control t)

(provide 'emacs)
;;; .emacs ends here
