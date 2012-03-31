;; Add ~/.elisp to load-path.
(add-to-list 'load-path "~/.elisp/")

;; Package management
(require 'package)
(add-to-list 'package-archives
				 '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Interface

;; Don't display the 'Welcome to GNU Emacs' buffer on startup.
(setq inhibit-startup-message t)

;; Replace 'yes or no' prompts by 'y or n'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Get rid of prompt when killing buffers associated with a live
;; process.
;; (setq kill-buffer-query-function
;;       (remq 'process-kill-buffer-query-function
;;             kill-buffer-query-functions))

;; Highlight selection.
(transient-mark-mode t)

;; Faster mark popping.
(setq set-mark-command-repeat-pop t)

;; Always use color syntaxing.
(global-font-lock-mode t)

;; Set color syntaxing to 11.
(setq font-lock-maximum-decoration t)

;; Improve the uniquification of buffer names.
;; Makefile and Makefile<2> become Makefile|project Makefile|test
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward)

;; Set zenburn as the color theme.
(add-to-list 'custom-theme-load-path "~/.elisp/zenburn-emacs")
(load-theme 'zenburn t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Keybindings

;; Quick switch between windows with shift + arrows.
(windmove-default-keybindings)

;; Open a browser page for the url at point.
(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; C-x o is a pain.
(global-set-key [C-tab] 'other-window)

;; Numerical prefixes are not that useful to me. C-u is enough.
(global-set-key "\M-0" 'delete-window)
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-2" 'split-window-vertically)
(global-set-key "\M-3" 'split-window-horizontally)

;; Use ido in M-x.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Org mode bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Jump to a definition in the current file.
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Magit rules!
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp everywhere
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Grab URL contents and open in a new buffer
(global-set-key (kbd "C-c u") 'view-url-in-buffer)

;; Quick open Gnus
(global-set-key (kbd "C-c m") 'gnus)

;; Connect to Bitlbee
(global-set-key (kbd "C-c t") 'bitlbee-connect)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Editing

(prefer-coding-system 'utf-8)

;; Text pasted from the mouse middle click is inserted at point rather
;; than at the mouse cursor position.
(setq-default mouse-yank-at-point t)

;; Automatically fill paragraphs when in text mode.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Delete trailing whitespace on file save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save point place in visited files.
(require 'saveplace)
(setq-default save-place t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode)

;; Project management with projectile
(require 'projectile)
(projectile-global-mode)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Utils

;; Replace all tabs by spaces in the whole buffer.
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;; Indent the whole buffer.
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; Ack integration
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Ack executable is 'ack-grep' in Debian and variants
(setq ack-executable (executable-find "ack-grep"))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
	;; Add CoffeeScript regexps to etags.
	(format "etags -f %s/TAGS --options=$HOME/.elisp/coffee.etags -R %s" dir-name dir-name)))

(defun view-url-in-buffer ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Ido

;; Setup ido-mode.
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 12)
(ido-mode t)
(ido-everywhere)

;; Auto-completion in minibuffer
(icomplete-mode)

;; Refresh the imenu definitions automatically.
(setq-default imenu-auto-rescan t)

;; Use ido in M-x commands
(require 'smex)
(smex-initialize)

;; Setup imenu with ido.
(require 'imenu)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org mode

;; Org-mode auto loading.
(add-to-list 'load-path "~/.elisp/org-mode/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Write a timestamp when a task is marked as DONE.
(setq org-log-done t)

;; Add XeTeX support
(load "~/.elisp/org-export-xelatex.lisp")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Code editing

(setq tags-file-name "TAGS")

;; By default, use tabs to indent and space to align.
(require 'smart-tabs)
(setq-default indent-tabs-mode t)

;; Default to 3 spaces for indentation
(setq-default tab-width 3)

;; Some modes set this to nil.
(defun turn-on-indent-tabs ()
  (setq indent-tabs-mode t))

;; Blacklist modes to use only space
(defun turn-off-indent-tabs ()
  (setq indent-tabs-mode nil))

;; Show matching parens (mixed style).
(show-paren-mode t)
(setq show-paren-delay 0.0)

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-off-electric-indent ()
  (electric-indent-mode nil))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
          1 font-lock-warning-face t))))

(defun coding-hooks ()
  "Hook that gets run on activation of any programming mode."
  (local-column-number-mode)
  (local-comment-auto-fill)
  (abbrev-mode)
  (add-watchwords)
  (electric-indent-mode)
  (electric-layout-mode)
  (electric-pair-mode))

(add-hook 'prog-mode-hook 'coding-hooks)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Coding modes

;; Default javascript mode.
(setq js-indent-level tab-width)

;; Coffee mode for CoffeeScript
(add-to-list 'load-path "~/.elisp/coffee-mode")
(require 'coffee-mode)
(add-hook 'coffee-mode-hook 'turn-on-indent-tabs)
(add-hook 'coffee-mode-hook 'turn-off-electric-indent)

;; Javadoc lookup
(autoload 'javadoc-lookup "javadoc-help" "Look up Java class in Javadoc." t)
(autoload 'javadoc-help "javadoc-help" "Open up the Javadoc-help menu." t)
(autoload 'javadoc-set-predefined-urls "javadoc-help" "Set pre-defined urls." t)
(javadoc-set-predefined-urls '("/home/fmdkdd/Edsger/Java/docs/api"))

;; HTML mode
(add-hook 'html-mode-hook 'turn-off-indent-tabs)

;; CSS mode
(setq css-indent-offset tab-width)

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Load MIT/GNU Scheme mode.
(load "~/.elisp/xscheme.elc")

;; Scala mode from latest distribution
(add-to-list 'load-path "~/Archimède/Univ/Cours/MIS9/mobile/scala-2.9.1.final/misc/scala-tool-support/emacs/")
(require 'scala-mode-auto)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Lisp interaction with SBCL

(setq inferior-lisp-program "/usr/bin/sbcl")

;; Speed-up Slime launch with a core dump file.
;; http://common-lisp.net/project/slime/doc/html/Loading-Swank-faster.html
;; (setq slime-lisp-implementations
;;       '((sbcl ("sbcl" "--core" "/home/fmdkdd/.elisp/sbcl.core-with-swank")
;;               :init (lambda (port-file _)
;;                       (format "(swank:start-server %S)\n" port-file)))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Let there be sound!

;; (require 'emms-setup)
;; (emms-standard)
;; (emms-default-players)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Customization

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "firefox")
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(python-python-command "python3")
 '(safe-local-variable-values (quote ((ispell-local-dictionary . francais)))))

(setq calendar-week-start-day 1)
(put 'upcase-region 'disabled nil)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Printing

;; No header and A4 paper by default.
(setq ps-print-header nil
      ps-paper-type 'a4)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Misc

(defun let-her-sleep ()
  (interactive)
  (let ((duration (read-string "How much (15 min): " nil nil "15 min")))
    (run-at-time duration nil
					  (lambda ()
						 (notifications-notify :title "Wake up sleepyhead")))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Mail with Gnus

(setq gnus-directory "~/Miel")
(require 'offlineimap)
(add-hook 'gnus-before-startup-hook 'offlineimap)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Contacts with org-contacts

;; Org-contacts from contrib
(load "~/.elisp/org-mode/contrib/lisp/org-contacts.el")

;; My contacts file
(setq org-contacts-files '("~/Miel/contacts.org"))

;; Google-contacts
;; (add-to-list 'load-path "~/.elisp/google-contacts")
;; (require 'google-contacts)
;; (require 'google-contacts-gnus)
;; (require 'google-contacts-message)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Bibliography management with org-mode and reftex
;; see https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
			(reftex-parse-all)
			(reftex-set-cite-format  ; add a custom reftex cite format to
											 ; insert links
			 '((?b . "[[bib:%l][%l-bib]]")
				(?n . "[[notes:%l][%l-notes]]")
				(?p . "[[papers:%l][%l-paper]]")
				(?t . "%t")
				(?h . "** %t\n\t:PROPERTIES:\n\t:Custom_ID: %l\n\t:END:\n\t[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun org-mode-reftex-search ()
  ; jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (reftex-citation t))))

(setq org-link-abbrev-alist
		'(("bib" . "~/Archimède/Univ/Cours/MIS10/refs.bib::%s")
		  ("notes" . "~/Archimède/Univ/Cours/MIS10/notes.org::#%s")
		  ("papers" . "~/Archimède/Univ/Cours/MIS10/papers/%s.pdf")))

;; Linking to this bibliography when drafting in org-mode
;; see http://www-public.it-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/

(defun my-rtcite-export-handler (path desc format)
  (message "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
  (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (cond ((eq format 'latex)
           (if (or (not desc)
                   (equal 0 (search "rtcite:" desc)))
               (format "\\cite{%s}" search)
             (format "\\cite[%s]{%s}" desc search))))))

(require 'org)
(org-add-link-type "rtcite"
                   'org-bibtex-open
                   'my-rtcite-export-handler)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ERC

;; Connect to local Bitlbee for MSN / Google Talk / Twitter
(defun bitlbee-connect ()
  (interactive)
  (erc :server "localhost"
		 :port "6667"
		 :nick "fmdkdd"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Typing game to annoy your coworkers by banging on your keyboard

(autoload 'typing-of-emacs "typing" "Typing Of Emacs, a game." t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Improve your writing style

(require 'writegood-mode)
