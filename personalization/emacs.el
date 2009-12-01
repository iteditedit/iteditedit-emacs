;;
;; EX's emacs config files
;; mostly consists of other people configs

;; Emacs should be compiled from CVS in order to make it work correctly with
;; nXhtml and ruby mode. At least in debian based, emacs-snapshot is broken.
;;
;; cvs -d:pserver:anonymous@cvs.sv.gnu.org:/sources/emacs co emacs
;; cd emacs
;;
;; Read the INSTALL file
;;
;; Verify you have Xfonts support if you want pretty fonts and other
;; dependencies: txinfo, libgif-dev, libxpm-dev (Images), libgpmg1-dev
;; (Mouse Support)
;;
;; wajig install libxfont-dev libxfont1 txinfo libgif-dev libxpm-dev libgpmg1-dev
;;
;; ./configure
;;
;; make
;; sudo make install
;;
;; Set Monospace font
;;
;; echo "Emacs.font: Monospace-10" >> ~/.Xresources
;; xrdb -merge ~/.Xresources
;;
;; /usr/local/bin/emacs
;; /usr/local/bin/emacsclient

;; Only start emacs-server if it is not already started
(when (and
       (> emacs-major-version 22)
       (or (not (boundp 'server-process))
	   (not (eq (process-status server-process) 'listen))))
  (server-start))

(load-file (concat config-dir "/helpers/general-helper.el"))

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; And for file system too
(setq file-name-coding-system 'utf-8)

;; Type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir (concat config-dir "/autosaves/"))
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms `((".*" ,(concat autosave-dir "\\1") t)))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat config-dir "/backups/"))
(make-directory backup-dir t)
(setq backup-directory-alist (list (cons "." backup-dir)))


;; Running save-buffer automatically
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (buffer-file-name)
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

;; disable menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; disable scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; disable toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable tooltips
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; don't show startup message
(setq inhibit-startup-message t)

;; show line and column numbers
(line-number-mode t)
(column-number-mode t)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ido-mode
(setq ido-use-filename-at-point t)
(ido-mode t)
(ido-everywhere t)
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)

;; icomplete
;; preview command completion when writing in Minibuffer
;; this is part of emacs
(icomplete-mode 1)

;; svn co http://svn.ruby-lang.org/repos/ruby/trunk/misc/ ~/.emacs.d/elisp/ruby-mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)

;; inf-ruby
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys) ))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; rinari
;; http://github.com/eschulte/rinari
(require 'rinari)

(require 'yaml-mode)

;; use exuberant-ctags
;;
;; Generate file with:
;;   ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor
(setq rinari-tags-file-name "TAGS")

;; haml-mode and & sass-mode
;; http://github.com/nex3/haml/
(require 'haml-mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\.sass$" . sass-mode))

;; we are going to debugging
;; svn checkout svn://rubyforge.org/var/svn/ruby-debug
(autoload 'rdebug "rdebug" "Ruby debugging support." t)

(defun rinari-generate-tags()
  (interactive)
  (let ((my-tags-file (concat (rinari-root) "TAGS"))
	(root (rinari-root)))
    (message "Regenerating TAGS file: %s" my-tags-file)
    (if (file-exists-p my-tags-file)
	(delete-file my-tags-file))
    (shell-command
     (format "find %s -regex \".+rb$\" | xargs ctags-exuberant -a -e -f %s"
	     root my-tags-file))
    (if (get-file-buffer my-tags-file)
	 (kill-buffer (get-file-buffer my-tags-file)))
    (visit-tags-table my-tags-file)))
;; (add-hook 'rinari-minor-mode-hook 'rinari-generate-tags)

;; require DVC
;; (require 'dvc-autoloads)
(require 'magit)

(require 'cheat)

(require 'ruby-block)
(ruby-block-mode t)

;; anything
;;(add-to-list 'load-path (concat config-dir "/vendor/anything"))
(require 'anything)

;; anything-rcodetools
;;(add-to-list 'load-path (concat config-dir "/vendor/rcodetools"))
;(require 'rcodetools)
;(require 'icicles-rcodetools)
;(require 'anything)
;;       ;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat fri -l -L")
;;(define-key anything-map "\C-z" 'anything-execute-persistent-action)

;; yasnippet
(add-to-list 'load-path (concat config-dir "/vendor/yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat config-dir "/vendor/yasnippet/snippets"))


;; yasnippet rails
(load (concat config-dir "/vendor/yasnippets-rails/setup.el"))

(when (require 'auto-complete nil t)
  (require 'auto-complete-yasnippet)
  (require 'auto-complete-css)

  (global-auto-complete-mode t)           ;enable global-mode
  (setq ac-auto-start t)                  ;automatically start
  (setq ac-dwim 3)                        ;Do what i mean
  (setq ac-override-local-map nil)        ;don't override local map
  ;   (define-key ac-complete-mode-map "\t" 'ac-expand)
  ;   (define-key ac-complete-mode-map "\r" 'ac-complete)
  ;   (define-key ac-complete-mode-map "\M-n" 'ac-next)
  ;   (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

  (setq ac-modes
	(append ac-modes
		'(eshell-mode
		    ;org-mode
		  )))
		    ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

  (add-hook 'eshell-mode-hook
    (lambda ()
      (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer)))))


;; require AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 3 nil
    (lambda () (delete-windows-on "*Completions*")))))

;; run a few shells.
(shell "*shell5*")

(global-hl-line-mode 1) ; highlighting current line

(desktop-save-mode t) ; save current session on exit

;; close buffer without a confirmation
(defun kill-current-buffer ()
  (interactive)
    (kill-buffer (current-buffer)))

;; Display time
(setq display-time-interval 1)
(setq display-time-format "%H:%M:%S")
(display-time-mode)

(setq custom-file (concat config-dir "/customization.el"))
(load custom-file)
