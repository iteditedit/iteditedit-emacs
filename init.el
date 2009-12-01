;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Richard Steckroth Emacs Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIAL PATHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq config-dir (file-name-directory
									(or (buffer-file-name) load-file-name)))
(setq vendor-dir (concat config-dir "vendor"))

(setq personal-dir (concat config-dir "personalization"))

(setq module-dir (concat config-dir "modules"))

;; Add top level vendor
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path module-dir)

;; Add all vendor sub dirs
;; http://www.emacswiki.org/emacs/LoadPath
;;(progn (cd vendor-dir)
;;       (normal-top-level-add-subdirs-to-load-path))
;;(normal-top-level-add-to-load-path
;;     '((concat vendor-dir "color-theme")
;;       (concat vendor-dir "gist.el")
;;       (concat vendor-dir "yaml-mode.el")
;;       (concat vendor-dir "markdown-mode")
;;       (concat vendor-dir "yasnippet")
;;       (concat vendor-dir "anything")
;;       (concat vendor-dir "maxframe")
;;       (concat vendor-dir "haml-mode")
;;       (concat vendor-dir "workspaces.el")
;;       (concat vendor-dir "autotest")
;;       (concat vendor-dir "twilight-emacs")
;;       (concat vendor-dir "remember")
;;       (concat vendor-dir "cheat.el")
;;       (concat vendor-dir "ruby-mode")
;;       (concat vendor-dir "auctex")
;;       (concat vendor-dir "rdebug")
;;       (concat vendor-dir "jump.el")
;;       (concat vendor-dir "rinari")
;;       (concat vendor-dir "auto-complete")
;;       (concat vendor-dir "rcov")
;;       (concat vendor-dir "auctex.el")
;;       (concat vendor-dir "yasnippets-rails")
;;       (concat vendor-dir "ruby-block")
;;       (concat vendor-dir "ri-emacs")
;;       (concat vendor-dir "tex-site.el")
;;       (concat vendor-dir "preview-latex.el")
;;       (concat vendor-dir "magit")))
;;;
;;
(add-to-list 'load-path (concat vendor-dir "/."))
(add-to-list 'load-path (concat vendor-dir "/color-theme"))
(add-to-list 'load-path (concat vendor-dir "/gist.el"))
(add-to-list 'load-path (concat vendor-dir "/yaml-mode.el"))
(add-to-list 'load-path (concat vendor-dir "/markdown-mode"))
(add-to-list 'load-path (concat vendor-dir "/yasnippet"))
(add-to-list 'load-path (concat vendor-dir "/anything"))
(add-to-list 'load-path (concat vendor-dir "/maxframe"))
(add-to-list 'load-path (concat vendor-dir "/haml-mode"))
(add-to-list 'load-path (concat vendor-dir "/workspaces.el"))
(add-to-list 'load-path (concat vendor-dir "/autotest"))
(add-to-list 'load-path (concat vendor-dir "/twilight-emacs"))
(add-to-list 'load-path (concat vendor-dir "/remember"))
(add-to-list 'load-path (concat vendor-dir "/cheat.el"))
(add-to-list 'load-path (concat vendor-dir "/ruby-mode"))
(add-to-list 'load-path (concat vendor-dir "/auctex"))
(add-to-list 'load-path (concat vendor-dir "/rdebug"))
(add-to-list 'load-path (concat vendor-dir "/jump.el"))
(add-to-list 'load-path (concat vendor-dir "/rinari"))
(add-to-list 'load-path (concat vendor-dir "/auto-complete"))
(add-to-list 'load-path (concat vendor-dir "/rcov"))
(add-to-list 'load-path (concat vendor-dir "/auctex.el"))
(add-to-list 'load-path (concat vendor-dir "/yasnippets-rails"))
(add-to-list 'load-path (concat vendor-dir "/ruby-block"))
(add-to-list 'load-path (concat vendor-dir "/ri-emacs"))
(add-to-list 'load-path (concat vendor-dir "/tex-site.el"))
(add-to-list 'load-path (concat vendor-dir "/preview-latex.el"))
(add-to-list 'load-path (concat vendor-dir "/magit"))
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
(load-file (concat vendor-dir "/desktop-menu.el"))

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; And for file system too
(setq file-name-coding-system 'utf-8)

;; ido-mode
(setq ido-use-filename-at-point t)
(ido-mode t)
(ido-everywhere t)
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)

;; icomplete
;; preview command completion when writing in Minibuffer
;; this is part of emacs
(icomplete-mode 1)

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

;; require DVC
;; (require 'dvc-autoloads)
(require 'magit)

(require 'cheat)

;; anything
;;(add-to-list 'load-path "~/.emacs.d/vendor/anything")
(require 'anything)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat config-dir "/vendor/yasnippet/snippets"))
(yas/load-directory (concat config-dir "/snippets"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby Module 
;; Provides: Ruby Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ruby-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cedet-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SemanticDB Parser and Senator
;; Pythonic includes and system cmds are put with python module.
;; Currently includes C/C++ for completness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'semantic-module)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ecb Mode
;; Requires: CEDET Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ecb-module)

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

(setq custom-file (concat config-dir "customization.el"))
(load custom-file)
;; Add easy navigation
(global-set-key (kbd "M-<left>") 'windmove-left)          ; move to left windnow
(global-set-key (kbd "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)          ; move to downer window

;; switch to console
(global-set-key (kbd "C-5")
  (lambda () (interactive) (switch-to-buffer "*shell5*")))

;; kill current buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; IDO keybindings
(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

;; Add key for comment-or-uncomment-region function
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; use regexp while searching
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

;; duplicate line
(global-set-key (kbd "C-=") 'duplicate-line)

;; magit-status
(global-set-key "\C-xg" 'magit-status)

;; debugger
(global-set-key [f9] 'gud-step)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-cont)
(global-set-key "\C-c\C-d" 'rdebug)
(require 'color-theme)
(color-theme-initialize)
;; http://edward.oconnor.cx/config/elisp/color-theme-hober2.el (wget) 
(load-file (concat vendor-dir "/twilight-emacs/color-theme-twilight.el"))
(color-theme-twilight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal Switching - Keybindings are F12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'terminal-module)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalized Control modules 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat personal-dir "/keybindings.el"))
(load-file (concat personal-dir "/interface.el"))
(load-file (concat personal-dir "/behavior.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fullscreen on startup
;; Requires: personalization/behavior.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(toggle-fullscreen)
