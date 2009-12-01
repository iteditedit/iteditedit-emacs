;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups get saved to the same directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq backup-directory-alist '(("" . "~/.emacs-backups")))
;;;; Backup version control
;;(setq version-control t)
;;(setq kept-new-versions 2)
;;(setq delete-old-versions t)
;;(setq kept-old-versions 2)
;;(setq dired-kept-versions 1)

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
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Compatibility modes ... default window behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat vendor-dir "/pc-mode.elc"))
(pc-mode 1)

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; Better Tab support
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

;; turn on paren matching
(show-paren-mode t)
(setq show-paren-style 'mixed)

; Moving cursor down at bottom scrolls only a single line, not half page
;; for smooth scrolling and disabling the automatical recentering of emacs when moving the cursor
(setq-default scroll-margin 1 scroll-conservatively 0)
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

;(savehist-mode) ; to save minibuffer history
;;(setq visual-bell t)

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)


(defadvice kill-new (before kill-new-push-xselection-on-kill-ring activate)
  "Before putting new kill onto the kill-ring, add the clipboard/external selection to the kill ring"
  (let ((have-paste (and interprogram-paste-function
                         (funcall interprogram-paste-function))))
    (when have-paste (push have-paste kill-ring))))

;; clean out buffers except shell, agenda and org
(defun restart ()
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
             (name (buffer-name buffer)))
        (and (not (string-equal name "*shell*"))
             (not (string-equal name "*scratch*"))
             (kill-buffer buffer)))
      (setq list (cdr list)))))

;; clean out buffers except shell, agenda and org
(defun clear-help-buffers ()
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
             (name (buffer-name buffer)))
        (and (not (string-equal name "*shell*"))
             (not (string-equal name "*scratch*"))
             (kill-buffer buffer)))
      (setq list (cdr list)))))


;;;
;;; Matching parentheses
;;;
(defun match-parenthesis (arg)
  "Match the current character according to the syntax table.

Based on the freely available match-paren.el by Kayvan Sylvan.
I merged code from goto-matching-paren-or-insert and match-it.

You can define new \"parentheses\" (matching pairs).
Example: angle brackets. Add the following to your .emacs file:

	(modify-syntax-entry ?< \"(>\" )
	(modify-syntax-entry ?> \")<\" )

You can set hot keys to perform matching with one keystroke.
Example: f6 and Control-C 6.

	(global-set-key \"\\C-c6\" 'match-parenthesis)
	(global-set-key [f6] 'match-parenthesis)

Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  (let
      ((syntax (char-syntax (following-char))))
  (cond
   ((= syntax ?\()
    (forward-sexp 1) (backward-char))
   ((= syntax ?\))
    (forward-char) (backward-sexp 1))
   (t (message "No match"))
   )
  ))

;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
	(save-excursion
	  (indent-rigidly (region-beginning) (region-end) distance)
	  (push-mark mark t t)
	  ;; Tell the command loop not to deactivate the mark
	  ;; for transient mark mode
	  (setq deactivate-mark nil))))

;; TODO: Select based on mode
(defun shift-right ()
  (interactive)
  (shift-region tab-width))

(defun shift-left ()
  (interactive)
  (shift-region (- tab_width)))

(defun pull-next-line()
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

;; Buffer Quick Kill
(defun buffer-quick-kill ()
  "Shortcut for killing a buffer"
  (interactive)
  (kill-buffer (buffer-name)))


;;(defun message (arg &rest args)
;;)
