;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups get saved to the same directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist '(("" . "~/.emacs-backups")))
;; Backup version control 
(setq version-control t)
(setq kept-new-versions 2)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Compatibility modes ... default window behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/modes/pc-mode.elc")
(pc-mode 1)

(setq x-select-enable-clipboard t) 

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
  (shift-region 4))

(defun shift-left ()
  (interactive)
  (shift-region -4))

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


;; Insert Python debug statements
(defun set-py-breakpoint (&optional no-other-breakpoints)
  "Sets the necessary breakpoint py code."
;;  (interactive)
  (cond
   (no-other-breakpoints
    (insert "if not globals().get( 'PDB_ACTIVE', 0 ):
       globals()['PDB_ACTIVE'] = 1
       import pdb; pdb.set_trace()")
    (indent-relative))
   (t
    (insert "import pdb; pdb.set_trace()"))
   ))