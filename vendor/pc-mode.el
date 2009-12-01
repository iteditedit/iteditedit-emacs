
;; This is PC-MODE, a minor mode for GNU Emacs.
;; Copyright (C) 2007 Thomas Becker.

;; PC-MODE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;; Everyone is granted permission to copy, modify and redistribute
;; PC-MODE, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; The HTML documentation of PC mode can be found at
;;
;; http://thbecker.net/free_software_utilities/emacs_lisp/pc_mode/pc_mode.html
;;
;; Send bug reports, questions, and comments to: pcmode@thbecker.net

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Each minor mode must have an entry in minor-mode-alist and a variable of
;; the same name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or (assq 'pc-mode minor-mode-alist)
    (setq minor-mode-alist 
	  (cons '(pc-mode " PC") minor-mode-alist)))

(defvar pc-mode nil
  "Non-nil when emulating PC-based editors")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Variables that save state while pc-mode is on.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pc-save-scroll-step nil
  "Holds the original value of scroll-step while pc-mode is on.")

(defvar pc-save-pc-selection-mode
  "Holds the original value of pc-selection-mode while pc-mode is on.")

(defvar pc-save-transient-mark-mode
  "Holds the original value of transient-mark-mode while pc-mode is on.")

(defvar pc-save-delete-selection-mode
  "Holds the original value of delete-selection-mode while pc-mode is on.")

(defvar pc-save-next-line-add-newlines nil
  "Original value of next-line-add-newlines while pc-mode is on.")

(defvar pc-save-region-face-foreground (face-foreground 'region)
  "Original value of the region's face foreground while pc-mode is on.")

(defvar pc-save-region-face-background (face-background 'region)
  "Original value of the region's face background while pc-mode is on.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Variables that control the behavior of pc-mode externally
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pc-exchanging-point-and-mark-activates-mark-state nil
  "Variable that controls whether exchanging the point and the mark activates the mark.")

(defvar pc-kill-ring-save-deactivates-mark-state nil
  "Variable that controls whether calling saving the region as kill will deactivate the mark.")

(defvar pc-control-tab-emulation-type 'vc8
  "Variable that controls how buffer selection with control-tab works.
Must be one of 'emacs, 'vc6, and 'vc8.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables that pc-mode uses internally
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pc-internal-goal-column 0)
(make-variable-buffer-local 'pc-internal-goal-column)
(defvar pc-fixed-cursor-scroll-column 0)
(make-variable-buffer-local 'pc-fixed-cursor-scroll-column)
(defvar pc-other-fixed-cursor-scroll-column 0)
(make-variable-buffer-local 'pc-other-fixed-cursor-scroll-column)
(defvar pc-fixed-cursor-scroll-line 0)
(make-variable-buffer-local 'pc-fixed-cursor-scroll-line)
(defvar pc-other-fixed-cursor-scroll-line 0)
(make-variable-buffer-local 'pc-other-fixed-cursor-scroll-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Each minor mode must have a function of the same name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-mode (arg)
  "Emulates some features of PC-based editors such as MS Visual Studio 8.

There is hardly any need to study documentation in order to use PC Mode. If
you want to find out about PC Mode in detail (e.g., to learn about ways to
customize PC Mode), go to

http://thbecker.net/free_software_utilities/emacs_lisp/pc_mode/pc_mode.html

For most users, it will suffice to try the following key combinations (you
may want to pay attention to the behavior of the cursor upon scrolling):

arrow keys, home, control-home, end, control-end, and page up/down,
all with and without the shift key

page-up and page-down

control-up-arrow, control-down-arrow, alt-up-arrow and alt-down-arrow

control-TAB and shift-control-TAB

control-page-up and control-page-down, with two visible buffers

M-% with and without an active selection, and when a search has been performed earlier

The function pc-mode controls whether PC Mode is on or off. When called interactively
with no prefix argument or programmatically with argument nil, the function toggles
PC Mode. When called with an argument whose numeric value is positive (e.g., interactively
with prefix C-u or C-u 42), it turns PC Mode on. When called with argument whose numeric
value is non-positive (e.g., interactively with prefix C-u 0 or C-u -1), it turns PC Mode
off."
  (interactive "P")
  (if (null arg) 
      (if pc-mode
          (pc-mode-off)
        (pc-mode-on))
    (if (> (prefix-numeric-value arg) 0)
        (if (null pc-mode)
            (pc-mode-on))
      (if pc-mode 
          (pc-mode-off)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Turning pc-mode on and off
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-mode-on()
  "Turns pc-mode on. Called by function pc-mode."

  (setq pc-mode t)

  ;; Save value of scroll-step, transient-mark-mode, delete-selection-mode, and
  ;; next-line-add-newlines, then set their values to 1, t, t, and nil, respectively.

  (setq pc-save-scroll-step scroll-step)
  (setq scroll-step 1)

  (setq pc-save-pc-selection-mode pc-selection-mode)
  (setq pc-save-transient-mark-mode transient-mark-mode)
  (setq pc-save-delete-selection-mode delete-selection-mode)

  (pc-selection-mode 0)
  (transient-mark-mode 1)
  (delete-selection-mode 1)

  (setq pc-save-next-line-add-newlines next-line-add-newlines)
  (setq next-line-add-newlines nil)

  ;; Save and reset region face foreground and background.
  (setq pc-save-region-face-foreground (face-foreground 'region))
  (setq pc-save-region-face-background (face-background 'region))
  (set-face-foreground 'region "ghostwhite")
  (set-face-background 'region "midnightblue")

  ;; Set function keys so that scrolling and cursor movement deactivate region
  ;;
  (global-set-key [down] 'pc-next-line)
  (global-set-key [up] 'pc-previous-line)
  (global-set-key [right] 'pc-forward-char)
  (global-set-key [left] 'pc-backward-char)
  (global-set-key [home] 'pc-beginning-of-line)
  (global-set-key [end] 'pc-end-of-line)
  (global-set-key [prior] 'pc-fixed-cursor-scroll-down)
  (global-set-key [next] 'pc-fixed-cursor-scroll-up)
  (global-set-key [C-prior] 'pc-fixed-cursor-parallel-scroll-down-one)
  (global-set-key [C-next] 'pc-fixed-cursor-parallel-scroll-up-one)
  (global-set-key [C-down] 'pc-fixed-cursor-scroll-up-one)
  (global-set-key [C-up] 'pc-fixed-cursor-scroll-down-one)
  (global-set-key [M-down] '(lambda () (interactive) (scroll-up 1)))
  (global-set-key [M-up] '(lambda () (interactive) (scroll-down 1)))
  (global-set-key [C-right] 'pc-forward-word)
  (global-set-key [C-left] 'pc-backward-word)
  (global-set-key [C-home] 'pc-beginning-of-buffer)
  (global-set-key [C-end] 'pc-end-of-buffer)

  ;; Set function keys so that scrolling and cursor movement with shift key 
  ;; pressed will set the mark when region is not active, perform movement, 
  ;; and activate region. 
  ;;
  (global-set-key [S-down] 'pc-next-line-shift)
  (global-set-key [S-up] 'pc-previous-line-shift)
  (global-set-key [S-right] 'pc-forward-char-shift)
  (global-set-key [S-left] 'pc-backward-char-shift)
  (global-set-key [S-home] 'pc-beginning-of-line-shift)
  (global-set-key [S-end] 'pc-end-of-line-shift)
  (global-set-key [S-prior] 'pc-fixed-cursor-scroll-down-shift)
  (global-set-key [S-next] 'pc-fixed-cursor-scroll-up-shift)
  (global-set-key [C-S-down] 'pc-fixed-cursor-scroll-up-one-shift)
  (global-set-key [C-S-up] 'pc-fixed-cursor-scroll-down-one-shift)
  (global-set-key [C-S-right] 'pc-forward-word-shift)
  (global-set-key [C-S-left] 'pc-backward-word-shift)
  (global-set-key [C-S-home] 'pc-beginning-of-buffer-shift)
  (global-set-key [C-S-end] 'pc-end-of-buffer-shift)

  ;; Switch buffers C-TAB style
  (define-key global-map [C-tab] 'pc-control-tab-emulation)
  (define-key global-map [C-S-tab] 'pc-control-tab-emulation)
  (define-key global-map [C-S-iso-lefttab] 'pc-control-tab-emulation)

  ;; Rebind C-x C-x to make it sensitive to the value of  pc-exchanging-point-and-mark-activates-mark-state
  (define-key global-map [?\C-x ?\C-x] 'pc-exchange-point-and-mark)

  ;; Rebind M-% to run a slightly modified version of query-replace
  (define-key global-map [?\M-%] 'pc-query-replace)

  ;; Rebind saving and yanking (copy and paste) so that copy does not deactivate the region.
  (define-key global-map [?\C-y] 'pc-yank)
  (define-key global-map [S-insert] 'pc-yank)
  (define-key global-map [?\M-w] 'pc-kill-ring-save)
  (define-key global-map [C-insert] 'pc-kill-ring-save))

(defun pc-mode-off()
  "Turns pc-mode off. Called by function pc-mode."

  (setq pc-mode nil)

  ;; Restore region face foreground and background.
  (set-face-foreground 'region pc-save-region-face-foreground)
  (set-face-background 'region pc-save-region-face-background)

  ;; Restore function keys that scroll or move the cursor
  ;;
  (define-key global-map [down] 'next-line)
  (define-key global-map [up] 'previous-line)
  (define-key global-map [right] 'forward-char)
  (define-key global-map [left] 'backward-char)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (define-key global-map [C-home] 'beginning-of-buffer)
  (define-key global-map [C-end] 'end-of-buffer)
  (define-key global-map [C-up] 'backward-paragraph)
  (define-key global-map [C-down] 'forward-paragraph)
  (define-key global-map [M-up] nil)
  (define-key global-map [M-down] nil)
  (define-key global-map [prior] 'scroll-down)
  (define-key global-map [next] 'scroll-up)
  (define-key global-map [C-right] 'forward-word)
  (define-key global-map [C-left] 'backward-word)

  ;; Restore shift versions of function keys that scroll or move cursor
  ;;
  (define-key global-map [S-down] 'next-line)
  (define-key global-map [S-up] 'previous-line)
  (define-key global-map [S-right] 'forward-char)
  (define-key global-map [S-left] 'backward-char)
  (define-key global-map [S-home] 'beginning-of-line)
  (define-key global-map [S-end] 'end-of-line)
  (define-key global-map [C-S-home] 'beginning-of-buffer)
  (define-key global-map [C-S-end] 'end-of-buffer)
  (define-key global-map [C-S-up] 'backward-paragraph)
  (define-key global-map [C-S-down] 'forward-paragraph)
  (define-key global-map [S-prior] 'scroll-down)
  (define-key global-map [S-next] 'scroll-up)
  (define-key global-map [C-S-right] 'forward-word)
  (define-key global-map [C-S-left] 'backward-word)

  ;; Undefine C-tab and C-S-tab
  ;;
  (global-unset-key [C-tab])
  (global-unset-key [C-S-tab])
  (global-unset-key [C-S-iso-lefttab])

  ;; Restore default C-x C-x binding
  (define-key global-map [?\C-x ?\C-x] 'exchange-point-and-mark)

  ;; Restore M-% standard query-replace
  (define-key global-map [?\M-%] 'query-replace)

  ;; Restore standard saving and yanking
  (define-key global-map [?\C-y] 'yank)
  (define-key global-map [S-insert] 'yank)
  (define-key global-map [?\M-w] 'kill-ring-save)
  (define-key global-map [C-insert] 'kill-ring-save)

  ;; Restore value of scroll-step, transient-mark-mode, delete-selection-mode,
  ;; and next-line-extends-end-of-buffer.

  (setq  scroll-step pc-save-scroll-step)

  (if pc-save-pc-selection-mode
      (pc-selection-mode 1)
    (pc-selection-mode 0))

  (if pc-save-transient-mark-mode
      (transient-mark-mode 1)
    (transient-mark-mode 0))

  (if pc-save-delete-selection-mode
      (delete-selection-mode 1)
    (delete-selection-mode 0))

  (setq next-line-add-newlines	pc-save-next-line-add-newlines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that are to be bound to keys that scroll or move the cursor.
;; These functions deactivate the region if it was active, then perform the
;; requested action.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-next-line (arg)
  "Replaces next-line in pc-mode"
  (interactive "p")
  (let ((old-goal-column goal-column))
    (if (not goal-column)
        (if (or (eq last-command 'pc-next-line)
                (eq last-command 'pc-previous-line)
                (eq last-command 'pc-next-line-shift)
                (eq last-command 'pc-previous-line-shift))
            (setq goal-column pc-internal-goal-column)
          (setq goal-column (current-column))
          (setq pc-internal-goal-column goal-column)))
    (deactivate-mark)
    (condition-case
        err
        (next-line arg)
      (beginning-of-buffer
       (progn (ding) (message "Beginning of buffer")))
      (end-of-buffer
       (progn (ding) (message "End of buffer")))
      (error nil))
    (setq goal-column old-goal-column)))

(defun pc-previous-line (arg)
  "Replaces previous-line in pc-mode"
  (interactive "p")
  (let ((old-goal-column goal-column))
    (if (not goal-column)
        (if (or (eq last-command 'pc-next-line)
                (eq last-command 'pc-previous-line)
                (eq last-command 'pc-next-line-shift)
                (eq last-command 'pc-previous-line-shift))
            (setq goal-column pc-internal-goal-column)
          (setq goal-column (current-column))
          (setq pc-internal-goal-column goal-column)))
    (deactivate-mark)
    (condition-case
        err
        (previous-line arg)
      (beginning-of-buffer
       (progn (ding) (message "Beginning of buffer")))
      (end-of-buffer
       (progn (ding) (message "End of buffer")))
      (error nil))
    (setq goal-column old-goal-column)))

(defun pc-forward-char (arg)
  "Replaces forward-char in pc-mode"
  (interactive "p")
  (deactivate-mark)
  (forward-char arg))

(defun pc-backward-char (arg)
  "Replaces backward-char in pc-mode"
  (interactive "p")
  (deactivate-mark)
  (backward-char arg))

(defun pc-beginning-of-line (arg)
  "Replaces beginnng-of-line in pc-mode"
  (interactive "p")
  (deactivate-mark)
  (beginning-of-line arg))

(defun pc-end-of-line (arg)
  "Replaces end-of-line in pc-mode"
  (interactive "p")
  (deactivate-mark)
  (end-of-line arg))

(defun pc-beginning-of-buffer ()
  "Replaces beginning-of-buffer in pc-mode"
  (interactive)
  (deactivate-mark)
  (beginning-of-buffer))

(defun pc-end-of-buffer ()
  "Replaces end-of-buffer in pc-mode"
  (interactive)
  (deactivate-mark)
  (end-of-buffer))

(defun pc-fixed-cursor-scroll-up-one ()
  "To be bound to \(control down\) in pc-mode"
  (interactive)
  (deactivate-mark)
  (pc-fixed-cursor-scroll-up-one-internal))

(defun pc-fixed-cursor-scroll-down-one ()
  "To be bound to \(control up\) in pc-mode"
  (interactive)
  (deactivate-mark)
  (pc-fixed-cursor-scroll-down-one-internal))

(defun pc-fixed-cursor-scroll-up (arg)
  "Replaces scroll-up in pc-mode"
  (interactive "P")
  (deactivate-mark)
  (pc-fixed-cursor-scroll-up-internal arg))

(defun pc-fixed-cursor-scroll-down (arg)
  "Replaces scroll-down in pc-mode"
  (interactive "P")
  (deactivate-mark)
  (pc-fixed-cursor-scroll-down-internal arg))

(defun pc-forward-word (arg)
  "Replaces forward-word in pc-mode"
  (interactive "p")
  (deactivate-mark)
  (forward-word arg))

(defun pc-backward-word (arg)
  "Replaces backward-word in pc-mode"
  (interactive "p")
  (deactivate-mark)
  (backward-word arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that are to be bound to the shift versions of the keys that
;; scroll or move the cursor. These functions set the mark and activate it if
;; it was inactive, then perform the requested action.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-next-line-shift (arg)
  "Replaces next-line in pc-mode"
  (interactive "p")
  (let ((old-goal-column goal-column))
    (if (not goal-column)
        (if (or (eq last-command 'pc-next-line)
                (eq last-command 'pc-previous-line)
                (eq last-command 'pc-next-line-shift)
                (eq last-command 'pc-previous-line-shift))
            (setq goal-column pc-internal-goal-column)
          (setq goal-column (current-column))
          (setq pc-internal-goal-column goal-column)))
    (set-mark-here-if-not-active)
    (condition-case
        err
        (next-line arg)
      (beginning-of-buffer
       (progn (ding) (message "Beginning of buffer")))
      (end-of-buffer
       (progn (ding) (message "End of buffer")))
      (error nil))
    (setq goal-column old-goal-column)))

(defun pc-previous-line-shift (arg)
  "Replaces previous-line in pc-mode"
  (interactive "p")
  (let ((old-goal-column goal-column))
    (if (not goal-column)
        (if (or (eq last-command 'pc-next-line)
                (eq last-command 'pc-previous-line)
                (eq last-command 'pc-next-line-shift)
                (eq last-command 'pc-previous-line-shift))
            (setq goal-column pc-internal-goal-column)
          (setq goal-column (current-column))
          (setq pc-internal-goal-column goal-column)))
    (set-mark-here-if-not-active)
    (condition-case
        err
        (previous-line arg)
      (beginning-of-buffer
       (progn (ding) (message "Beginning of buffer")))
      (end-of-buffer
       (progn (ding) (message "End of buffer")))
      (error nil))
    (setq goal-column old-goal-column)))

(defun pc-forward-char-shift (arg)
  "Replaces forward-char in pc-mode"
  (interactive "p")
  (set-mark-here-if-not-active)
  (forward-char arg))

(defun pc-backward-char-shift (arg)
  "Replaces backward-char in pc-mode"
  (interactive "p")
  (set-mark-here-if-not-active)
  (backward-char arg))

(defun pc-beginning-of-line-shift (arg)
  "Replaces beginnng-of-line in pc-mode"
  (interactive "p")
  (set-mark-here-if-not-active)
  (beginning-of-line arg))

(defun pc-end-of-line-shift (arg)
  "Replaces end-of-line in pc-mode"
  (interactive "p")
  (set-mark-here-if-not-active)
  (end-of-line arg))

(defun pc-beginning-of-buffer-shift ()
  "Replaces beginning-of-buffer in pc-mode"
  (interactive)
  (set-mark-here-if-not-active)
  (beginning-of-buffer))

(defun pc-end-of-buffer-shift ()
  "Replaces end-of-buffer in pc-mode"
  (interactive)
  (set-mark-here-if-not-active)
  (end-of-buffer))

(defun pc-fixed-cursor-scroll-up-one-shift ()
  "To be bound to \(control down\) in pc-mode"
  (interactive)
  (set-mark-here-if-not-active)
  (pc-fixed-cursor-scroll-up-one-internal))

(defun pc-fixed-cursor-scroll-down-one-shift ()
  "To be bound to \(control up\) in pc-mode"
  (interactive)
  (set-mark-here-if-not-active)
  (pc-fixed-cursor-scroll-down-one-internal))

(defun pc-fixed-cursor-scroll-up-shift (arg)
  "Replaces scroll-up in pc-mode"
  (interactive "P")
  (set-mark-here-if-not-active)
  (pc-fixed-cursor-scroll-up-internal arg))

(defun pc-fixed-cursor-scroll-down-shift (arg)
  "Replaces scroll-down in pc-mode"
  (interactive "P")
  (set-mark-here-if-not-active)
  (pc-fixed-cursor-scroll-down-internal arg))

(defun pc-forward-word-shift (arg)
  "Replaces forward-word in pc-mode"
  (interactive "p")
  (set-mark-here-if-not-active)
  (forward-word arg))

(defun pc-backward-word-shift (arg)
  "Replaces backward-word in pc-mode"
  (interactive "p")
  (set-mark-here-if-not-active)
  (backward-word arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Control-Tab emulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-control-tab-emulation-type-vc8 ()
  "Sets the behavior of control-tab to vc8-style buffer selection."
  (interactive)
  (setq pc-control-tab-emulation-type 'vc8))

(defun pc-control-tab-emulation-type-vc6 ()
  "Sets the behavior of control-tab to vc6-style buffer selection."
  (interactive)
  (setq pc-control-tab-emulation-type 'vc6))

(defun pc-control-tab-emulation-type-emacs ()
  "Sets the behavior of control-tab to emacs-style buffer selection."
  (interactive)
  (setq pc-control-tab-emulation-type 'emacs))

(defun pc-control-tab-emulation (arg)
  "Switches to another buffer (control-tab)-style.
This function must be bound to both the C-TAB key and the C-Sh-TAB key.
The exact behavior can be set to emacs-style, vc6-style, or vc8-style,
by calling the functions pc-control-tab-emulation-type-vc8,
pc-control-tab-emulation-type-vc8, and pc-control-tab-emulation-type-vc8.
Default behavior is vc8."
  (interactive "P")
  (let ((filtered-buffer-list (buffer-list))
        (tmp-buffer-list)
        (check-buffer))

    ;; Remove minibuffers and auxiliary buffers from buffer list
    ;;
    (setq tmp-buffer-list filtered-buffer-list)
    (setq filtered-buffer-list nil)
    (while tmp-buffer-list
      (setq check-buffer (car tmp-buffer-list))
      (setq tmp-buffer-list (cdr tmp-buffer-list))
      (if (or (not (string-match "^[ \t]*\\*.*\\*$" (buffer-name check-buffer)))
              (equal "*scratch*" (buffer-name check-buffer)))
          (setq filtered-buffer-list (cons check-buffer filtered-buffer-list))))
    (setq filtered-buffer-list (nreverse filtered-buffer-list))

    ;; Message if no buffer is left, or only the current buffer is left
    ;; and the request is not to use another window.
    ;;
    (if (null filtered-buffer-list)
        (message "No eligible buffer to switch to")
      (if (and (null (cdr filtered-buffer-list))
               (not arg)
               (eq (car filtered-buffer-list) (current-buffer)))
          (message "%s is the only eligible buffer" 
                   (buffer-name (car filtered-buffer-list)))

        ; Good to go, dispatch to appropriate type of control-tab behavior
        (if (equal pc-control-tab-emulation-type 'emacs)
            (pc-control-tab-emulation-emacs filtered-buffer-list arg)
          (if (equal pc-control-tab-emulation-type 'vc6)
              (pc-control-tab-emulation-vc6 filtered-buffer-list)
            (pc-control-tab-emulation-vc8 filtered-buffer-list arg)))))))
      
(defun pc-control-tab-emulation-emacs (filtered-buffer-list arg)
  "Switches to another buffer with control-tab, using emacs-style prompting in the minibuffer.
When called by C-TAB, it offers to switch to the first buffer on
the local buffer list that is not a minibuffer or an auxiliary
buffer such as *Help*.  When called by C-Sh-TAB, the last such
buffer on the buffer list is used.  When the user presses C-TAB
or C-Sh-TAB repeatedly, all buffers on the buffer list are
offered cyclically, subject to the same selection criteria as the
first one.  Return or TAB selects the buffer, any other input
event is dispatched."
  (let ((num-buffers)
        (current-buf-num)
        (input))

    ;; Set num-buffers to number of buffers on list and 
    ;; current-buf-num to second or last position on list as appropriate
    ;;
    ;; NOTE: Filtered buffer list may have length 1 if there is one
    ;; eligible buffer, but the current buffer is an uneligible one.
    (setq num-buffers (length filtered-buffer-list))
    (if (equal (event-modifiers last-command-event) '(control))
        (setq current-buf-num ; Second buffer on list, counting from 0
              (if (= num-buffers 1) 0 1))
      (setq current-buf-num (1- num-buffers))) ; Last buffer on list
        
    ;; Offer to switch buffers. Input C-TAB moves to next on list, while
    ;; input C-Sh-TAB moves to previous
    ;;
    (message "Switch to buffer %s? (Type TAB or Ret to confirm)"
             (buffer-name (nth current-buf-num filtered-buffer-list)))
    (setq input (read-event))
    (while (and (eventp input)
                (or
                 (eq (event-basic-type input) 'tab)
                 (eq (event-basic-type input) 'iso-lefttab))
                (or (equal (event-modifiers input) '(control))
                    (equal (event-modifiers input) '(control shift))))
      (if (equal (event-modifiers input) '(control))
          (setq current-buf-num (% (1+ current-buf-num) num-buffers))
        (if (= current-buf-num 0)
            (setq current-buf-num (1- num-buffers))
          (setq current-buf-num (1- current-buf-num))))
      (message "Switch to buffer %s? (Type TAB or Ret to confirm)" 
               (buffer-name (nth current-buf-num filtered-buffer-list)))
      (setq input (read-event)))
        
    ;; Switch buffers upon input RET or TAB, dispatch any other input
    ;;
    (if (and (eventp input)
             (or
              (eq (event-basic-type input) 'return)
              (eq (event-basic-type input) 'tab))
             (not (event-modifiers input)))
        (progn 
          (if arg
              (switch-to-buffer-other-window (nth current-buf-num filtered-buffer-list))
            (switch-to-buffer (nth current-buf-num filtered-buffer-list)))
          (message ""))
      (setq unread-command-events (cons input unread-command-events)))))
  
(defun pc-control-tab-emulation-vc6 (filtered-buffer-list)
  "Switches to another buffer with control-tab, using vc6-style displaying of buffers offered.
When called by C-TAB, it offers to switch to the first buffer on the local
buffer list that is not a minibuffer or an auxiliary buffer such as *Help*. 
When called by C-Sh-TAB, the last such buffer on the buffer list is used.
When the user presses C-TAB or C-Sh-TAB, all buffers on the buffer list are 
offered cyclically, subject to the same selection criteria as the first one."
  (let ((original-buffer (current-buffer))
        (num-buffers)
        (current-buf-num)
        (help-message "Selecting buffer. Type C-TAB or C-S-TAB to move, TAB or RET to select, any other key to abort.")
        (input)
        window)

    ;; Set num-buffers to number of buffers on list and 
    ;; current-buf-num to second or last position on list as appropriate
    ;;
    ;; NOTE: Filtered buffer list may have length 1 if there is one
    ;; eligible buffer, but the current buffer is an uneligible one.
    (setq num-buffers (length filtered-buffer-list))
    (if (equal (event-modifiers last-command-event) '(control))
        (setq current-buf-num ; Second buffer on list, counting from 0
              (if (= num-buffers 1) 0 1))
      (setq current-buf-num (1- num-buffers))) ; Last buffer on list
        
    ;; Offer to switch buffers. Input C-TAB moves to next on list, while
    ;; input C-Sh-TAB moves to previous
    ;;
    (unwind-protect
        (progn
          (switch-to-buffer (nth current-buf-num filtered-buffer-list) t)
          (message help-message)
          (setq input (read-event))
          (while (and (eventp input)
                      (or
                       (eq (event-basic-type input) 'tab)
                       (eq (event-basic-type input) 'iso-lefttab))
                      (or (equal (event-modifiers input) '(control))
                          (equal (event-modifiers input) '(control shift))))
            (if (equal (event-modifiers input) '(control))
                (setq current-buf-num (% (1+ current-buf-num) num-buffers))
              (if (= current-buf-num 0)
                  (setq current-buf-num (1- num-buffers))
                (setq current-buf-num (1- current-buf-num) num-buffers)))
            (switch-to-buffer (nth current-buf-num filtered-buffer-list) t)
            (message help-message)
            (setq input (read-event))))
              
          ;; Switch buffers upon input RET, discard any other input
          ;;
          (if (and input
                   (eventp input)
                   (or (eq (event-basic-type input) 'return) (eq (event-basic-type input) 'tab))
                   (not (event-modifiers input)))
              (switch-to-buffer (current-buffer))
            (switch-to-buffer original-buffer))))) ; to fix the buffer list, and in case user aborted

(defun pc-control-tab-emulation-vc8 (filtered-buffer-list arg)
  "Switches to another buffer with control-tab, using vc8-style display of list of buffers.
When called by C-TAB, it offers to switch to the first buffer on the local
buffer list that is not a minibuffer or an auxiliary buffer such as *Help*. 
When called by C-Sh-TAB, the last such buffer on the buffer list is used.
When the user presses C-TAB or C-Sh-TAB, all buffers on the buffer list are 
offered cyclically, subject to the same selection criteria as the first one. "
  (let* ((temp-buffer-list)
        (selection-buffer-name "*PC-Mode Quick Buffer Selection*")
        (help-message "C-TAB/down-arrow=down, C-S-TAB/up-arrow=up, TAB/RET=select, other=abort.")
        (input)
        (num-windows-before (length (window-list)))
        (curr-buffer (current-buffer))
        (curr-buffer-name (buffer-name curr-buffer))
        (force-other-window)
        (target-buffer-name))
    
    (setq force-other-window (or arg
                                 (and (string-match "^[ \t]*\\*.*\\*$" curr-buffer-name)
                                      (not (equal curr-buffer-name "*scratch*")))))

    ;; Bring up the buffer selection buffer.
    ;;
    (unwind-protect
        (progn
          (if (eq 1 num-windows-before)
              (switch-to-buffer-other-window selection-buffer-name t)
            (switch-to-buffer selection-buffer-name t))
          (setq temp-buffer-list filtered-buffer-list)
          (while temp-buffer-list
            (insert (concat (buffer-name (car temp-buffer-list)) "\n"))
            (setq temp-buffer-list (cdr temp-buffer-list)))
          (set-buffer-modified-p nil)
          
          ;; Initial position of selection
          ;;
          (goto-char (point-min))
          ;; NOTE: Filtered buffer list may have length 1 if there is one
          ;; eligible buffer, but the current buffer is an uneligible one.
          (if (> (length filtered-buffer-list) 1)
              (if (equal (event-modifiers last-command-event) '(control))
                  (progn (end-of-line)
                         (if (equal curr-buffer-name (buffer-substring (point-min) (point)))
                             (forward-line 1))
                         (beginning-of-line))
                (goto-char (1- (point-max)))
                (beginning-of-line)))
          (set-mark (point))
          (end-of-line)

          ;; Cycle through displayed buffer names
          ;;
          (message help-message)
          (setq input (read-event))
          (while (and (eventp input)
                      (or (and (eq (event-basic-type input) 'down)
                               (eq (event-modifiers input) nil))
                          (and (eq (event-basic-type input) 'up)
                               (eq (event-modifiers input) nil))
                          (and (eq (event-basic-type input) 'tab)
                               (equal (event-modifiers input) '(control)))
                          (and (eq (event-basic-type input) 'tab)
                               (equal (event-modifiers input) '(control shift)))
                          (and (eq (event-basic-type input) 'iso-lefttab)
                               (equal (event-modifiers input) '(control)))
                          (and (eq (event-basic-type input) 'iso-lefttab)
                               (equal (event-modifiers input) '(control shift)))))
            (message help-message)
            (if (or (and (eq (event-basic-type input) 'down)
                         (eq (event-modifiers input) nil))
                    (and (eq (event-basic-type input) 'tab)
                         (equal (event-modifiers input) '(control)))
                    (and (eq (event-basic-type input) 'iso-lefttab)
                         (equal (event-modifiers input) '(control))))
                (progn
                  (forward-line 1)
                  (if (eobp) (goto-char (point-min))))
              (if (equal -1 (forward-line -1))
                  (progn (goto-char (1- (point-max)))
                         (beginning-of-line))))
            (set-mark (point))
            (end-of-line)
            (setq input (read-event))))

      ;; Record selected buffer name, kill temp buffer
      ;;
      (let ((eolpos (point)))
        (beginning-of-line)
        (setq target-buffer-name (buffer-substring (point) eolpos)))
      (kill-buffer nil)
      (if (eq 1 num-windows-before)
          (delete-window)))

    (switch-to-buffer curr-buffer) ; without this, buffer selection window would not always be restored properly

    ;; Switch buffers upon input RET or TAB
    ;;
    (if (and (eventp input)
             (or (eq (event-basic-type input) 'return)
                 (eq (event-basic-type input) 'tab))
             (not (event-modifiers input)))
        (if (or force-other-window
                (and
                 (get-buffer-window target-buffer-name)
                 (not (eq (get-buffer target-buffer-name) curr-buffer))))
              (switch-to-buffer-other-window target-buffer-name)
          (switch-to-buffer target-buffer-name))))
  (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that deal with the behavior of the cursor upon scrolling.
;; The goal is to make the cursor stay at the same screen position as much
;; as possible.
;:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-screen-column ()
  "Returns current column counted from beginning of screen line.
Here, beginning of screen line may not be exactly the left window margin,
namely when a character whose screen representation consists of several 
columns protrudes into the current screen line."
  (let ((pos (point))
        (pos-col (current-column))
        (col))
    (vertical-motion 0)
    (setq col (- pos-col (current-column)))
    (goto-char pos)
    col))

(defun pc-count-screen-lines ()
  "Counts the number of screen lines from top of window to cursor.
This is clumsy and should really be a primitive function."
  (if truncate-lines
      (+ (count-lines (window-start) (point))
         (if (bolp) 1 0))
    (pc-count-screen-lines-wrapped)))
       
(defun pc-count-screen-lines-wrapped ()
  "Called by pc-count-screen-lines if truncate-lines is nil"
  (let ((pos (point))
        (window-line-count 0))
    (goto-char (window-start))
    (while (and (<= (point) pos)
                (/= (point) (point-max)))
      (vertical-motion 1)
      (setq window-line-count (1+ window-line-count)))
    (goto-char pos)
    (if (and (bolp)
             (= (point) (point-max)))
        (1+ window-line-count)
      window-line-count)))

(defun pc-fixed-cursor-scroll-down-internal (arg)
  "Acts like scroll-down, but leaves point fixed relative to window.
Just like previous-line and forward-line, this function should not be 
used in programs."
  (if (and arg (< arg 0))
      (pc-fixed-cursor-scroll-up-internal (- arg)) 
    (or (string-match "^pc-fixed-cursor-\\(scroll\\|parallel-scroll\\)-" (prin1-to-string last-command))
        (and (setq pc-fixed-cursor-scroll-column (pc-screen-column))
             (setq pc-fixed-cursor-scroll-line (pc-count-screen-lines))))
    (let ((scroll-by (if arg arg (1- (- (window-height) next-screen-context-lines)))))
      (condition-case
          err
          (scroll-down scroll-by)
        (beginning-of-buffer
         (progn (ding) (message "Beginning of buffer")))
        (end-of-buffer
         (progn (ding) (message "End of buffer")))
        (error nil)))
    (move-to-window-line (1- pc-fixed-cursor-scroll-line)) 
    (move-to-column (+ (current-column) 
                       pc-fixed-cursor-scroll-column))
    (and (= pc-fixed-cursor-scroll-column (1- (window-width))) 
         (null (eolp))
         (forward-char -1)))
  nil)

(defun pc-fixed-cursor-scroll-up-internal (arg)
  "Acts like scroll-up, but leaves point fixed relative to window.
Just like previous-line and forward-line, this function should not 
be used in programs."
  (if (and arg (< arg 0))
      (pc-fixed-cursor-scroll-down-internal (- arg)) 
    (or (string-match "^pc-fixed-cursor-\\(scroll\\|parallel-scroll\\)-" (prin1-to-string last-command))
        (and (setq pc-fixed-cursor-scroll-column (pc-screen-column))
             (setq pc-fixed-cursor-scroll-line (pc-count-screen-lines))))
    (let ((scroll-by (if arg arg (1- (- (window-height) next-screen-context-lines)))))
      (condition-case
          err
          (scroll-up scroll-by)
        (beginning-of-buffer
         (progn (ding) (message "Beginning of buffer")))
        (end-of-buffer
         (progn (ding) (message "End of buffer")))
        (error nil)))
    (if (not (eobp)) (move-to-window-line (1- pc-fixed-cursor-scroll-line)))
    (move-to-column (+ (current-column) pc-fixed-cursor-scroll-column))
    (and (= pc-fixed-cursor-scroll-column (1- (window-width))) 
         (null (eolp))
         (forward-char -1)))
  nil)

(defun pc-fixed-cursor-scroll-down-one-internal () 
  "Acts like pc-fixed-cursor-scroll-down, but scrolls by one line only.
Although this function does nothing but \(pc-fixed-cursor-scroll-down 1\), 
this must be bound to a key as a function and not as a lambda expression. 
Just like previous-line and forward-line, this function should not be used 
in programs."  
  (pc-fixed-cursor-scroll-down-internal 1))

(defun pc-fixed-cursor-scroll-up-one-internal ()
  "Acts like pc-fixed-cursor-scroll-up, but scrolls by one line only.
Although this function does nothing but \(pc-fixed-cursor-scroll-up 1\), this 
must be bound to a key as a function and not as a lambda expression. 
Just like previous-line and forward-line, this function should not be 
used in programs."
  (pc-fixed-cursor-scroll-up-internal 1))

(defun pc-fixed-cursor-parallel-scroll-up-one ()
  "Fixed-Cursor-scrolls window and other window up by one screen line."
  (interactive)

  (let ((win (get-buffer-window (current-buffer))))
    (other-window 1)                               
    (if (eq win (get-buffer-window (current-buffer)))
        (error "There is no other window")))

  (let ((pos (point))
        can-scroll)
    (goto-char (window-start))
    (setq can-scroll (>= (vertical-motion 2) 2))
    (goto-char pos)
    (other-window -1)
    (if (not can-scroll)
      (error "End of buffer in other window")))

  (let ((pos (point))
        can-scroll)
    (goto-char (window-start))
    (setq can-scroll (>= (vertical-motion 2) 2))
    (goto-char pos)
    (if (not can-scroll)
      (error "End of buffer")))

  (pc-fixed-cursor-scroll-up-internal 1)
  (other-window 1)                               

  (or (string-match "^pc-fixed-cursor-parallel-scroll-" (prin1-to-string last-command))
      (and (setq pc-other-fixed-cursor-scroll-column (pc-screen-column))
           (setq pc-other-fixed-cursor-scroll-line (pc-count-screen-lines))))
  (scroll-up 1)
  (move-to-window-line (1- pc-other-fixed-cursor-scroll-line)) 
  (move-to-column (+ (current-column) pc-other-fixed-cursor-scroll-column))
  (and (= pc-other-fixed-cursor-scroll-column (1- (window-width))) 
       (null (eolp))
       (forward-char -1))

  (other-window -1))

(defun pc-fixed-cursor-parallel-scroll-down-one ()
  "Fixed-Cursor-scrolls window and other window down by one screen line."
  (interactive)

  (let ((win (get-buffer-window (current-buffer))))
    (other-window 1)                               
    (if (eq win (get-buffer-window (current-buffer)))
        (error "There is no other window")))

  (let ((pos (point))
        can-scroll)
    (goto-char (window-start))
    (setq can-scroll (not (pos-visible-in-window-p (point-min))))
    (goto-char pos)
    (other-window -1)
    (if (not can-scroll)
      (error "Beginning of buffer in other window")))

  (let ((pos (point))
        can-scroll)
    (goto-char (window-start))
    (setq can-scroll (not (pos-visible-in-window-p (point-min))))
    (goto-char pos)
    (if (not can-scroll)
      (error "Beginning of buffer")))

  (pc-fixed-cursor-scroll-down-internal 1)
  (other-window 1)                               

  (or (string-match "^pc-fixed-cursor-parallel-scroll-" (prin1-to-string last-command))
      (and (setq pc-other-fixed-cursor-scroll-column (pc-screen-column))
           (setq pc-other-fixed-cursor-scroll-line (pc-count-screen-lines))))
  (scroll-down 1)
  (move-to-window-line (1- pc-other-fixed-cursor-scroll-line)) 
  (move-to-column (+ (current-column) pc-other-fixed-cursor-scroll-column))
  (and (= pc-other-fixed-cursor-scroll-column (1- (window-width))) 
       (null (eolp))
       (forward-char -1))

  (other-window -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Exchanging point and mark and (de)activating the mark
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-exchange-point-and-mark ()
  "Replaces exchange-point-and-mark in pc-mode. Acts like exchange-point-and-mark
when pc-exchanging-point-and-mark-activates-mark-state is true. Otherwise, it just
exchanges the point and the mark (even when the latter is not active), but does
not activate an inactive mark."
  (interactive)
  (let ((mark-was-already-active mark-active))
    (exchange-point-and-mark)
    (if (and (not mark-was-already-active)
             (not pc-exchanging-point-and-mark-activates-mark-state))
        (deactivate-mark))))

(defun pc-exchanging-point-and-mark-activates-mark (arg)
  "Controls whether or not exchanging the point and the mark activates an inactive mark.
With argument nil (no prefix argument), toggles the state. When called with argument
whose numeric value is positive (such as prefix C-u or C-u 42), exchanging the point
and the mark will activate an inactive mark. When called with argument whose numeric
value is non-positive (such as prefix C-u 0 or C-u -1), exchanging the point and the mark
will not activate an inactive mark."
  (interactive "P")
  (if arg
      (if (> (prefix-numeric-value arg) 0)
          (setq pc-exchanging-point-and-mark-activates-mark-state t)
        (setq pc-exchanging-point-and-mark-activates-mark-state nil))
    (setq pc-exchanging-point-and-mark-activates-mark-state
          (not pc-exchanging-point-and-mark-activates-mark-state))))

(defun pc-toggle-mark-active ()
  "Toggles mark between active and not active."
  (interactive)
  (if (not (mark t))
      (error "No mark set in this buffer"))
  (if mark-active
      (deactivate-mark)
    (setq mark-active t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Saving and yanking (copy and paste). We want the mark to remain active
;; when saving the region as kill (copying).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-kill-ring-save (beg end)
  "Like kill-ring-save, but does not deactivate the mark."
  (interactive "r")
  (copy-region-as-kill beg end)
  (message "Region saved as kill")
  (if (not pc-kill-ring-save-deactivates-mark-state)
     (setq deactivate-mark nil))
  )

(defun pc-yank (arg)
  "Like yank, but deletes the region, if active, before yanking.
This may seem odd, but it is necessary to make pc-kill-ring-save work properly."
  (interactive "*P")
  (if (and (not pc-kill-ring-save-deactivates-mark-state)
           mark-active
           (mark t))
      (delete-region (point) (mark)))
  (yank arg))
   
(defun pc-kill-ring-save-deactivates-mark (arg)
  "Controls whether or not saving the region as kill (C-insert or M-w) deactivates the mark.
With argument nil (no prefix argument), toggles the state. When called with argument
whose numeric value is positive (such as prefix C-u or C-u 42), saving the region as kill
deactivates the mark. When called with argument whose numeric value is non-positive (such
as prefix C-u 0 or C-u -1), saving the region as kill does not deactivate the mark."
  (interactive "P")
  (if arg
      (if (> (prefix-numeric-value arg) 0)
          (setq pc-kill-ring-save-deactivates-mark-state t)
        (setq pc-kill-ring-save-deactivates-mark-state nil))
    (setq pc-kill-ring-save-deactivates-mark-state
          (not pc-kill-ring-save-deactivates-mark-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Query replace should show the current active region, if any, as the
;; default for the string that is to be replaced.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc-query-replace-read-args (string)
  "Modified version of query-replace-read-args for pc-mode"
  (let (from to from-initial-value)

    ; The if-condition is defensive. Why take chances.
    (if (and (mark t)
             mark-active)
        (setq from-initial-value (buffer-substring (point) (mark)))
      (setq from-initial-value (car search-ring)))
    
    (setq from (read-from-minibuffer (format "%s: " string)
                                     from-initial-value
                                     nil
                                     nil
                                     query-replace-from-history-variable
                                     nil
                                     t))
  
    (setq to (read-from-minibuffer (format "%s %s with: " string from)
                                   nil
                                   nil
                                   nil
                                   query-replace-to-history-variable
                                   from
                                   t
                                   ))
    
    (list from to current-prefix-arg nil nil)))

(defun pc-query-replace (from-string to-string &optional delimited start end)
  "Very similar to query replace, with prompting for user input slightly modified."
  (interactive (pc-query-replace-read-args "Query replace"))

  ; The if-condition is defensive. Why take chances.
  (if (and (mark t)
           mark-active
           (< (mark) (point)))
      (exchange-point-and-mark))

  (perform-replace from-string
                   to-string
                   t
                   nil
                   delimited
                   nil
                   nil
                   (point)
                   (point-max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-mark-here-if-not-active ()
  "Sets the mark at point if it is not active."
  (if (not mark-active)
      (set-mark (point))))

