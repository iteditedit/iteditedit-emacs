;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load a better way to assign multiple commands or controls to the
;; the same key bind.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'define-key-wise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set global key bindings. Compatibility built-ins if you will
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F key mappings
(global-set-key [f3] 'find-file)
(global-set-key [f4] 'set-mark-command)
(global-set-key [f6] 'save-buffer) ;; Save
;; Shift-F4 is "pop mark off of stack"
(global-set-key [(shift f4)] (lambda() (interactive) (set-mark-command t)))
;;(global-set-key [f8] 'start-kbd-macro)
;;(global-set-key [f9] 'end-kbd-macro)
;;(global-set-key [f10] 'call-last-kbd-macro)
;;(global-set-key [f12] 'menu-bar-mode)
(global-set-key [(shift f12)] 'tool-bar-mode)
(global-set-key [(control u)] 'undo)


(global-set-key "\C-t" 'winring-new-configuration)
(global-set-key "\C-f" 'find-file)
(global-set-key "\C-b" 'ecb-toggle-ecb-windows)
(global-set-key [C-M-next] 'winring-prev-configuration)
(global-set-key [C-M-prior] 'winring-next-configuration)
(global-set-key "\C-w" 'other-window)
(global-set-key [(control shift w)] 'other-frame)
(global-set-key [(control \;)] 'eval-buffer)
(global-set-key [(<C-escape>)] '(kill-buffer (current-buffer)))
(global-set-key "\C-x\C-\\" 'delete-other-windows-vertically)
(global-set-key "\M-\\" 'comment-or-uncomment-region)
(global-set-key "\C-d" 'delete-char)
(global-set-key [M-next] 'previous-buffer)
(global-set-key [M-prior] 'next-buffer)



;;(global-set-key [(control escape)] 'buffer-quick-kill)
;;(global-set-key [(control <up>)] 'next-buffer)
;;(global-set-key "\\d" 'delete-char)
;;(global-set-key [(<delete>)] 'delete-char)


;; Set behavioral bindings
;; TODO: Test for personal-behavior.el
(global-set-key "\M-5" 'match-parenthesis)

(global-set-key [(control \.)] 'shift-right)
(global-set-key [(control ,)] 'shift-left)

(global-set-key "\C-j" 'pull-next-line)
(global-set-key-wise "\C-o" 'end-of-line 'newline-and-indent)
(global-set-key "\C-m" 'newline-and-indent)

(global-set-key [C-delete] 'buffer-quick-kill)
