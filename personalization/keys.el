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

;; Org

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; I use C-c r to start org-remember
(global-set-key (kbd "C-c r") 'org-remember)

