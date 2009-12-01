;; org-mode
(require 'org-install)
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.(org$|txt$)'" . org-mode))

;; turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; flyspell mode to spell check everywhere
            (flyspell-mode 1)))

;; Fast todo selection allows changing from any task todo state to any other state directly by selecting the appropriate key from the fast todo selection key menu. This is a great feature!
;; Changing a task state is done with [C-c C-t KEY]
(setq org-use-fast-todo-selection t)

;; allows changing todo states with S-left and S-right skipping all of the normal processing when entering or leaving a todo state. This cycles through the todo states but skips setting timestamps and entering notes which is very convenient when all you want to do is fix up the status of an entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;;    * Moving a task to CANCELLED adds a CANCELLED tag
;;    * Moving a task to WAITING adds a WAITING tag and removes any NEXT tag
;;    * Moving a task to SOMEDAY adds a WAITING tag
;;    * Moving a task to a done state removes NEXT and WAITING tags
;;    * Moving a task to TODO removes WAITING and CANCELLED tags
;;    * Moving a task to STARTED removes a WAITING tag and adds a NEXT tag

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t) ("NEXT"))
              ("SOMEDAY" ("WAITING" . t))
              (done ("NEXT") ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("STARTED" ("WAITING") ("NEXT" . t)))))

;; Refile setup

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only

;; custom agenda views
(setq org-agenda-custom-commands
      (quote (("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING/!" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("N" "Notes" tags "NOTE" nil)
              ("n" "Next" tags "NEXT-WAITING-CANCELLED/!" nil))))


; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00"))))

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

; Yes it's long... but more is better ;)
(setq org-clock-history-length 35)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to STARTED when clocking in
;;(setq org-clock-in-switch-to-state "STARTED")

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Don't clock out when moving task to a done state
(setq org-clock-out-when-done nil)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Remeber
(require 'remember)

;; http://www.emacswiki.org/emacs/RememberMode
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(org-remember-insinuate)
(setq org-default-notes-file "~/org/refile.org")

;; The following setting makes time editing round to 5 minute increments
(setq org-time-stamp-rounding-minutes (quote (1 5)))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items (quote (clock)))

; Tags with fast selection keys
(setq org-tag-alist (quote (("PERSONAL" . ?p)
                            ("FAMILY" . ?f)
                            ("UNIVERSITY" . ?u)
                            ("COMPANY" . ?c)
                            ("COUNTRY" . ?C)
                            ("WORLD" . ?W)
                            ("THINK" . ?z)
                            ("WORK" . ?w)
                            ("LEARN" . ?l)
                            ("MANAGE" . ?m)
                            ("TEACH" . ?t))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

; This setting prevents tasks from changing to DONE if any subtasks are still open.
(setq org-enforce-todo-dependencies t)

; The following setting hides all blank lines inside folded contents of a tasks:
(setq org-cycle-separator-lines 0)

; The following setting prevents creating blank lines before list items and headings:
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item))))


;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

(setq org-remember-default-headline "Notes")

;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates
      (quote
       (("todo" ?t "* TODO %? %u  %a" nil bottom nil)
	("note" ?n "* %? :NOTE: %u %a" nil bottom nil))))
