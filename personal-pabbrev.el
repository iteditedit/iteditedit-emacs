(require 'auto-complete)

(defun pabbrevx-ac-on-pre-command ()
  (if (or (eq this-command 'self-insert-command)
          (and (not (ac-trigger-command-p))
               (or (not (symbolp this-command))
                   (not (string-match "^ac-" (symbol-name this-command))))))
      (progn
        (remove-hook 'post-command-hook 'pabbrevx-ac-on-post-command t)
        (remove-hook 'pre-command-hook 'pabbrevx-ac-on-pre-command t)
        (ac-abort))))

(defun pabbrevx-ac-on-post-command ()
  (if (and (not isearch-mode)
           (ac-trigger-command-p))
      (pabbrevx-ac-start)))

(defun pabbrevx-ac-start ()
  (let ((candidates (mapcar 'car pabbrev-expansion-suggestions)))
    (add-hook 'pre-command-hook 'pabbrevx-ac-on-pre-command nil t)
    (add-hook 'post-command-hook 'pabbrevx-ac-on-post-command nil t)
    (let* ((point (save-excursion (funcall ac-prefix-function)))
           (reposition (not (equal ac-point point))))
      (if (null point)
          (ac-abort)
        (setq ac-point point)
        (when (not (equal ac-point ac-old-point))
          (setq ac-old-point point))
        (setq ac-prefix (buffer-substring-no-properties point (point)))
        (setq ac-limit ac-candidate-max)
        (if (or reposition (null ac-menu))
            (save-excursion
              (funcall ac-init-function)))
        (let* ((current-width (if ac-menu (ac-menu-width ac-menu) 0))
               (width (let ((w '(0)) s)
                        (dotimes (i ac-candidate-menu-height)
                          (setq s (nth i candidates))
                          (if (stringp s) (push (string-width s) w)))
                        (apply 'max w))))
          (if (or reposition
                  (null ac-menu)
                  (> width current-width)
                  (< width (- current-width 20)))
              (ac-setup point (* (ceiling (/ width 20.0)) 20)))
          (ac-update-candidates candidates))))))

(defun pabbrevx-suggestions-goto-buffer (suggestions)
  (pabbrevx-ac-start))

(fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer)

(global-pabbrev-mode)
