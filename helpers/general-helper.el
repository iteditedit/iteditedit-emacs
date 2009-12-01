(defun personalize (name)
  (load (path-to-personalized name)))

(defun path-to-personalized (name)
  (concat (concat config-dir "/personalization/") name ".el"))

(defun path-to-vendor (name)
  (concat (concat config-dir "/vendor/") name))

(defun path-to-vendor-lib (name)
  (concat (concat config-dir "/vendor/") name "/" name ".el" ))

(defun vendor (name &optional callback)
  (let ((path-to-vendor-name (path-to-vendor name))
	(path-to-personalized-name (path-to-personalized name)))
    (when (memq path-to-vendor-name load-path)
      (add-to-list 'load-path path-to-vendor-name))
    (when callback (funcall callback))
    (when (file-exists-p path-to-personalized-name)
      (load-file path-to-personalized-name))))

;; Duplicate one line
(defun duplicate-line ()
 "Duplicate it."
 (interactive)
 (let (
       (beg (line-beginning-position))
       (end (line-end-position)))
   (copy-region-as-kill beg end)
   (beginning-of-line)
   (forward-line 1)
   (yank)
   (newline)
   (forward-line -1)))

