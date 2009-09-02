(defvar jc-package-regexp "^\\s *package \\(.+\\)\\s *;")
(defvar jc-import-regexp "^\\s *import \\(.+\\)\\s *;")
(defvar jc-class-regexp "^\\s *\\(\\(?:public\\|private\\|static\\|final\\|abstract\\)\\s +\\)*class\\s +\\(\\w+\\)\\(\\s +.+?\\)?\\s *{")
(defvar jc-method-regexp "^\\s *\\(\\(?:\\(?:public\\|protected\\|private\\|static\\|final\\)\\s +\\)*\\w.*?\\s +\\(\\w+\\)\\)\\s *(\\([^)]*?\\))\\s *\\(.*?\\)\\s *{")
(defvar jc-variable-regexp "^\\s *\\(\\(?:\\(?:public\\|protected\\|private\\|static\\|final\\|volatile\\|transient\\)\\s +\\)*\\(\\w.*?\\)\\)\\s +\\(\\w+\\)\\s *\\(?:;\\|=\\)")
(defvar jc-classpath (list (format "%s/libexec" (getenv "HOME"))))

(defun jc-analyze-current-context (point)
  (let (package
        imports
        classes
        variables
        prefix
        (syntax-table (let ((syntax-table (copy-syntax-table (syntax-table))))
                        (modify-syntax-entry ?< "(>" syntax-table)
                        (modify-syntax-entry ?> ")<" syntax-table)
                        syntax-table)))
    (with-syntax-table syntax-table
      (save-excursion
        ;; scan package
        (goto-char (point-min))
        (if (re-search-forward jc-package-regexp nil t)
            (setq package (match-string-no-properties 1)))

        ;; scan imports
        (goto-char (point-min))
        (while (re-search-forward jc-import-regexp nil t)
          (push (match-string-no-properties 1) imports))

        ;; scan inner most method
        (goto-char point)
        (when (re-search-backward jc-method-regexp nil t)
          (let ((indent (save-excursion
                          (goto-char (match-beginning 1))
                          (current-indentation)))
                (end (ignore-errors (save-excursion
                                      (goto-char (1- (match-end 0)))
                                      (forward-list)
                                      (point)))))
            (if (and indent end (< point end))
                (while (re-search-forward jc-variable-regexp end t)
                  (if (>= (+ 4 indent)
                          (save-excursion
                            (goto-char (match-beginning 1))
                            (current-indentation)))
                      (push (list (match-string-no-properties 3)
                                  (match-string-no-properties 2))
                            variables))))))
      
        ;; scan classes
        (goto-char point)
        (while (re-search-backward jc-class-regexp nil t)
          (let ((indent (save-excursion
                          (goto-char (match-beginning 1))
                          (current-indentation)))
                (end (ignore-errors (save-excursion
                                      (goto-char (1- (match-end 0)))
                                      (forward-list)
                                      (point)))))
            (when (and indent end (< point end))
              (push (list (match-string-no-properties 2) (match-string-no-properties 3))
                    classes)
              (save-excursion
                (while (re-search-forward jc-variable-regexp end t)
                  (if (>= (+ 4 indent)
                          (save-excursion
                            (goto-char (match-beginning 1))
                            (current-indentation)))
                      (push (list (match-string-no-properties 3)
                                  (match-string-no-properties 2))
                            variables)))))))

        ;; scan prefix
        (goto-char point)
        (ignore-errors
          (while (or (null prefix) (re-search-backward "\\.\\s *\\=" nil t))
            (let ((end (point)))
              (while (re-search-backward "[[:alnum:]>)]\\=" nil t)
                (goto-char (match-end 0))
                (backward-sexp))
              (push (buffer-substring-no-properties (point) end) prefix))))
        
        (list package imports classes variables prefix)))))

(defun jc-get-completions (context)
  (let ((package (nth 0 context))
        (imports (nth 1 context))
        (classes (nth 2 context))
        (variables (nth 3 context))
        (prefix (nth 4 context))
        class-info)
    (dolist (p prefix)
      (let ((type (assoc-default p variables)))
        (if type
            (setq class-info (jc-get-class-info imports type)))))
    (jc-class-info->completions class-info)))
          
(defun jc-get-class-info (imports classes)
  (ignore-errors
    (car-safe
     (read-from-string
      (shell-command-to-string
       (format "java -cp \"%s\" JavaComplete -i \"%s\" \"%s\""
               (mapconcat 'identity jc-classpath ":")
               (mapconcat 'identity imports ":")
               (mapconcat 'identity classes "\" \"")))))))

(defun jc-class-info->completions (class-info)
  (if (and class-info
           (> (length class-info) 0))
      (apply
       'append
       (mapcar (lambda (info)
                 (append (nth 1 info) (nth 2 info)))
               class-info))))


(defun jc-get-eclipse-classpath ()
  (let ((dir default-directory)
        parent-dir
        file
        classpath)
    (while (and dir
                (setq parent-dir (file-name-directory (directory-file-name dir)))
                (not (equal dir parent-dir)))
      (setq file (format "%s/.classpath" dir))
      (if (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "kind=\"lib\"[^>]+?path=\"\\([^\"]+\\)\"" nil t)
              (push (concat dir (match-string 1)) classpath))
            (setq dir nil))
        (setq dir parent-dir)))
    (nreverse classpath)))

(defvar ac-java-method-candidates nil)
(defvar ac-source-java-method
  '((init . (lambda ()
              ;; resolve class path
              (unless (local-variable-p 'jc-classpath)
                (make-local-variable 'jc-classpath)
                (setq jc-classpath (append jc-classpath (jc-get-eclipse-classpath))))
              
              ;; cache candidates
              (setq ac-java-method-candidates
                    (jc-get-completions
                     (jc-analyze-current-context
                      (point))))))
    (candidates . (lambda () (all-completions ac-prefix ac-java-method-candidates)))))

(defvar ac-java-omni-completion-sources
  '(("\\.\\=" ac-source-java-method)))

(provide 'java-complete)
