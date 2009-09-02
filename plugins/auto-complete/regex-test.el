;; -*-coding:utf-8-*-

(defvar test-counter 0)

(defmacro test (&rest form)
  `(princ-list (format "%d ... " (setq test-counter (1+ test-counter)))
               (condition-case nil
                   (if (progn ,@form) 'ok 'fail)
                 (error 'invalid))))

(defun expect-invalid (regexp)
  (test (condition-case nil
            (prog1 nil (string-match regexp ""))
          (error t))))

(defun expect-match (regexp string &optional group-number group-string)
  (test (and (string-match regexp string)
             (if group-number
                 (equal (match-string group-number string) group-string)
               t))))

(defun expect-not-match (regexp string)
  (test (not (string-match regexp string))))

(expect-match "\\(?=\\)" "")
(expect-not-match "\\(?=a\\)" "")
(expect-match "a\\(?=b\\)b" "ab")
(expect-not-match "a\\(?=b\\)c" "ab")
(expect-match "\\(?=a\\)a" "a")
(expect-not-match "\\(?=b\\)a" "a")
(expect-match "\\(?=^\\)a" "a")
(expect-match "a\\(?=$\\)$" "a")
(expect-match "a\\(?=\\)$" "a")
(expect-match "a\\(?=.*c\\)b" "abc")
(expect-not-match "a\\(?=.*d\\)b" "abc")
(expect-match "a\\(?=b\\|c\\|d\\|e\\)" "ae")
(expect-not-match "a\\(?=b\\|c\\|d\\|e\\)" "af")
(expect-match "a\\(?=\\(b\\)\\)b" "ab" 1 "b")
(expect-match "a\\(\\(?=b\\)\\)" "ab" 1 "")
(expect-match "a\\(?=\\(b\\)\\)" "ab" 1 "b")
(expect-match "\\(a\\(?=\\(b\\)\\)\\2\\)\\1" "abab" 1 "ab")
(expect-not-match "\\(a\\)\\(?=\\(b\\)\\)\\1" "ab")
(expect-match "\\(a\\(?=b\\(?=c\\)\\)\\)" "abc" 1 "a")
(expect-not-match "\\(a\\(?=b\\(?=c\\)\\)\\)" "abd")
(expect-not-match "\\(?!\\)" "")
(expect-match "\\(?!a\\)" "")
(expect-not-match "a\\(?!b\\)b" "ab")
(expect-match "a\\(?!b\\)c" "ac")
(expect-not-match "\\(?!a\\)a" "a")
(expect-match "\\(?!b\\)a" "a")
(expect-match "\\(?!^\\)a" "ba")
(expect-not-match "\\(?!^\\)a" "a")
(expect-not-match "a\\(?!$\\)$" "a")
(expect-not-match "a\\(?!\\)$" "a")
(expect-not-match "a\\(?!.*c\\)b" "abc")
(expect-match "a\\(?!.*d\\)b" "abc")
(expect-not-match "a\\(?!b\\|c\\|d\\|e\\)" "ae")
(expect-match "a\\(?!b\\|c\\|d\\|e\\)" "af")
(expect-match "a\\(?!\\(b\\)\\)c" "ac")
(expect-match "a\\(\\(?!b\\)\\)" "ac")
(expect-match "a\\(?!b\\(?!c\\)\\)" "abc")
(expect-not-match "a\\(?!b\\(?=\\(c\\)\\)\\)" "abc")
(expect-not-match "a\\(?!b\\(?!c\\)\\)" "abd")
(expect-match "\\(?<=\\)" "")
(expect-not-match "\\(?<=a\\)" "")
(expect-match "\\(?<=a\\)" "a")
(expect-not-match "\\(?<=b\\)" "a")
(expect-match "\\(?<=^\\)" "")
(expect-not-match "a\\(?<=^\\)" "")
(expect-match "\\(?<=$\\)" "")
(expect-not-match "\\(?<=$\\)a" "")
(expect-match "\\(?<=a\\)b" "ab")
(expect-not-match "\\(?<=c\\)b" "ab")
(expect-match "\\(?<=\\(?<=a\\)\\)b" "ab")
(expect-not-match "\\(?<=\\(?<=b\\)\\)b" "ab")
(expect-match "\\(?<=\\(?=a\\).\\)b" "ab")
(expect-match "\\(?<=\\(a\\)\\)b\\1" "aba" 1 "a")
(expect-match "\\(?<=.\\)a" "aa")
(expect-match "\\(?<=\\(.\\)\\)a" "aa")
(expect-match "\\(?<=\\w\\)a" "aa")
(expect-not-match "\\(?<=\\w\\)a" "!a")
(expect-match "\\(?<=\\sw\\)a" "aa")
(expect-not-match "\\(?<=\\sw\\)a" "!a")
(expect-match "\\(?<=\\cg\\)a" "λa")
(expect-not-match "\\(?<=\\Cg\\)a" "λa")
(expect-match "\\(?<=[a-z]\\)" "aa")
(expect-not-match "\\(?<=[a-z]\\)a" "1a")
(expect-match "\\(?<=[^a-z]\\)" "1a")
(expect-not-match "\\(?<=[^a-z]\\)" "aa")
(expect-match "\\(?<=[:ascii:]\\)a" "aa")
(expect-match "\\(?<=\\`\\)" "")
(expect-not-match "a\\(?<=\\`\\)" "a")
(expect-match "\\(?<=\\'\\)" "")
(expect-not-match "\\(?<=\\'\\)a" "a")
(expect-not-match "\\(?<=\\=\\)" "")
(expect-match "\\(?<=\\b\\)a" "a")
(expect-not-match "a\\(?<=\\b\\)b" "ab")
(expect-match "\\(?<=\\B\\)a" "aa")
(expect-not-match "\\(?<=\\B\\)a" " a")
(expect-match "\\(?<=\\<\\)a" "a")
(expect-not-match "a\\(?<=\\<\\)b" "ab")
(expect-match "a\\(?<=\\>\\)" "a")
(expect-not-match "a\\(?<=\\>\\)b" "ab")
(expect-match "\\(?<=\\_<\\)a" "a")
(expect-not-match "a\\(?<=\\_<\\)b" "ab")
(expect-match "a\\(?<=\\_>\\)" "a")
(expect-not-match "a\\(?<=\\_>\\)b" "ab")
(expect-invalid "\\(?<=\\(.\\)\\1\\)")  ; duplicate
(expect-invalid "\\(?<=a*\\)")          ; variable width
(expect-invalid "\\(?<=a*?\\)")         ; variable width
(expect-invalid "\\(?<=a+\\)")          ; variable width
(expect-invalid "\\(?<=a+?\\)")         ; variable width
(expect-invalid "\\(?<=a?\\)")          ; variable width
(expect-invalid "\\(?<=a??\\)")         ; variable width
(expect-invalid "\\(?<=a\\{1,4\\}\\)")  ; variable width
(expect-invalid "\\(?<=a\\|bb\\|ccc\\)") ; variable width
(expect-invalid "\\(?<=a\\{4\\}\\)")   ; fixed width but not supported yet
(expect-invalid "\\(?<=a\\|\\b\\c\\)")   ; fixed width but not supported yet
(expect-not-match "\\(?<!\\)" "")
(expect-match "\\(?<!a\\)" "")
(expect-match "\\(?<!a\\)" "a")
(expect-not-match "\\(?<!a\\)b" "ab")
(expect-match "\\(?<!b\\)" "a")
(expect-not-match "\\(?<!^\\)" "")
(expect-not-match "a\\(?<!^\\)" "")
(expect-not-match "\\(?<!$\\)" "")
(expect-match "\\(?<=a\\)b" "ab")
(expect-match "\\(?<!c\\)b" "ab")
(expect-match "\\(?<!\\(?<!a\\)\\)b" "ab")
(expect-not-match "\\(?<!\\(?<!b\\)\\)b" "ab")
(expect-match "\\(?<!\\(?!a\\).\\)b" "ab")
(expect-match "\\(?<!.\\)a" "aa")
(expect-not-match "\\(?<!.\\)b" "ab")
(expect-not-match "\\(?<!\\(.\\)\\)b" "ab")
(expect-not-match "\\(?<!\\w\\)b" "ab")
(expect-not-match "\\(?<!\\w\\)b" "ab")
(expect-not-match "\\(?<!\\sw\\)b" "ab")
(expect-match "\\(?<!\\sw\\)a" "!a")
(expect-not-match "\\(?<!\\cg\\)a" "λa")
(expect-match "\\(?<!\\Cg\\)a" "λa")
(expect-match "\\(?<![a-z]\\)" "aa")
(expect-match "\\(?<![a-z]\\)a" "1a")
(expect-not-match "\\(?<![^a-z]\\)a" "1a")
(expect-not-match "\\(?<![:ascii:]\\)b" "ab")
(expect-not-match "\\(?<!\\`\\)" "")
(expect-match "a\\(?<!\\`\\)" "a")
(expect-not-match "\\(?<!\\'\\)" "")
(expect-match "\\(?<!\\'\\)a" "a")
(expect-match "\\(?<!\\=\\)" "")
(expect-not-match "\\(?<!\\b\\)a" "a")
(expect-match "a\\(?<!\\b\\)b" "ab")
(expect-not-match "\\(?<!\\B\\)b" "ab")
(expect-match "\\(?<!\\B\\)a" " a")
(expect-not-match "\\(?<!\\<\\)a" "a")
(expect-match "a\\(?<!\\<\\)b" "ab")
(expect-not-match "a\\(?<!\\>\\)" "a")
(expect-match "a\\(?<!\\>\\)b" "ab")
(expect-not-match "\\(?<!\\_<\\)a" "a")
(expect-match "a\\(?<!\\_<\\)b" "ab")
(expect-not-match "a\\(?<!\\_>\\)" "a")
(expect-match "a\\(?<!\\_>\\)b" "ab")
(expect-invalid "\\(?<!\\(.\\)\\1\\)")  ; duplicate
(expect-invalid "\\(?<!a*\\)")          ; variable width
(expect-invalid "\\(?<!a*?\\)")         ; variable width
(expect-invalid "\\(?<!a+\\)")          ; variable width
(expect-invalid "\\(?<!a+?\\)")         ; variable width
(expect-invalid "\\(?<!a?\\)")          ; variable width
(expect-invalid "\\(?<!a??\\)")         ; variable width
(expect-invalid "\\(?<!a\\{1,4\\}\\)")  ; variable width
(expect-invalid "\\(?<!a\\|bb\\|ccc\\)") ; variable width
(expect-invalid "\\(?<!a\\{4\\}\\)")   ; fixed width but not supported yet
(expect-invalid "\\(?<!a\\|\\b\\c\\)")   ; fixed width but not supported yet



;; japanese

;; invalid syntax
;; best start

;; uncomment DEBUG


;; enabled when shy groups

;; isearch bound
