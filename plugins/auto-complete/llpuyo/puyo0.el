(require 'gamegrid)

(defvar puyo-width 6
  "プレイ領域の幅")

(defvar puyo-height 12
  "プレイ領域の高さ")

(defvar puyo-buffer-width 30
  "ゲーム領域の幅")

(defvar puyo-buffer-height 22
  "ゲーム領域の高さ")

(defvar puyo-top-left-x 3
  "ゲーム領域からプレイ領域までの X")

(defvar puyo-top-left-y 1
  "ゲーム領域からプレイ領域までの Y")

(defvar puyo-xpm "\
/* XPM */
static char *noname[] = {
/* columns rows colors chars-per-pixel */
\"16 16 16 1\",
\"  c black\",
\". c col1\",
\"X c col2\",
\"o c col3\",
\"O c col4\",
\"+ c col7\",
\"@ c col8\",
\"# c col9\",
\"$ c col0\",
\"% c gray100\",
\"& c gray100\",
\"* c gray100\",
\"= c gray100\",
\"- c gray100\",
\"; c gray100\",
\": c gray100\",
/* pixels */
\"$$$$$$$$$$$$$$$$\",
\"$$$       $$$$$$\",
\"$$ X+++OoX  $$$$\",
\"$ o+##@+OoXX $$$\",
\" X+##@Oo   XX $$\",
\" o+#@Oo ### oX $\",
\" oo+    # # oo $\",
\" Xo ### ### oOX \",
\"$ X # #    oO+X \",
\" XX ### oooO++X \",
\" Xo.   ooO++@+X \",
\" XoooooO+@@@+o $\",
\"$ .oO++@@@@+oX $\",
\"$$  oO++++oX  $$\",
\"$$$$        $$$$\",
\"$$$$$$$$$$$$$$$$\"
};
"
  "ぷよのスプライト")

(defconst puyo-blank 0
  "何もないことを意味する色")

(defconst puyo-border 8
  "ゲーム領域の境界の色")

(defconst puyo-space 9
  "")

(defvar puyo-x-colors
  [nil [0.8 0.3 0.3] [0.6 0.4 0.6] [0.9 0.9 0.3] [0.3 0.8 0.3] [0.8 0.3 0.9] [0.8 0.8 0.8]]
  "RGB での色指定。インデックス 0 は無色。")

(defvar puyo-next-width 3
  "ネクストぷよを表示する領域の幅")

(defvar puyo-next-height 8
  "ネクストぷよを表示する領域の高さ")

(defvar puyo-next-x (+ (* 2 puyo-top-left-x) puyo-width)
  "ネクストぷよを表示する領域の X")

(defvar puyo-next-y puyo-top-left-y
  "ネクストぷよを表示する領域の Y")

(defvar puyo-drop 0
  "ぷよの落下回数")

(defvar puyo-default-drop-speed 0.5
  "ぷよの落下速度")

(defvar puyo-default-tick-period 0.1
  "ぷよぷよのすすむ速度")



;;;; 実装

(defun puyo-init ()
  "初期化する"
  (puyo-hack-gamegrid)
  (setq gamegrid-user-glyphs t)
  (setq gamegrid-use-colors t)
  (gamegrid-init (puyo-display-options))
  (add-hook 'kill-buffer-hook 'puyo-cleanup nil t)
  (puyo-cleanup))

(defun puyo-cleanup ()
  "クリーンアップする"
  (gamegrid-kill-timer))

(defun puyo-init-buffer ()
  "プレイ領域を初期化する"
  (gamegrid-init-buffer puyo-buffer-width
                        puyo-buffer-height
                        puyo-space)
  (let ((buffer-read-only nil))
    (dotimes (y puyo-height)
      (dotimes (x puyo-width)
        (gamegrid-set-cell (+ puyo-top-left-x x)
                           (+ puyo-top-left-y y)
                           puyo-blank)))))



;;;; ここから先は読む必要なし

(defvar puyo-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar puyo-cell-options
  '(((glyph colorize)
     (emacs-tty ?\0)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty))))

(defvar puyo-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar puyo-space-options
  '(((t ?\040))
    nil
    nil))
  
(defun puyo-hack-gamegrid ()
  (if (and (fboundp 'window-system)
            (eq (window-system) 'x))
      (defun gamegrid-colorize-glyph (color)
        (find-image `((:type xpm :data ,puyo-xpm
                             :ascent center
                             :color-symbols
                             (("col0" . ,(gamegrid-color color 0))
                              ("col1" . ,(gamegrid-color color 0.4))
                              ("col2" . ,(gamegrid-color color 0.6))
                              ("col3" . ,(gamegrid-color color 0.8))
                              ("col4" . ,(gamegrid-color color 1.0))
                              ("col7" . ,(gamegrid-color color 1.0))
                              ("col8" . ,(gamegrid-color color 1.0))
                              ("col9" . ,(gamegrid-color color 1.0))))
                      (:type xbm :data ,gamegrid-xbm
                             :ascent center
                             :foreground ,(gamegrid-color color 1.0)
                             :background ,(gamegrid-color color 0.5)))))))

(defun puyo-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
            (cond ((= c puyo-blank)
                   puyo-blank-options)
                  ((and (>= c 1) (<= c 6))
                   (append
                    puyo-cell-options
                    `((((glyph color-x) ,(aref puyo-x-colors c))
                       (t nil)))))
                  ((= c puyo-border)
                   puyo-border-options)
                  ((= c puyo-space)
                   puyo-space-options)
                  (t
                   '(nil nil nil)))))
    options))
