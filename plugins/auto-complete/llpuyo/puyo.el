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



;; 1

(defvar puyo-shape nil)

(defun puyo ()
  "ぷよぷよをはじめる"
  (interactive)
  (switch-to-buffer "*Puyo*")
  (puyo-mode)
  (puyo-start-game))

(defun puyo-mode ()
  "ぷよぷよモード"
  (interactive)
  (setq major-mode 'puyo-mode)
  (setq mode-name "Puyo")
  (puyo-init))

(defun puyo-start-game ()
  "ぷよぷよをはじめる"
  (interactive)
  (puyo-init-buffer)
  (gamegrid-start-timer puyo-default-tick-period 'puyo-update-game)
  (puyo-new-shape))

(defun puyo-update-game (puyo-buffer)
  "ゲームをすすめる"
  (let (hit)
    (setq puyo-drop (+ puyo-drop (/ puyo-default-tick-period puyo-default-drop-speed)))
    (when (>= puyo-drop 1)
      (puyo-erase-shape)
      (setq puyo-pos-y (1+ puyo-pos-y))
      (let ((hit (puyo-test-shape)))
        (if hit
            (setq puyo-pos-y (1- puyo-pos-y))
          (setq puyo-drop 0))
        (puyo-draw-shape)
        (if (and hit (>= puyo-drop 2))
            (puyo-new-shape))))))

(defun puyo-get-cell (x y)
  "ぷよを取得する"
  (gamegrid-get-cell (+ puyo-top-left-x x)
                     (+ puyo-top-left-y y)))

(defun puyo-set-cell (x y color)
  "ぷよを設定する"
  (gamegrid-set-cell (+ puyo-top-left-x x)
                     (+ puyo-top-left-y y)
                     color))

(defun puyo-get-shape-cell (x y)
  (aref puyo-shape (+ (* y 3) x)))

(defun puyo-draw-shape ()
  "ぷよを描画する"
  (dotimes (y 3)
    (dotimes (x 3)
      (let ((c (puyo-get-shape-cell x y)))
        (if (/= c puyo-blank)
            (puyo-set-cell (+ puyo-pos-x x) (+ puyo-pos-y y) c))))))

(defun puyo-erase-shape ()
  "ぷよを消去する"
  (dotimes (y 3)
    (dotimes (x 3)
      (let ((c (puyo-get-shape-cell x y)))
        (if (/= c puyo-blank)
            (puyo-set-cell (+ puyo-pos-x x) (+ puyo-pos-y y) puyo-blank))))))

(defun puyo-make-shape ()
  "ぷよを作る"
  (vector 0 (1+ (random 4)) 0
          0 (1+ (random 4)) 0
          0 0               0))

(defun puyo-new-shape ()
  "次のぷよを出す"
  (setq puyo-pos-x (- (floor (/ puyo-width 2)) 2))
  (if (/= (puyo-get-cell (+ puyo-pos-x 1)
                         0)
          puyo-blank)
      (puyo-end-game)
    (setq puyo-pos-y 0)
    (setq puyo-shape (puyo-make-shape))
    (puyo-draw-shape)))

(defun puyo-end-game ()
  "ゲームオーバー"
  (interactive)
  (puyo-cleanup)
  (message "ゲームオーバー"))

(defun puyo-test-shape()
  "ぷよが衝突したか"
  (let ((hit nil))
    (dotimes (y 3)
      (dotimes (x 3)
        (unless hit
          (setq hit
                (let ((c (puyo-get-shape-cell x y))
                      (xx (+ puyo-pos-x x))
                      (yy (+ puyo-pos-y y)))
                  (and (/= c puyo-blank)
                       (if (< yy 0)
                           (or (< xx 0)
                               (>= xx puyo-width))
                         (or (>= xx puyo-width)
                             (>= yy puyo-height)
                             (/= (puyo-get-cell xx yy)
                                 puyo-blank)))))))))
    hit))



;; 2

(defvar puyo-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "x" 'puyo-move-down)
    (define-key keymap "z" 'puyo-move-left)
    (define-key keymap "c" 'puyo-move-right)
    (define-key keymap "v" 'puyo-rotate-left)
    (define-key keymap "b" 'puyo-rotate-right)
    keymap))

(defun puyo-mode ()
  "ぷよぷよモード"
  (interactive)
  (setq major-mode 'puyo-mode)
  (setq mode-name "Puyo")
  (puyo-init)
  (use-local-map puyo-mode-map))

(defun puyo-rotate-left ()
  "ぷよを反時計回りに回す"
  (interactive)
  (let ((old puyo-shape))
    (puyo-erase-shape)
    (setq puyo-shape
          (vector (aref puyo-shape 2) (aref puyo-shape 5) (aref puyo-shape 8)
                  (aref puyo-shape 1) (aref puyo-shape 4) (aref puyo-shape 7)
                  (aref puyo-shape 0) (aref puyo-shape 3) (aref puyo-shape 6)))
    (if (puyo-test-shape)
        (setq puyo-shape old))
    (puyo-draw-shape)))

(defun puyo-rotate-right ()
  "ぷよを時計回りに回す"
  (interactive)
  (let ((old puyo-shape))
    (puyo-erase-shape)
    (setq puyo-shape
          (vector (aref puyo-shape 6) (aref puyo-shape 3) (aref puyo-shape 0)
                  (aref puyo-shape 7) (aref puyo-shape 4) (aref puyo-shape 1)
                  (aref puyo-shape 8) (aref puyo-shape 5) (aref puyo-shape 2)))
    (if (puyo-test-shape)
        (setq puyo-shape old))
    (puyo-draw-shape)))

(defun puyo-move-left ()
  "ぷよを左に動かす"
  (interactive)
  (puyo-erase-shape)
  (setq puyo-pos-x (1- puyo-pos-x))
  (if (puyo-test-shape)
      (setq puyo-pos-x (1+ puyo-pos-x)))
  (puyo-draw-shape))

(defun puyo-move-right ()
  "ぷよを右に動かす"
  (interactive)
  (puyo-erase-shape)
  (setq puyo-pos-x (1+ puyo-pos-x))
  (if (puyo-test-shape)
      (setq puyo-pos-x (1- puyo-pos-x)))
  (puyo-draw-shape))

(defun puyo-move-down ()
  "ぷよを下に動かす"
  (interactive)
  (puyo-erase-shape)
  (setq puyo-pos-y (1+ puyo-pos-y))
  (if (puyo-test-shape)
      (progn
        (setq puyo-pos-y (1- puyo-pos-y))
        (puyo-draw-shape)
        (puyo-new-shape))
    (puyo-draw-shape)))



;; 3

(defvar puyo-task nil
  "実行中のタスク")

(defun puyo-move-down ()
  "ぷよを下に動かす"
  (interactive)
  (puyo-erase-shape)
  (setq puyo-pos-y (1+ puyo-pos-y))
  (if (puyo-test-shape)
      (progn
        (setq puyo-pos-y (1- puyo-pos-y))
        (puyo-draw-shape)
        (puyo-shape-done))
    (puyo-draw-shape)))

(defun puyo-update-game (puyo-buffer)
  "ゲームをすすめる"
  (let (hit)
    (setq puyo-drop (+ puyo-drop (/ puyo-default-tick-period puyo-default-drop-speed)))
    (when (>= puyo-drop 1)
      (cond
       ((eq puyo-task 'drop)
        (puyo-drop)
        (setq puyo-task 'chain)
        (setq puyo-drop 0))
       ((eq puyo-task 'chain)
        (let ((count 0))
          (dotimes (y puyo-height)
            (dotimes (x puyo-width)
              (let ((c (puyo-chain x y nil)))
                (message "%s" c)
                (when (>= c 4)
                  (puyo-chain x y puyo-blank)
                  (setq count (1+ count))))))
          (if (> count 0)
              (setq puyo-task 'drop)
            (setq puyo-task nil)
            (puyo-new-shape)))
        (setq puyo-drop 0))
       (t
        (puyo-erase-shape)
        (setq puyo-pos-y (1+ puyo-pos-y))
        (let ((hit (puyo-test-shape)))
          (if hit
              (setq puyo-pos-y (1- puyo-pos-y))
            (setq puyo-drop 0))
          (puyo-draw-shape)
          (if (and hit (>= puyo-drop 2))
              (puyo-shape-done))))))))

(defun puyo-shape-done ()
  "ぷよが接地した"
  (setq puyo-task 'drop))

(defun puyo-drop ()
  "浮いてるぷよを落とす"
  (dotimes (y (1- puyo-height))
    (dotimes (x puyo-width)
      (let* ((yy (- puyo-height y))
             (c (puyo-get-cell x yy)))
        (when (/= c puyo-blank)
          (while (= (puyo-get-cell x (1+ yy)) puyo-blank)
            (setq yy (1+ yy)))
          (puyo-set-cell x (- puyo-height y) puyo-blank)
          (puyo-set-cell x yy c))))))

(defun puyo-chain-1 (x y c color footprint)
  (if (or (< x 0)
          (< y 0)
          (>= x puyo-width)
          (>= y puyo-height)
          (= (aref footprint (+ x (* y puyo-width))) 1))
      0
    (let ((d (puyo-get-cell x y)))
      (if (= c d)
          (progn
            (aset footprint (+ x (* y puyo-width)) 1)
            (if color
                (puyo-set-cell x y color))
            (+ (puyo-chain-1 (1- x) y      c color footprint)
               (puyo-chain-1 x      (1- y) c color footprint)
               (puyo-chain-1 (1+ x) y      c color footprint)
               (puyo-chain-1 x      (1+ y) c color footprint)
               1))
        0))))
  
(defun puyo-chain (x y color)
  "連鎖する"
  (let ((c (puyo-get-cell x y)))
    (if (/= c puyo-blank)
        (let ((footprint (make-vector (* puyo-width puyo-height) 0)))
          (+ (puyo-chain-1 (1- x) y      c color footprint)
             (puyo-chain-1 x      (1- y) c color footprint)
             (puyo-chain-1 (1+ x) y      c color footprint)
             (puyo-chain-1 x      (1+ y) c color footprint)
             ))
      0)))


(defun puyo-start-game ()
  "ぷよぷよをはじめる"
  (interactive)
  (puyo-init-buffer)
  (setq puyo-shape nil)
  (setq puyo-task nil)
  (setq puyo-drop 0)
  (gamegrid-start-timer puyo-default-tick-period 'puyo-update-game)
  (puyo-new-shape))



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
  (if (eq (window-system) 'x)
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
