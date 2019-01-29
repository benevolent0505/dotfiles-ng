(setq inhibit-startup-screen t)                  ;; startup 非表示
(setq initial-scratch-message "")                ;; scratchの初期メッセージ消去
(tool-bar-mode -1)                               ;; ツールバー非表示
(menu-bar-mode -1)                               ;; メニューバー非表示
(set-scroll-bar-mode nil)                        ;; スクロールバー非表示
(column-number-mode t)                           ;; カラム番号を表示
(show-paren-mode t)                              ;; 対応する括弧を表示
(setq line-spacing 0)                            ;; 行間を0に
(setq show-trailing-whitespace t)                ;; 行末の空白を強制表示
(electric-pair-mode t)                           ;;
(blink-cursor-mode 0)                            ;;
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq-default auto-fill-mode -1)

(set-frame-parameter nil 'alpha 90)              ;;
(set-frame-parameter nil 'fullscreen 'maximized) ;;

;; scroll setting
(setq scroll-conservatively 1
      scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 5)

;; モードラインに時刻表示
(custom-set-variables
 '(display-time-interval 1)
 '(display-time-string-forms
   '((format "%s:%s:%s" 24-hours minutes seconds))))
(setq display-time-day-and-date t)
(display-time-mode t)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にSourceHanCodeJPを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;;英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "源ノ角ゴシック Code JP" ;;英数
                             :height 130)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "源ノ角ゴシック Code JP")))
        ((eq ws 'x)
         (set-face-attribute 'default nil
                             :family "源ノ角ゴシック Code JP"
                             :height 96)
          (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "源ノ角ゴシック Code JP")))))
