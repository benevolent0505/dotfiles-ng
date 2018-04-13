(setq inhibit-startup-screen t)           ;; startup 非表示
(setq initial-scratch-message "")         ;; scratchの初期メッセージ消去
(tool-bar-mode -1)                        ;; ツールバー非表示
(menu-bar-mode -1)                        ;; メニューバー非表示
(set-scroll-bar-mode nil)                 ;; スクロールバー非表示
(global-linum-mode t)                     ;; 行番号を表示する
(column-number-mode t)                    ;; カラム番号を表示
(show-paren-mode t)                       ;; 対応する括弧を表示
(setq-default line-spacing 0)             ;; 行間を0に
(setq-default show-trailing-whitespace t) ;; 行末の空白を強制表示
(setq indicate-buffer-boundaries 'right)

;; scroll setting
(setq scroll-conservatively 1
      scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 5)

;; cursor setting
(blink-cursor-mode 0)

(el-get-bundle! zk-phi/sky-color-clock
  (sky-color-clock-initialize 35)
  (setq sky-color-clock-format "%H:%M")
  (setq sky-color-clock-enable-emoji-icon t)
  (push '(:eval (sky-color-clock)) (default-value 'mode-line-format)))

(el-get-bundle bbatsov/zenburn-emacs
  (add-to-list 'custom-theme-load-path (locate-user-emacs-file "el-get/zenburn-emacs"))
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg+05" . "#282828")
      ("zenburn-bg+1"  . "#2F2F2F")
      ("zenburn-bg+2"  . "#3F3F3F")
      ("zenburn-bg+3"  . "#4F4F4F")))
  (load-theme 'zenburn t))
