(setq inhibit-startup-screen t)           ;; startup 非表示
(setq initial-scratch-message "")         ;; scratchの初期メッセージ消去
(tool-bar-mode -1)                        ;; ツールバー非表示
(menu-bar-mode -1)                        ;; メニューバー非表示
(set-scroll-bar-mode nil)                 ;; スクロールバー非表示
(column-number-mode t)                    ;; カラム番号を表示
(show-paren-mode t)                       ;; 対応する括弧を表示
(setq-default line-spacing 0)             ;; 行間を0に
(setq-default show-trailing-whitespace t) ;; 行末の空白を強制表示
(electric-pair-mode t)

;; scroll setting
(setq scroll-conservatively 1
      scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 5)

;; cursor setting
(blink-cursor-mode 0)

(el-get-bundle rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

  ;; 括弧の色を強調する設定
  (require 'cl-lib)
  (require 'color)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (defvar rainbow-delimiters-max-face-count)
    (cl-loop
      for index from 1 to rainbow-delimiters-max-face-count
      do
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
        (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

;; モードラインに時刻表示
(custom-set-variables
  '(display-time-interval 1)
  '(display-time-string-forms
     '((format "%s:%s:%s" 24-hours minutes seconds))))
(setq display-time-day-and-date t)
(display-time-mode t)

(el-get-bundle bbatsov/zenburn-emacs
  (add-to-list 'custom-theme-load-path (locate-user-emacs-file "el-get/zenburn-emacs"))
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg+05" . "#282828")
      ("zenburn-bg+1"  . "#2F2F2F")
      ("zenburn-bg+2"  . "#3F3F3F")
      ("zenburn-bg+3"  . "#4F4F4F")))
  (load-theme 'zenburn t))
