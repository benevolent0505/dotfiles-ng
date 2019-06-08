;; emacs -l でinit.elを指定して設定を読み込めるように
;; https://web.archive.org/web/20180203065224/https://unknownplace.org/memo/2013/01/21/1/
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; package.el で入るパッケージとかち合うので、 package-initialize よりも先に呼ぶ必要がある
(el-get-bundle dash)
(el-get-bundle s)

;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Appearance
(setq-default inhibit-startup-screen t
              initial-scratch-message ""
              indent-tabs-mode nil
              show-trailing-whitespace t
              tab-width 2)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(show-paren-mode t)
(tool-bar-mode -1)

(set-frame-parameter nil 'alpha 90)
(set-frame-parameter nil 'fullscreen 'maximized)

;; Language environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; Font
;; TODO: 設定方法を理解できていないので困まったら調べ直す
;; See Also: http://extra-vision.blogspot.com/2016/07/emacs.html
(create-fontset-from-ascii-font
  "Source Han Code JP-12:weight=normal:slant=normal" nil "Source Han Code JP")
(set-fontset-font
  "fontset-Source Han Code JP" 'unicode "Source Han Code JP-12:weight=normal:slant=normal" nil 'append)
(add-to-list 'default-frame-alist '(font . "fontset-Source Han Code JP"))

;; Color theme
(el-get-bundle dracula/emacs
  :name dracula-theme

  (add-to-list 'custom-theme-load-path (locate-user-emacs-file "el-get/dracula-theme"))
  (load-theme 'dracula t))

;; Brackets settings
(electric-pair-mode t)
(el-get-bundle rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Other build-in features

;; yes/no を y/n に
(defalias 'yes-or-no-p 'y-or-n-p)
;; バッファ移動を Meta + 矢印でも出来るように
(windmove-default-keybindings 'meta)

;; バックアップファイルを一箇所に集める
(add-to-list 'backup-directory-alist (cons "." (locate-user-emacs-file "backups")))
(setq-default auto-save-file-name-transforms
  `(("*" ,(expand-file-name (locate-user-emacs-file "backups")) t)))
