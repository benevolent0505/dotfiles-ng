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
