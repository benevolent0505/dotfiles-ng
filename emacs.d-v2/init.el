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

;; init-loader setting
(el-get-bundle! emacs-jp/init-loader
  (setq init-loader-show-log-after-init nil
	init-loader-byte-compile t)
  (unless (file-directory-p (locate-user-emacs-file "inits"))
    (make-directory (locate-user-emacs-file "inits")))
  (init-loader-load (locate-user-emacs-file "inits")))
;; package.el で入るパッケージとかち合うので、 package-initialize よりも先に呼ぶ必要がある
(el-get-bundle dash)
(el-get-bundle s)
