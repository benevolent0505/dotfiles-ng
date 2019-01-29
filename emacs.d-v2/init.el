;; isolate emacs env
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; 使わないけど, package-install の設定はする
(package-initialize)

;; el-get setting
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
