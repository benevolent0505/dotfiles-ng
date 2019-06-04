;; emacs の設定を隔離
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get settings
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

;; init-loader settings
(el-get-bundle! emacs-jp/init-loader
  (setq-default init-loader-show-log-after-init nil
                init-loader-byte-compile t)
  (init-loader-load (locate-user-emacs-file "inits")))
