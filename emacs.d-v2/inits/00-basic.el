;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." (locate-user-emacs-file "backups")))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name (locate-user-emacs-file "backups")) t)))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; align
(require 'align nil t)

(el-get-bundle tarao/with-eval-after-load-feature-el)
