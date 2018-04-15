(el-get-bundle ggtags)

(el-get-bundle helm-gtags
  (with-eval-after-load-feature 'helm-config
    (require 'helm-config)
    (require 'helm-gtags)

    (add-hook 'helm-gtags-mode-hook
      '(lambda ()
         (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
         (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
         (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
         (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))))
