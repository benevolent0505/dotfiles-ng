(setq-default skk-init-file (expand-file-name "init/.skk" user-emacs-directory)
	      skk-user-directory "~/.ddskk")

(setq-default skk-server-host "localhost") ; AquaSKK のサーバー機能を利用
(setq-default skk-server-portnum 1178)     ; ポートは標準
(setq-default skk-share-private-jisyo t)   ; 複数 skk 辞書を共有

(el-get-bundle ddskk
  (global-set-key (kbd "C-j") 'skk-mode)

  (add-hook 'org-mode-hook '(lambda () (local-set-key (kbd "C-j") 'skk-mode))))
