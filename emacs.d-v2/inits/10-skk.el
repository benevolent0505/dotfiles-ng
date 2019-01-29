(el-get-bundle ddskk
  (global-set-key (kbd "C-j") 'skk-mode)

  (setq-default skk-init-file (expand-file-name "init/.skk" user-emacs-directory)
		skk-user-directory "~/.ddskk"
		skk-server-host "localhost"
		skk-server-portnum 1178
		skk-share-private-jisyo t))
