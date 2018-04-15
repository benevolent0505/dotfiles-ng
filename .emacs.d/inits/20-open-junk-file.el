(el-get-bundle! emacswiki:open-junk-file
  (setq open-junk-file-format "~/local/tmp/%Y/%m/%Y-%m-%d-%H%M%S-.org")
  (global-set-key (kbd "C-c j") 'open-junk-file))
