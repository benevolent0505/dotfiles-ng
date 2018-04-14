(el-get-bundle open-junk-file
  :url "https://github.com/emacsmirror/emacswiki.org/blob/master/open-junk-file.el"
  :type emacsmirror
  (setq open-junk-file-format "~/local/tmp/%Y/%m/%Y-%m-%d-%H%M%S-.org")
  (global-set-key (kbd "C-c j") 'open-junk-file))
