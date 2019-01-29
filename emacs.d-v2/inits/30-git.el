(el-get-bundle magit
  (setq-default magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-x g") 'magit-status))
