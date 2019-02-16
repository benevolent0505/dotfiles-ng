(el-get-bundle magit
  (setq-default magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-x g") 'magit-status))

(el-get-bundle windymelt/counsel-ghq
  (global-set-key (kbd "C-x C-g") 'counsel-ghq))
