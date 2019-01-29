(el-get-bundle counsel
  (ivy-mode 1)

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep))
