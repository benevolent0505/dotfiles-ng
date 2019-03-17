(el-get-bundle swiper
  :features ivy

  (ivy-mode +1)

  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-height 20
                ivy-extra-directories nil
                ivy-count-format "(%d/%d) ")

  (setq-default swiper-include-line-number-in-search t)

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(el-get-bundle windymelt/counsel-ghq
  (global-set-key (kbd "C-x C-g") 'counsel-ghq))
