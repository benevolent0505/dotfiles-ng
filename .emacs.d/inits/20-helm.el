(el-get-bundle helm
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "TAB") 'helm-execute-persistent-action)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)

  (helm-mode 1)
  (helm-autoresize-mode t)

  (setq-default helm-truncate-lines t
		helm-for-files-preferred-list))

(el-get-bundle! helm-ls-git
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(el-get-bundle! helm-git-grep
  (global-set-key (kbd "C-c g") 'helm-git-grep)
  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  (with-eval-after-load-feature 'helm
    (define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm)))

(el-get-bundle helm-ghq
  (add-to-list 'helm-for-files-preferred-list 'helm-source-ghq)
  (global-set-key (kbd "C-x C-g") 'helm-ghq))
