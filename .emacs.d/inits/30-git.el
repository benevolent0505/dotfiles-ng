(el-get-bundle magit
  (global-set-key (kbd "C-x g") 'magit-status))

(el-get-bundle! git-gutter+
  (global-git-gutter+-mode)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter+-next-hunk))

(el-get-bundle! sshaw/git-link
  (global-set-key (kbd "C-c g l") 'git-link)
  (custom-set-variables
    '(git-link-open-in-browser t)
    '(git-link-use-commit t)))
