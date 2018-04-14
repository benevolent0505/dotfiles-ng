(el-get-bundle magit
  (global-set-key (kbd "C-x g") 'magit-status))

(el-get-bundle! git-gutter
  (global-git-gutter-mode +1)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

  (custom-set-variables
 '(git-gutter:update-interval 2)))

(el-get-bundle! git-messenger
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
  (custom-set-variables
    '(git-messenger:use-magit-popup t)))
