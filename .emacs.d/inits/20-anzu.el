(el-get-bundle! anzu
  (global-anzu-mode +1)

  (set-face-attribute 'anzu-mode-line nil
    :foreground "yellow" :weight 'bold)

  (with-eval-after-load-feature 'migemo
    (setq anzu-use-migemo t))

  (custom-set-variables
    '(anzu-mode-lighter "")
    '(anzu-deactivate-region t)
    '(anzu-search-threshold 1000)
    '(anzu-replace-threshold 50)
    '(anzu-replace-to-string-separator " => "))

  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
