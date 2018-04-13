(el-get-bundle! company
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)

  (with-eval-after-load-feature 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-m") 'company-complete-selection)

    (define-key company-active-map (kbd "C-s") 'company-filter-candidates))
