(el-get-bundle! company
  :type github :pkgname "company-mode/company-mode"
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)

  (global-set-key (kbd "C-M-i") 'company-complete)

  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-m") 'company-complete-selection)

  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)

  (custom-set-variables
    '(package-selected-packages (quote (company))))
  (custom-set-faces))

(el-get-bundle company-quickhelp
  :depends pos-tip
  (with-eval-after-load-feature 'company
    (company-quickhelp-mode +1)))
