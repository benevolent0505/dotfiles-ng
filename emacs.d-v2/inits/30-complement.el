(el-get-bundle! company
  :type github :pkgname "company-mode/company-mode"

  (add-hook 'after-init-hook 'global-company-mode)

  (setq-default company-idle-delay 0
		            company-minimum-prefix-length 2
		            company-selection-wrap-around t
                company-dabbrev-downcase nil)

  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-i") 'company-complete-selection))

(el-get-bundle company-quickhelp
  :depends (pos-tip)
  (with-eval-after-load-feature 'company
    (company-quickhelp-mode +1)))
