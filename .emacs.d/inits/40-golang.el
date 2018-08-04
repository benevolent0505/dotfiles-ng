(el-get-bundle! go-mode
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(el-get-bundle! go-eldoc
  (with-eval-after-load-feature 'go-mode
    (add-hook 'go-mode-hook #'go-eldoc-setup)))

(el-get-bundle company-go
  :url "https://raw.githubusercontent.com/nsf/gocode/master/emacs-company/company-go.el"
  :features company-go

  (add-hook 'go-mode-hook
    (lambda ()
      (set (make-local-variable 'company-backends) '(company-go))
      (company-mode))))

(with-eval-after-load-feature (flycheck go-mode)
  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))
