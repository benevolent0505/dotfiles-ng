(el-get-bundle! rust-mode
  :depends (f)

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq rust-format-on-save t))

(el-get-bundle! racer in emacs-racer
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)

  (with-eval-after-load-feature 'rust-mode
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)))

(el-get-bundle flycheck-rust)

(defun setup-flycheck-rust ()
  (interactive)
  (flycheck-mode +1)
  (flycheck-rust-setup))

(with-eval-after-load-feature (rust-mode flycheck flycheck-rust)
  (add-hook 'rust-mode-hook #'setup-flycheck-rust))
