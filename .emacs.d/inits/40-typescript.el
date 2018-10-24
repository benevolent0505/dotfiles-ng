(el-get-bundle! typescript-mode
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(el-get-bundle! tide
  :depends (s)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(el-get-bundle Simplify/flycheck-typescript-tslint
  (with-eval-after-load-feature 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup)))

(with-eval-after-load-feature (web-mode flycheck)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
    (lambda ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode))))

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; setup
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)

  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))

(with-eval-after-load-feature (typescript-mode tide flycheck)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))
