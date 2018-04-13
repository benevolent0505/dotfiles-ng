(el-get-bundle! flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
