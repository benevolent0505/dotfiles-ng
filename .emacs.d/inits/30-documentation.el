(setq-default eldoc-idle-delay 0.1
              eldoc-echo-area-use-multiline-p t
              flycheck-display-errors-delay 0.2)

(dolist (hook '(emacs-list-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook #'eldoc-mode))

(el-get-bundle eldoc-extension)
