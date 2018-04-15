;; for perl-completion
(el-get-bundle auto-complete
  (ac-set-trigger-key "TAB")
  (setq ac-auto-start nil
    ac-auto-show-menu 0.02
    ac-menu-map t)
  (let ((map ac-complete-mode-map))
    (define-key map (kbd "C-n") 'ac-next)
    (define-key map (kbd "C-p") 'ac-previous)
    (define-key map (kbd "TAB") nil)))
