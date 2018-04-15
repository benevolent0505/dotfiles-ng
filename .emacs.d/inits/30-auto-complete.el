;; for perl-completion
(el-get-bundle auto-complete/fuzzy-el)
(el-get-bundle auto-complete/popup-el)

(el-get-bundle! auto-complete-config in auto-complete
  (ac-config-default)
  (ac-set-trigger-key "TAB")

  (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "ac-dict"))

  (custom-set-variables
    '(ac-auto-start nil))
  (setq
    ac-auto-show-menu 0.02
    ac-menu-map t
    ac-use-fuzzy t
    ac-use-comphist t)

  (let ((map ac-complete-mode-map))
    (define-key map (kbd "C-n") 'ac-next)
    (define-key map (kbd "C-p") 'ac-previous)
    (define-key map (kbd "TAB") nil)))
