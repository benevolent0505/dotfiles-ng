;; for perl-completion
(el-get-bundle! auto-complete-config in auto-complete
  (ac-config-default)

  (add-to-list 'ac-modes 'cperl-mode)

  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t
    ac-use-fuzzy t
    ac-use-comphist t
    ac-auto-show-menu 0.02))
