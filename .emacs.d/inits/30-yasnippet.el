(el-get-bundle! yasnippet
  (setq yas-snippet-dirs
    (list (locate-user-emacs-file "snippets")))
  (yas-global-mode 1))
