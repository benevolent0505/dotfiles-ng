(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))
