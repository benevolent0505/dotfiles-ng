(el-get-bundle company
  :type github :pkgname "company-mode/company-mode"

  (add-hook 'after-init-hook 'global-company-mode))
