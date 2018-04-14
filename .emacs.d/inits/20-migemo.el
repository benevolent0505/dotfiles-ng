(el-get-bundle! migemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  (let ((ws window-system))
    (cond ((eq ws 'ns)
            (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))
