(defun tuareg-mode-hooks ()
  (electric-indent-mode 0))

(el-get-bundle! tuareg in tuareg-mode
  (add-hook 'tuareg-mode-hook 'tuareg-mode-hooks))

