(el-get-bundle plenv
  :type github :pkgname "karupanerura/plenv.el"
  :feature plenv)

(el-get-bundle! cperl-mode
  (defalias 'perl-mode 'cperl-mode)

  (setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.psgi$" . cperl-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("cpanfile$" . cperl-mode) auto-mode-alist))

  (setq-default cperl-indent-level 4
    cperl-continued-statement-offset 4
    cperl-close-paren-offset -4
    cperl-comment-column 40
    cperl-highlight-variables-indiscriminately t
    cperl-indent-parens-as-block t
    cperl-indent-subs-specially nil
    cperl-label-offset -4
    cperl-tab-always-indent t
    cperl-font-lock t
    cperl-break-one-line-blocks-when-indent nil
    cperl-fix-hanging-brace-when-indent     nil
    cperl-merge-trailing-else               nil
    cperl-indent-region-fix-constructs      nil)

  ;; https://m0t0k1ch1st0ry.com/blog/2014/07/07/flycheck/
  (flycheck-define-checker perl-project-libs
    "A perl syntax checker."
    :command (
               "perl"
               (option-list "-I" flycheck-perl-include-path)
               "-MProject::Libs lib_dirs => [qw(t/lib modules/*/lib local/lib/perl5)]"
               "-wc"
               source-inplace)
    :error-patterns ((error line-start
                       (minimal-match (message))
                       " at " (file-name) " line " line
                       (or "." (and ", " (zero-or-more not-newline)))
                       line-end))
    :modes (cperl-mode))
  (add-hook 'cperl-mode-hook
    '(lambda ()
       (flycheck-mode t)
       (setq flycheck-checker 'perl-project-libs)
       (setq flycheck-perl-include-path `(,(git-root-directory)))))

  (add-hook 'cperl-mode-hook 'flycheck-mode))
