;; emacs -l でinit.elを指定して設定を読み込めるように
;; https://web.archive.org/web/20180203065224/https://unknownplace.org/memo/2013/01/21/1/
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

(el-get-bundle tarao/el-get-lock)

;; package.el
(require 'package)
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)
(package-initialize)


;; Emacs > 28
(if (>= emacs-major-version 28)
  ;; native compile setting
  (setq comp-async-report-warnings-errors nil
        warning-minimum-log-level :error)
  ;; yes/no を y/n に
  (defalias 'yes-or-no-p 'y-or-n-p))

;; server
(require 'server)
(unless (server-running-p)
  (server-start))


;; Appearance
(setq-default initial-scratch-message ""
              indent-tabs-mode nil
              show-trailing-whitespace t  ;; TODO: prog-mode 以外は whitespace をハイライトしないようにしたい
              tab-width 2)

;; UI Settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; smooth scroll
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse t
              scroll-step 1)

(set-frame-parameter nil 'alpha 90)
(set-frame-parameter nil 'fullscreen 'maximized)

(show-paren-mode t)

;; Language environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; Font
;; TODO: 設定方法を理解できていないので困まったら調べ直す
;; See Also: http://extra-vision.blogspot.com/2016/07/emacs.html
(set-frame-font "Source Han Code JP N-14")

;; Color theme
;; Emacs 27 以下には modus theme が含まれていない
(when (< emacs-major-version 28)
  (el-get-bundle! modus-themes)
  (setq-default modus-themes-italic-constructs t
                modus-themes-bold-constructs)
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

;; Brackets settings
(electric-pair-mode t)
(el-get-bundle! rainbow-delimiters
  :features color

  ;; emphasis
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30)))

  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Other build-in features

;; バッファ移動を Meta + 矢印でも出来るように
(windmove-default-keybindings 'meta)

(setq-default default-directory "~/"
              command-line-default-directory "~/")

;; バックアップファイルを一箇所に集める
(add-to-list 'backup-directory-alist (cons "." (locate-user-emacs-file "backups")))
(setq-default auto-save-file-name-transforms
  `(("*" ,(expand-file-name (locate-user-emacs-file "backups")) t)))


(el-get-bundle tarao/with-eval-after-load-feature-el)


(el-get-bundle exec-path-from-shell

  ;; 追加したい環境変数が増えたらここに足す
  (let ((envs '("PATH" "GOPATH" "GO111MODULE")))
    (exec-path-from-shell-copy-envs envs)))


(el-get-bundle helpful
  :depends (dash f s)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)

  (global-set-key (kbd "C-h C") #'helpful-command))

;; SKK
;; 何故か起動時にロードされないので package.el でインストールしている
;; (el-get-bundle ddskk)
(setq-default skk-server-host "localhost"
              skk-server-portnum 1178
              skk-dcomp-activate t
              skk-dcomp-multiple-rows 20
              skk-comp-prefix t
              skk-share-private-jisyo t)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key (kbd "C-x C-j") 'skk-mode)))
;; org-mode にすると C-j が改行に奪われている気がするのでその対応
(add-hook 'org-mode-hook
          (lambda ()
            (global-set-key (kbd "C-x C-j") 'skk-mode)))

(global-set-key (kbd "C-j") 'skk-mode)


(el-get-bundle ace-window
  (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (global-set-key (kbd "C-x o") 'ace-window))


(el-get-bundle avy
  (global-set-key (kbd "C-;") 'avy-goto-word-1))


(el-get-bundle! keyfreq
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(el-get-bundle! editorconfig
  (editorconfig-mode 1))


(el-get-bundle hl-todo
  ;; NOTE: 現状ハイライトしてくれるだけでも嬉しいので、キーワード間の移動は欲しくなったら考える
  (global-hl-todo-mode))


;; ivy/counsel
;; バージョン固定する
(el-get-lock 'swiper)

(el-get-bundle counsel
  :features (ivy)

  (ivy-mode 1)

  ;; ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 20
        ivy-wrap t)

  ;; swiper
  (setq-default swiper-include-line-number-in-search t)

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; 検索はこっちが見やすいが、ヘルプは helpful-key の方が見やすい
  ;; TODO: 検索インターフェースを counsel-descbinds にして、 helpful のヘルプが出せないか考える
  (global-set-key (kbd "C-h b") 'counsel-descbinds)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rg)

  (with-eval-after-load-feature 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  (counsel-mode 1))

;; ghq support
(el-get-bundle! analyticd/ivy-ghq)
(global-set-key (kbd "C-c C-g") 'ivy-ghq-open)

;; xref support
(el-get-bundle! alexmurray/ivy-xref)
(setq xref-show-definitions-function #'ivy-xref-show-defs
      xref-show-xrefs-function #'ivy-xref-show-xrefs)


;; インストールしていれば counsel-M-x が勝手に使ってくれる
(el-get-bundle amx
  :depends s)


(el-get-bundle company
  (global-company-mode)

  (setq-default company-idle-delay 0.0
                company-dabbrev-downcase nil
                company-minimum-prefix-length 1
                company-selection-wrap-around t
                completion-ignore-case t
                company-transformers '(company-sort-by-occurrence))

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(el-get-bundle company-quickhelp
  :depends (company-mode pos-tip)

  (company-quickhelp-mode 1))


;; LSP
(el-get-bundle! lsp-mode)
(setq lsp-enable-imenu nil
      lsp-modeline-diagnostics-enable t
      lsp-headerline-breadcrumb-enable t
      lsp-completion-enable t)

;; Performance Tuning
(setq gc-cons-threshold 800000000
      read-process-output-max (* 1024 1024))

(el-get-bundle lsp-ui)
(setq-default lsp-ui-doc-use-webkit t
              lsp-ui-doc-max-height 300
              lsp-ui-doc-max-width 150)

;; https://github.com/emacs-lsp/lsp-ui/issues/123#issuecomment-384941120
(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Monaco" :height 150)))

(el-get-bundle lsp-ivy
  :depends (dash lsp-mode counsel))


;; Git
;; NOTE: counsel の方に git grep 等の設定があるのでそちらもチェックする

;; magit 関係はバージョン固定にする
(el-get-lock 'magit)

(el-get-bundle magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x C-b") 'magit-blame))

(el-get-lock 'transient)
(el-get-bundle transient)

(el-get-bundle! git-gutter+
  (global-git-gutter+-mode t))

(el-get-bundle git-link
  (setq-default git-link-open-in-browser t
                git-link-use-commit t))


(el-get-bundle flycheck
  (setq-default flycnheck-disabled-checkers '(emacs-lisp-checkdoc javascript-jshint javascript-jscs json-jsonlint))

  (add-hook 'prog-mode-hook #'flycheck-mode))


(el-get-bundle! rg
  :depends (transient wgrep)

  (rg-enable-default-bindings))


;; yasnippet
(el-get-bundle! yasnippet
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippet"))
  (yas-global-mode 1))

(el-get-bundle yasnippet-snippets
  (with-eval-after-load-feature 'yasnippet
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "el-get/yasnippet-snippets/snippets"))))


;; Jump
(el-get-bundle dumb-jump
  :depends (popup)

  (setq-default dumb-jump-selector 'ivy)
  (dumb-jump-mode 1))
(el-get-bundle jojojames/smart-jump
  :depends (popup)

  (smart-jump-setup-default-registers))


;; ivyだと拡張子を入力しようとすると絞り込みになってしまうので,
;; open-junk-file 実行後は RET を押してファイルを作る
(el-get-bundle! open-junk-file
  (setq-default open-junk-file-format "~/local/tmp/%Y/%m/%Y-%m-%d-%H%M%S-.org"))

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))


;; Docker
(el-get-bundle! dockerfile-mode
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


(el-get-bundle! yaml-mode
  (setq-default yaml-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))


(el-get-bundle graphql-mode)


;; terraform
(el-get-bundle terraform-mode)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)


(el-get-bundle restclient)


;; For Jenkinsfile
(el-get-bundle groovy-mode
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode)))


;; Web development
(el-get-bundle! web-mode

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl?\\'" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for web-mode"
    (setq-default web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-script-padding 2))
  (add-hook 'web-mode-hook #'my-web-mode-hook))


(el-get-bundle! nginx-mode)


;; SQL
(defun my-sql-mode-hook ()
  (setq-default tab-width 4))
(add-hook 'sql-mode-hook #'my-sql-mode-hook)


;; JavaScript
(el-get-bundle js2-mode
  (setq-default js-indent-level 2)

  (add-hook 'js-mode-hook 'js2-minor-mode)

  (with-eval-after-load-feature 'lsp-mode
    (add-hook 'js-mode-hook #'lsp)))

;; TypeScript
(el-get-bundle typescript-mode
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

  (setq-default typescript-indent-level 2)

  (with-eval-after-load-feature 'lsp-mode
    (add-hook 'typescript-mode-hook #'lsp)))

(el-get-bundle add-node-modules-path
  (with-eval-after-load-feature 'js2-mode
    (add-hook 'js2-mode-hook #'add-node-modules-path))

  (with-eval-after-load-feature 'typescript-mode
    (add-hook 'typescript-mode-hook #'add-node-modules-path)))

(el-get-bundle json-mode
  (with-eval-after-load-feature 'flycheck
    (add-hook 'json-mode-hook #'flycheck-mode)))


;; Go
(el-get-bundle go-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))


;; Perl
(defalias 'perl-mode 'cperl-mode)

(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("cpanfile$" . cperl-mode) auto-mode-alist))

(setq-default cperl-indent-level 4
  cperl-continued-statement-offset 4
  cperl-close-paren-offset -4
  cperl-comment-column 119
  cperl-highlight-variables-indiscriminately t
  cperl-indent-parens-as-block t
  cperl-indent-subs-specially nil
  cperl-label-offset -4
  cperl-tab-always-indent t
  cperl-font-lock t
  cperl-break-one-line-blocks-when-indent nil
  cperl-fix-hanging-brace-when-indent     nil
  cperl-merge-trailing-else               nil
  cperl-indent-region-fix-constructs      nil
  cperl-max-help-size 119)

;; Documentation
(el-get-bundle! org-roam)
(setq-default org-roam-directory "~/local/org-roam"
              org-roam-completion-everywhere t
              org-roam-v2-ack t)
(org-roam-setup)
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(define-key org-mode-map (kbd "C-M-i") 'complition-at-point)

;; 環境固有の設定はここに入れる
(when (file-exists-p (expand-file-name "~/.private.el"))
  (load (expand-file-name "~/.private.el")))
