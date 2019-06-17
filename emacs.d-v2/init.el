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

;; package.el で入るパッケージとかち合うので、 package-initialize よりも先に呼ぶ必要がある
(el-get-bundle dash)
(el-get-bundle f)
(el-get-bundle s)

;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Appearance
(setq-default inhibit-startup-screen t
              initial-scratch-message ""
              indent-tabs-mode nil
              show-trailing-whitespace t  ;; TODO: prog-mode 以外は whitespace をハイライトしないようにしたい
              tab-width 2)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(show-paren-mode t)
(tool-bar-mode -1)

(set-frame-parameter nil 'alpha 90)
(set-frame-parameter nil 'fullscreen 'maximized)

;; Language environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; Font
;; TODO: 設定方法を理解できていないので困まったら調べ直す
;; See Also: http://extra-vision.blogspot.com/2016/07/emacs.html
(create-fontset-from-ascii-font
  "Source Han Code JP-12:weight=normal:slant=normal" nil "Source Han Code JP")
(set-fontset-font
  "fontset-Source Han Code JP" 'unicode "Source Han Code JP-12:weight=normal:slant=normal" nil 'append)
(add-to-list 'default-frame-alist '(font . "fontset-Source Han Code JP"))

;; Color theme
(el-get-bundle dracula/emacs
  :name dracula-theme

  (add-to-list 'custom-theme-load-path (locate-user-emacs-file "el-get/dracula-theme"))
  (load-theme 'dracula t))

;; Brackets settings
(electric-pair-mode t)
(el-get-bundle rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Other build-in features

;; yes/no を y/n に
(defalias 'yes-or-no-p 'y-or-n-p)
;; バッファ移動を Meta + 矢印でも出来るように
(windmove-default-keybindings 'meta)

;; バックアップファイルを一箇所に集める
(add-to-list 'backup-directory-alist (cons "." (locate-user-emacs-file "backups")))
(setq-default auto-save-file-name-transforms
  `(("*" ,(expand-file-name (locate-user-emacs-file "backups")) t)))


(el-get-bundle tarao/with-eval-after-load-feature-el)


(el-get-bundle exec-path-from-shell

  ;; 追加したい環境変数が増えたらここに足す
  (let ((envs '("PATH" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))


(el-get-bundle helpful
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)

  (global-set-key (kbd "C-h C") #'helpful-command))


;; TODO: AquaSKKと辞書を共有する方法を調べる
(el-get-bundle ddskk
  (setq-default skk-server-host "localhost"
                skk-server-portnum 1178
                skk-dcomp-activate t
                skk-dcomp-multiple-rows 20
                skk-comp-prefix t
                skk-share-private-jisyo t)

  (global-set-key (kbd "C-x j") 'skk-mode))


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
(el-get-bundle counsel
  :features ivy

  (ivy-mode 1)

  ;; ivy
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-height 20
                ivy-extra-directories nil
                ivy-count-format "(%d/%d) ")

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
  (global-set-key (kbd "C-c k") 'counsel-rg))

(el-get-bundle windymelt/counsel-ghq
  :depends (counsel)
  ;; FIXME: Emacs 起動時にキーバインドが設定されないのを直す
  (with-eval-after-load-feature 'counsel
    (global-set-key (kbd "C-x C-g") 'counsel-ghq)))


(el-get-bundle! company
  (add-hook 'after-init-hook 'global-company-mode)

  (setq-default company-dabbrev-downcase nil
                company-idle-delay 0
                company-minimum-prefix-length 2
                company-selection-wrap-around t
                completion-ignore-case t)

  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates))

(el-get-bundle company-quickhelp
  :depends (company-mode pos-tip)

  (company-quickhelp-mode 1))


;; Git
;; NOTE: counsel の方に git grep 等の設定があるのでそちらもチェックする
(el-get-bundle magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x C-b") 'magit-blame))

(el-get-bundle! git-gutter+
  (global-git-gutter+-mode t))

(el-get-bundle git-link
  (setq-default git-link-open-in-browser t
                git-link-use-commit t))


(el-get-bundle flycheck
  (setq-default flycnheck-disabled-checkers '(emacs-lisp-checkdoc javascript-jshint javascript-jscs json-jsonlint))

  (add-hook 'after-init-hook #'global-flycheck-mode))


;; yasnippet
(el-get-bundle! yasnippet
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippet"))

  (yas-global-mode 1))

(el-get-bundle yasnippet-snippets
  :depends (yasnippet)

  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "el-get/yasnippet-snippets/snippets")))


;; 挙動が変なのでそのまま open-junk-file 実行後は RET を押してファイルを作る
;; TODO: ivy との噛み合わせで意図しない挙動になっている気がするので調べる
(el-get-bundle! open-junk-file
  (setq-default open-junk-file-format "~/local/junk/%Y/%m/%Y-%m-%d-%H%M%S.org")

  (global-set-key (kbd "C-x C-j") 'open-junk-file))


(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))


(el-get-bundle! lsp-mode

  (add-hook 'prog-mode-hook #'lsp))

(el-get-bundle! company-lsp
  :depends (lsp-mode company yasnippet)

  (push 'company-lsp company-backends))


;; Web development
(el-get-bundle! web-mode

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for web-mode"
    (setq-default web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-script-padding 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook))


(el-get-bundle! nginx-mode)


;; JavaScript
(el-get-bundle js2-mode

  (setq-default js2-basic-offset 2
                js-indent-level 2)

  (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode))

(el-get-bundle rjsx-mode

  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode)))

(el-get-bundle tern

  (defun my-tern-mode-hook ()
    "Hooks for js-mode"
    (tern-mode t))
  (add-hook 'js-mode-hook 'my-tern-mode-hook))

(el-get-bundle company-tern
  :depends (company)

  (with-eval-after-load-feature 'company
    (add-to-list 'company-backends 'company-tern)))

(el-get-bundle add-node-modules-path

  (with-eval-after-load-feature 'js2-mode
    (add-hook 'js2-mode-hook 'add-node-modules-path)))


;; TypeScript


;; Go
(el-get-bundle go-mode
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))



;; Perl


;; Python
