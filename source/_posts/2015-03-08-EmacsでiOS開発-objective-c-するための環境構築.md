title: EmacsでiOS開発(objective-c)するための環境構築
date: 2015-03-08 16:57:04
tags:
- Emacs
- Objective-C

---

最近iOS開発をはじめたけど、Interface builderやstoryboardなど
Xcodeに依存する機能が多くEmacsで開発できんの？って思ったけど
UI系はXcode、コード改修はEmacsみたいに行う作業を割り切れば全然問題なさそう。
ただ、Emacsのobjc-modeがデフォルト設定のままだとツラすぎなので
いろいろ機能を追加して開発環境を整えたので紹介しようかと。

# 追加した機能
1. objc-modeの拡張設定
2. コード整形(clang-format)
3. インタラクティブな実行環境(quickrun)
4. Xcodeのドキュメント検索(helm-xcdoc)
5. コード自動補完(emaXcode, yasnippet, auto-complete)
6. tagジャンプ(gtags)
7. シンタックスチェック(flymake)

## 1. objc-modeの拡張設定

以下の拡張設定をします
- TABスペースは利用せず、半角スペースを4つにする(デフォルトのままでは2になってる)
- .hファイルもobjc-modeで開くけるようにする
- FoundationやUIKit等のFrameworkの.hファイルを検索対象に含めるようにする
- 対になってる.hファイルと.mファイルをトグルできるようにする
- .hと.mを左右に並べて開く

![objc-mode-extend](/image/objc/objc-mode-extend.gif)

### 設定

```lisp
;; iOS SDKへのPATH
(defvar xcode:sdk "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")

;; TABスペースは利用せず、半角スペースを4つにする(デフォルトのままでは2になってました)
(add-hook 'objc-mode-hook
          '(lambda()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)))

;; .hファイルもobjc-modeで開くけるようにする
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

;; FoundationやUIKit等のFrameworkの.hファイルを検索対象に含めるようにする
(setq xcode:frameworks (concat xcode:sdk "/System/Library/Frameworks"))
(setq cc-search-directories (list xcode:frameworks))
(defadvice ff-get-file-name (around ff-get-file-name-framework
                                    (search-dirs
                                     fname-stub
                                     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

;; 対になってる.hファイルと.mファイルをトグルできるようにする
(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
(define-key c-mode-base-map (kbd "C-c '") 'ff-find-other-file)

;; .hと.mを左右に並べて開く
(defun open-header-and-method-file ()
  (interactive)
  (other-window-or-split)
  (ff-find-other-file))
(define-key objc-mode-map (kbd "C-c ;") 'open-header-and-method-file)
```

## 2. コード整形
コード整形はclang-formatを利用します。
clang-formatはC、C++、Objective-c用のコード整形ツールで選択したリジョンを
指定したスタイルでを整形することができます。

![codeformat](/image/objc/codeformat.gif)

### 設定
・clang-formatコマンドのインストール

```bash
$ brew install clang-format
```

・clang-format.elはmelpaでインストール。
　　M-x package-refresh-contents
　　M-x package-install clang-format

```lisp
(require 'clang-format)
;; 以下で整形スタイルを設定できます(defult: "file")
(setq clang-format-style "Google")
;; キーバインド指定
(define-key objc-mode-map (kbd "C-c ") 'clang-format-region)
```

## 3. インタラクティブな実行環境
テストコードや処理の挙動を手軽に確認したい場合は[quickrun](https://github.com/syohex/emacs-quickrun)を利用します。
あらかじめmainが入ってるテンプレートファイルを作成しておき
確認したい場合、テンプレートファイルを開けばインタラクティブな実行確認ができます

![quickrun](/image/objc/quickrun.gif)

### 設定
・quickrunはmelpaでインストール。
　　M-x package-refresh-contents
　　M-x package-install quickrun

```lisp
;; quickrunにclangでの実行環境を追加しておきます
(add-to-list 'quickrun-file-alist '("\\.m$" . "objc/clang"))
(quickrun-add-command "objc/clang"
                      '((:command . "clang")
                        (:exec    . ("%c -fobjc-arc -framework Foundation %s -o %e" "%e"))
                        (:remove  . ("%e")))
                      :default "objc")
```

## 4. Xcodeのドキュメント検索
Xcodeのドキュメント検索にはhelm-xcdoc(拙作)を利用します
カーソル上のシンボル名 or 検索ワードからドキュメント検索を行いViewer(eww)で確認できます。
Emacs24.4以上、helm 1.5以上が利用可能条件となります

![xcdoc](/image/objc/xcdoc.gif)

### 設定
・emacs-helm-xcdocは、以下からダウンロードします
　　https://github.com/fujimisakari/emacs-helm-xcdoc

```lisp
(require 'helm-xcdoc)
(setq helm-xcdoc-command-path "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
(setq helm-xcdoc-document-path "~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS8.1.iOSLibrary.docset")
(define-key objc-mode-map (kbd "C-c d") 'helm-xcdoc-search-other-window)

(require 'popwin)
(push '("*eww*") popwin:special-display-config)
```
## 5. コード自動補完(emaXcode, yasnippet, auto-complete)
コード自動補完するためには、[emaXcode](https://github.com/ShingoFukuyama/emaXcode), yasnippet, auto-completeを利用します。
この3つelispの役割は以下のようになります。
- emaXcode: Frameworkの.hファイルから情報を抽出してyasnippetが理解できる形式でファイルに書き出す。
　　　　　　　auto-completeの情報源(ac-source-emaXcode-yasnippet)の設定を行う。
- auto-complete: 文脈に沿った自動補完をする
- yasnippet: 補完で選択されたsnippetを展開して入力する

※ emaXcodeはコード自動補完以外にも幅広く機能を備えてます。
　 たとえば、objc-modeの拡張設定やシンタックスチェック(flymake)部分の機能も用意されてたりします。
　 利用した場合、emaXcodeに依存した設定が多くなりそうだったので自分は自動補完のみ利用することにしました。

![auto-complate](/image/objc/auto-complate.gif)

### 設定
・emaXcodeは、以下からダウンロードします
　　https://github.com/ShingoFukuyama/emaXcode

・auto-complete、yasnippetはmelpaでインストール。
　　M-x package-refresh-contents
　　M-x package-install auto-complete
　　M-x package-install yasnippet

```elisp
;; 'emaXcode設定
(require 'emaXcode)
(setq xcode:foundation (concat xcode:sdk "/System/Library/Frameworks/Foundation.framework/Headers/"))
(setq xcode:uikit (concat xcode:sdk "/System/Library/Frameworks/UIKit.framework/Headers/"))
(setq emaXcode-yas-objc-header-directories-list (list xcode:foundation xcode:uikit))

;; auto-complete設定
(ac-config-default)
;;ac-auto-startが整数値の場合、文字列の長さがac-auto-start以上になると自動補完開始
(setq ac-auto-start 3)

;; yasnippet設定
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/share/snippets"))
(yas-global-mode 1)

;; (必須)最初にロードしておく
(yas-load-directory "~/.emacs.d/share/snippets")

;; ロードしていない場合、*.[mh]を開く時に始めてsnippetのloadが走ります。
;; 情報源(ac-source-emaXcode-yasnippet)の作成はsnippetのloadの前に
;; 行なわれますので空の情報源になってしまいます
```

・設定完了後に以下を実行し、Frameworkの.hファイルからyasnippet用の情報をファイルに書き出しときます(1回のみでOK)
　　M-x emaXcode-yas-get-objc-messages-from-header-files

## 6. tagジャンプ
tagジャンプには、globalコマンドとgtags.el、helm-gtags.elを利用します。
globalでObjective-cはサポートしておらず利用できなさそうだったのですが
pygmentsとctagsのカスタム設定を利用することで実現できました。

![gtags](/image/objc/gtags.gif)

### 設定
・gtags、helm-gtagsはmelpaでインストール。
　　M-x package-refresh-contents
　　M-x package-install gtags
　　M-x package-install helm-gtags

・globalコマンドインストール

```bash
$ brew install global --with-exuberant-ctags --with-pygments
```

・pygmentsのpipインストール
　　pygmentsはsudoでネイティブなシステム環境にインストールする必要がありあす。
　　globalがpygmentsを利用する際、#!/usr/bin/pythonから利用するので
　　virtualenvやpyenvでユーザーローカルにpygmentsをインストールしてもimportできなので。

```bash
$ sudo pip install pygments
```

・ctags拡張設定

```
$ vim ~/.ctags
--langdef=objc
--langmap=objc:.m.h
--regex-objc=/^[[:space:]]*[-+][[:space:]]*\([[:alpha:]]+[[:space:]]*\*?\)[[:space:]]*([[:alnum:]]+):[[:space:]]*\(/\1/m,method/
--regex-objc=/^[[:space:]]*[-+][[:space:]]*\([[:alpha:]]+[[:space:]]*\*?\)[[:space:]]*([[:alnum:]]+)[[:space:]]*\{/\1/m,method/
--regex-objc=/^[[:space:]]*[-+][[:space:]]*\([[:alpha:]]+[[:space:]]*\*?\)[[:space:]]*([[:alnum:]]+)[[:space:]]*\;/\1/m,method/
--regex-objc=/^[[:space:]]*\@property[[:space:]]+.*[[:space:]]+\*?(.*);$/\1/p,property/
--regex-objc=/^[[:space:]]*\@implementation[[:space:]]+(.*)$/\1/c,class/
--regex-objc=/^[[:space:]]*\@interface[[:space:]]+(.*)[[:space:]]+:.*{/\1/i,interface/
```

・シンボリクリンク生成
　　tagジャンプ対象にFrameworkの.hファイル含ませるため
　　project直下にFrameworkまでのシンボリックを配置します

```bash
$ cd ~/ios_project
$ ln -s /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk/System/Library/Frameworks frameworks
```

・TAGファイルの生成

```bash
$ cd ~/ios_project
$ find . -name "*.[hm]" -follow -print | /usr/local/bin/gtags --gtagslabel=pygments-parser -f - ./
```

・gtag設定

```lisp
(require 'gtags)
;; ファイル保存時にGTAGSを更新する
(setq gtags-auto-UPDATE t)

;; 相対pathで表示
(setq gtags-path-style 'relative)
;; (setq gtags-path-style 'absolute)

;; *GTAGS SELECT* のバッファは1つだけ生成する
(setq gtags-select-buffer-single t)
```

## 7. シンタックスチェック
シンタックスチェックは、数年前の記事だとgccを利用したチェックが行われてたけど
現在のMacOSXの標準コンパイラーはclangなのでこちらを利用しました。

![flymake](/image/objc/flymake.gif)

### 設定
```lisp
;;; flymakeでシンタックスチェック
(defvar flymake-objc-compiler (executable-find "clang"))
(defvar flymake-objc-compile-default-options (list "-D__IPHONE_OS_VERSION_MIN_REQUIRED=30200" "-fsyntax-only" "-fobjc-arc" "-fblocks" "-fno-color-diagnostics" "-Wreturn-type" "-Wparentheses" "-Wswitch" "-Wno-unused-parameter" "-Wunused-variable" "-Wunused-value" "-isysroot" xcode:sdk))
(defvar flymake-last-position nil)
(defcustom flymake-objc-compile-options '("-I.")
  "Compile option for objc check."
  :group 'flymake
  :type '(repeat (string)))

(defun flymake-objc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))

(defun flymake-display-err-minibuffer ()
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list
          (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file
                           (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(defun flymake-display-err-minibuffer-safe ()
  (ignore-errors flymake-display-err-minibuffer))

;; もともとのパターンにマッチしなかったので追加
(setq flymake-err-line-patterns
      (cons
       '("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" 1 2 3 4)
       flymake-err-line-patterns))

;; 拡張子 m と h に対して flymake を有効にする設定
(add-hook 'objc-mode-hook
          (lambda ()
            (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
            (push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)
            ;; 存在するファイルかつ書き込み可能ファイル時のみ flymake-mode を有効にします
            (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                (flymake-mode t))))
```

## 参考にしたサイト
http://www.emacswiki.org/emacs/ObjectiveCMode
http://fukuyama.co/emaxcode
http://sakito.jp/emacs/emacsobjectivec.html
