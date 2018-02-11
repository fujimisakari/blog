title: EmacsでGoの開発環境構築
date: 2018-02-04
tags:
- Emacs
- Go

---

今さら感がありますが、最近よくGoを使っててあらためてEmacs環境を整備したのでざっくりまとめてみます。

## メジャーモードの導入(go-mode)

まずGo言語のためのメジャーモードのgo-modeで入れます。
シンタックスハイライトやインデント、goコマンドをelispインタフェース通して利用できるようになります。

```lisp
M-x package-install go-mode
```

<img src="/image/go/go-mode.png" width="720" height="420" />

## 環境変数設定

go-modeやGoに関連する作業に`GOPATH`が必要になるのでEmacsにも環境変数を設定します。

```lisp
(setenv "GOPATH" "/home/dev/go")
```

## タグジャンプ設定(helm-gtags)

定義元へのジャンプは `godef`がよく紹介されてますが他言語の開発とタグジャンプ操作は同じインタフェースにしたいので`Gnu Global + Pygments`を利用した`helm-gtags`でタグジャンプ設定します。
GOPATH直下でタグDBを作成するのですべての定義元へジャンプできるようになります。

導入方法はこの辺りを参考
https://qiita.com/yoshizow/items/9cc0236ac0249e0638ff
https://github.com/syohex/emacs-helm-gtags
http://blog.10rane.com/2014/09/17/to-reading-comprehension-of-the-source-code-by-introducing-the-helm-gtags-mode/

Go言語はまだPygmentsでサポートされていないので`.ctags`に以下を追加します
```
--langdef=Go
--langmap=Go:.go
--regex-Go=/func([ \t]+\([^)]+\))?[ \t]+([a-zA-Z0-9_]+)/\2/d,func/
--regex-Go=/var[ \t]+([a-zA-Z_][a-zA-Z0-9_]+)/\1/d,var/
--regex-Go=/const[ \t]+([a-zA-Z_][a-zA-Z0-9_]+)/\1/d,const/
--regex-Go=/type[ \t]+([a-zA-Z_][a-zA-Z0-9_]+)/\1/d,type/
```

## コード補完(go-autocomplete)

コード補完は`gocode`を利用するので事前インストールします

```
$ go get -u github.com/nsf/gocode
```

`auto-complete.el`を利用したインタフェースで補完しますのでこちらをインストールして、
その後にGo言語の補完で利用する`go-autocomplete`をインストールします。

```lisp
(require 'go-autocomplete)
```

## エラーチェック(go-flymake)

エラーチェックはgo-flymakeを利用します。
入力のたびにGoのSyntaxを自動チェックして知らせてくれるようになります。

```lisp
(require 'go-flymake)
```


## ドキュメント確認

### go-eldoc

カーソル位置のシンボルのドキュメントをミニバッファに表示してくれます

```lisp
M-x package-install go-eldoc

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
```

### パッケージ単位でドキュメントを確認

ここのやつをちょっといじってるだけです
http://syohex.hatenablog.com/entry/20130618/1371567527

helm経由でパッケージを選んでドキュメントを確認できるようになります

```lisp
(defvar helm-go-packages-source
  (helm-build-sync-source "Helm Go Packages"
    :candidates (go-packages)
    :candidate-number-limit 300
    :action 'godoc))

(defun helm-go-packages ()
  (interactive)
  (helm :sources '(helm-go-packages-source) :buffer "*helm go packages*"))
```

### ポップアップでドキュメントを確認

`go-eldoc`とほぼ同じ機能なのですが、ミニバッファだと見にくかったり、詳細がなかったり、他とイベントとカブったとき表示されないのでPopupで確認できるようにしました。
操作はリージョン選択して`godoc-popup`を実行するだけです。(popup.elが必要になります)

```lisp
(defun godoc-popup ()
  (interactive)
  (unless (use-region-p)
    (error "Dose not region selection"))
  (let ((query (buffer-substring-no-properties (region-beginning) (region-end))))
    (run-at-time 0.1 nil 'deactivate-mark)
    (popup-tip
     (with-temp-buffer
       (let ((standard-output (current-buffer))
             (help-xref-following t))
         (prin1 (funcall 'shell-command-to-string (concat "go doc " query)))
         (buffer-substring-no-properties (+ (point-min) 1) (- (point-max) 3)))))))
```

<img src="/image/go/godoc-popup.png" width="720" height="420" />

## 任意パッケージをdiredで開く

こちらはhelm-ghq.elで実現できたのですが、目的のパッケージをdiredで開くだけのシンプルなものがほしかったので用意しました。(事前にghqコマンドをインストールしておく必要があります)

```lisp
(defun helm-ghq--get-candidates ()
  (let* ((cmd-result (funcall 'shell-command-to-string "ghq list"))
         (candidates (split-string cmd-result "\n"))
         (candidates (sort candidates 'string<))
         (candidates (cdr candidates)))
    candidates))

(defvar helm-ghq-list-source
  (helm-build-sync-source "Helm ghq list"
    :candidates (helm-ghq--get-candidates)
    :candidate-number-limit 300
    :action 'helm-ghq--dired))

(defun helm-ghq--dired (name)
  (when (one-window-p)(split-window-horizontally))
  (other-window 1)
  (dired (concat (getenv "GOPATH") "/src/" name)))

(defun helm-ghq-list ()
  (interactive)
  (helm :sources '(helm-ghq-list-source) :buffer "*helm ghq list*"))
```

<img src="/image/go/ghq-list.png" width="1000" />

## Tips

### コード整形

go-modeのデフォルト機能の`gofmt`を利用する。現在のバッファに対してコード整形が適用されます。
また保存時自動的にこのコマンドを実行したい場合は 次のようにhookに登録します。

```
(add-hook 'before-save-hook 'gofmt-before-save)
```

### パッケージのimport

こちらもgo-modeのデフォルト機能の`go-import-add`を利用します。helm経由で選択できるので便利。

```
go-import-add (Default: C-c C-a)
```

また、使っていないimportも `go-remove-unused-imports` を使うと使用していないpackageたちをimportから一括で外してくれるはずだがなぜかできない。。

### コード整形とimportの整理を同時にしてくれる

`goimports`はgofmtの関連ツールで標準のディストリビューションには入ってませんが
コード整形と必要に応じてインポートの宣言の挿入と削除を同時に行ってくれます。

```
$ go get golang.org/x/tools/cmd/goimports

gofmtコマンドの上書き設定します
(setq gofmt-command "goimports")
```

### godocのバッファー名は`*godoc*`にする

go-mode機能のgodoc使った場合に、新たにバッファが生成されどんどん増えていくのでpopwinで無駄なバッファを生成しないようにしたいのですが
バッファー名が`*godoc xxxxx*`みたいに毎回変わりpopwin設定ができないのでバッファー名は`*godoc*`固定となるように関数を上書きます。

```lisp
(defun godoc--get-buffer (query)
  "Get an empty buffer for a godoc QUERY."
  (let* ((buffer-name "*godoc*")
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))
```

### ショートカット

若干めんどうだったのでショートカットを用意しました

```
(defun insert-go-channel-arrow ()
  (interactive)
  (insert "<-"))

(defun insert-go-expression ()
  (interactive)
  (insert ":="))
```


## 現在の設定
https://github.com/fujimisakari/.emacs.d/blob/master/inits/46-go-mode.el

## 参考サイト
http://emacs-jp.github.io/programming/golang.html
http://syohex.hatenablog.com/entry/20130618/1371567527
http://unknownplace.org/archives/golang-editing-with-emacs.html
