title: emacs-eclimでjavaの環境構築
date: 2014-11-22 11:23:29
tags:
- java
- eclim

---

最近デザインパターンを勉強しようと思ってて、デザインパターンといえば
やっぱjavaかなって感じなのでjava未経験者なんだけど開発環境を整えてみた。
とりあえず、IDEは、Emacs一択しか考えてなかったのでいろいろ探したところ
emacs-eclimに辿りついた。

eclimっていうのは、Eclipsのプラグインでeclimデーモンを立ち上げて
外部のEmacsやvimからデーモンへコマンドリクエストを送ることで
Eclipseの機能が使用することができるようになる

とりあえず、ほしかった以下の機能が実現できた
- シンタックスのチェック
- Emacs上からのコンパイル
- Emacs上からの実行
- コード補完

## 開発環境
- MBA(10.10 Yosemite)
- java SE 1.8
- GNU Emacs 24.3.50.1
- Eclipse Luna(4.4.0)

## インストール
**javaとEclips、ビルドツールのantをインストール**
```
$ brew cask install java
$ brew cask install eclipse-java
$ brew install ant
```

**eclimをインストール**
本家からeclim_2.4.0.jarをダウンロードしてダブルクリックでインストールできた
http://eclim.org/install.html

**emacs-eclimをインストール**
MELPAからインストールしました(M-x list-package → emacs-eclim)


## eclimdを起動させる

1. Eclipse通常起動させ、workspaceとプロジェクトを作成させEclipseを終了させる

2. Emacsにeclim-mode設定を追記する
```el
(require 'eclim)
(require 'eclimd)

;; java-mode で有効
(add-hook 'java-mode-hook 'eclim-mode)

(custom-set-variables
  '(eclim-eclipse-dirs '("/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse"))
  '(eclim-executable "/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse/eclim")
  '(eclimd-default-workspace "~/projects/workspace"))

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)
;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;; エラー箇所にカーソルを当てるとエコーエリアに詳細を表示する
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(define-key eclim-mode-map (kbd "C-c C-e ;") 'eclim-run-class)
```

3. M-x start-eclimed でeclimをデーモンを起動させる

これでjava開発環境構築は完了。


## emacs-eclimの機能

### コード補完
auto-complete-modeとeclimを併用してTABでコード補完できる
![code_completion](/image/code_completion.gif)

### シンタックスのチェック
ファイル保存毎にシンタックスのチェックが動作する
![code_completion](/image/syntax_checking.gif)

### Emacs上からのコンパイル
M-x eclim-ant-run でbuildツールのantを使ってコンパイルできる
![code_completion](/image/ant_build.gif)

### Emacs上からの実行
M-x eclim-run-class でmainを実行します
![code_completion](/image/run_class.gif)


## 感想
eclimdと通信してるせいか、ときどきカクカクして重く感じた。
あとコード補完のレスポンスが遅い気がした(体感でTAB押してからレスポンスが来るまで3,4秒かかった気がする)
ほかはそんな気になったことはなかった。
eclimの機能は他にもいろいろあるのでちょこちょこ覚えていきたい
