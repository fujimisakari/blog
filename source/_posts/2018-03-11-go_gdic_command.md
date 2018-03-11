title: pecoでDockerを操作する
date: 2018-03-11 00:00
tags:
- Go
- Docker

---

タイトルの通りですが、[peco](https://github.com/peco/peco)でDockerを操作するコマンドを作りました。
はじめに理想を言うと、素のDockerコマンドをタイポせずすごい勢いで打てるのが一番カッコイイと思ってるのですが自分には無理なので最高のインタフェースを持つpeco(本当はpercol)でDocker操作を効率化できるようにしました。

これまではよく紹介されてるshell設定のエイリアスやスクリプト、pecoと組み合せた操作を設定して満足できてたんですが、ただ目的の操作のために毎回コマンドを実行しないといけないのが微妙にめんどいなって思ってたので1回のコマンド操作できるよう作りました。

だいたいの雰囲気はこんな感じです

- `docker rm`を連続で選択した場合

![gdic rm](/image/go/gdic-rm.gif)

- `docker exec`を実行した場合(zsh設定でCtr-x rにバインドして起動してます)

![gdic exec](/image/go/gdic-exec.gif)

## 特徴

https://github.com/fujimisakari/gdic

- コマンド名は go docker incremental cli の略から `gdic` としました。(特徴から単語を並べてみただけです)
- 次のDockerコマンドをpecoで起動 → 選択できるようになる
  - `docker exec`
  - `docker run`
  - `docker stop`
  - `docker rm`
  - `docker rmi`
- `stop`、`rm`、`rmi`の場合は一度コマンドを起動すると`exit`しない限りは連続で選択できる
- `run`、`exec` の場合は、選択したコンテナ名で実行コマンドを生成できる
- `rm`、 `rmi` のコマンド失敗時もpeco上でエラーメッセージを確認することができる


## インストール方法

pecoをインストールして、gdicを`go get`します(バイナリの配布はしていません)

```
# macの場合
$ brew install peco

# Linuxの場合
https://github.com/peco/peco/releasesからバイナリをダウンロードして実行Pathへ配置

$ go get github.com/fujimisakari/gdic
```


## 使い方

Dockerコマンドをgdicに変更して実行すると、コマンドに関連した候補がpecoで起動されます。

```sh
$ gdic exec
$ gdic run
$ gdic stop
$ gdic rm
$ gdic rmi
```

あと、お好みですがShell設定(zsh)に以下を追加してます。

```sh
# alias設定
alias dstop='gdic stop'
alias drm='gdic rm'
alias drmi='gdic rmi'

# dockerコンテナのRun先を選択
function peco-docker-run () {
   BUFFER=$(gdic run)
   CURSOR=$#BUFFER
   zle clear-screen
}
zle -N peco-docker-run
bindkey '^Xr' peco-docker-run

# dockerコンテナのexec先を選択
function peco-docker-exec () {
   BUFFER=$(gdic exec)
   CURSOR=$#BUFFER
   zle clear-screen
}
zle -N peco-docker-exec
bindkey '^Xe' peco-docker-exec
```
