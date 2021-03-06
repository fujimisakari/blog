title: pyenvで環境構築
date: 2015-07-16 00:55:56
tags: Python
---

python開発環境で複数プロジェクトを横断する場合に便利なpyevnの導入手順メモ。
pyenv自体は以前から利用していて、いつもyouhei-nitta大先生の[ブログ](http://blog.youhei.jp/post/43650820070/pyenv)を参考に
環境構築していたけど、個人的にも手順を残そうと思った。

## pyenv関連パッケージのインストール
python開発環境するにあたり、まずpyenv、pyenv-virtualenvの二つのパッケージをインストール
``` shell
brew install pyenv pyenv-virtualenv
```

shellログイン時にpyenv関連のコマンド等を認識させるため、.zshrcに以下を追記
```
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi 
```

## pythonのインストール

``` shell
pyenv install 2.7.9
```

## プロジェクトごとのpython環境の構築
python-virtualenv と pyenv local でプロジェクトごとに環境構築

さきほど作成した 2.7.9 を元にvirtualenvでgameA, gameBの環境を構築
``` shell
pyenv virtualenv --distribute 2.7.9 gameA
pyenv virtualenv --distribute 2.7.9 gameB
```

pyenv local でそれぞれの Project Root 上に .pyenv-version を作る
``` shell
cd ~/project/game-A
pyenv local gameA
cd ~/project/game-B
pyenv local gameB
```

これでProject Root配下にいる場合はvirtualenvで構築したpython環境を参照するようになる
