title: HEXOの使い方
date: 2014-11-16 21:40:40
tags: HEXO

---

## デプロイする
``` shell
% hexo deploy -g
or
% hexo d -g
```
-gで静的HTMLをpublicフォルダ内に生成してくれる

## 新たに投稿する
``` shell
% hexo new 'hexoについて'
```
source/_postsフォルダに"hexoについて.md"というファイルが生成される
あとはこのファイルを編集して記事の詳細を書く

## テーマの変更
HEXOのThemeはどれにするか迷うくらい公開されていて
以下のサイト確認できる
http://js.romareas.net/demo/ ← こっちはページ遷移が必要ないので見やすい
https://github.com/hexojs/hexo/wiki/Themes

テーマをインストールする
``` shell
% git clone テーマのgitリポジトリURL themes/Theme名
```
_config.ymlにインストールしたテーマを記載する
``` shell
theme: Theme名
exclude_generator:
```

## RSS機能を付ける
hexo-generator-feedをインストールする
``` shell
% npm install hexo-generator-feed --save
```

_config.ymlに以下のオプションを追記。
``` shell
feed:
    type: atom
    path: atom.xml
    limit: 20
```
