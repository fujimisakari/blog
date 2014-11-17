title: HEXOでブログ構築手順
date: 2014-11-16 15:46:06
tags: HEXO

---

今回ブログ構築で利用したブログ生成ツールHEXOの導入手順メモ。
基本的には以下サイトを参考にやってます
http://liginc.co.jp/web/programming/server/104594


## HEXOについて簡単に
- 静的なブログサイトを構築するための環境
- javascript製でNode.jsの動作環境が必要
- MarkDown方式で投稿ができる
- 開発サーバー機能があるのでローカルでの投稿チェック、UI改修ができる
- Themeやプラグインが豊富


## Node.jsを動作環境を準備

まずは、NVM（Node Version Manager）をインストールして、
nvmコマンドを読み込む
``` shell
% git clone git://github.com/creationix/nvm.git ~/.nvm
% source ~/.nvm/nvm.sh
```

最新安定版の0.10.26のNode.jsをインストールして
デフォルト利用するNode.jsバージョンを設定する
``` shell
% nvm install v0.10.26
% nvm alias default v0.10.26
```

デフォルトでnvmコマンドを利用できるよう以下を.zshrcへ追記
```
if [[ -s ~/.nvm/nvm.sh ]];
 then source ~/.nvm/nvm.sh
fi

```
## HEXOの導入

HEXOをインストールしてブログを生成
``` shell
% npm install -g hexo
% hexo init myblog
% cd myblog
% npm install
```

初期設定のため、_config.ymlを編集
```diff
--- a/_config.yml
+++ b/_config.yml
@@ -3,16 +3,16 @@
 ## Source: https://github.com/hexojs/hexo/

 # Site
-title: Hexo
+title: Fujimisakari
 subtitle:
 description:
 author: John Doe
-email:
+email: fujimisakari@gmail.com
 language:

 # URL
 ## If your site is put in a subdirectory, set url as 'http://yoursite.com/child' and root as '/child/'
-url: http://yoursite.com
+url: http://fujimisakari.github.io/
 root: /
 permalink: :year/:month/:day/:title/
 tag_dir: tags
@@ -79,10 +79,12 @@ disqus_shortname:

 # Deployment
 ## Docs: http://hexo.io/docs/deployment.html
 deploy:
-  type:
\ No newline at end of file
+  type: github
+  repo: git@github.com:fujimisakari/fujimisakari.github.io.git
+  branch: master
```

これでブログ構築が完了。
