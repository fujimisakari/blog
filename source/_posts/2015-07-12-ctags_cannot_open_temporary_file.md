title: "ctags: cannot open temporary file"
date: 2015-07-12 21:35:20
tags: gtags
---

久びさの技術投稿です。
gtagsのpygments-parserを利用してtagsを生成してたけど
global6.4以降でcrontabからのtagsの生成に失敗するようになった。
直そうとは思ってたけど、なかなか時間がとれなくて
ようやく着手したしたころには、もうglobal6.5になってましたw

状況としては、自分で直接コマンドを実行するとタグ情報の生成はうまく行き
cron経由でコマンドを実行するとタグ情報の生成に失敗してしまう。

実行してたコマンド
``` shell
find . -name "*.py" -follow -print | /usr/local/bin/gtags --gtagslabel=pygments-parser -f -
```

何が問題か調査のため、コマンドのエラー情報を出力するようにした
``` shell
find . -name "*.py" -follow -print | /usr/local/bin/gtags --gtagslabel=pygments-parser -f - > ~/error.log 2>&1
```

エラー情報を確認にすると「ctags: cannot open temporary file : No such file...」と表示されており
一時ファイルを開くことができないために失敗していた。
調査してみるとインタラクティブとcron時のTMPDIRの環境変数が差異があった。

インタラクティブ時の環境変数には以下が定義されており、
TMPDIR=/var/folders/60/0_yzzrwj4fzfdglbysmwjwrm0000gn/T/
cron時は何も定義されていなかったので、
cronの実行時は以下の環境変数を定義することで動作できるようなった。
``` shell
export TMPDIR='/var/folders/60/0_yzzrwj4fzfdglbysmwjwrm0000gn/T/'
```

ちなみにこの/var/foldersは、MacOS X独自の共通キャッシュ領域のようで
PC毎に独自定義のディレクトリが用意されて、再起動しても変更は無いようです
