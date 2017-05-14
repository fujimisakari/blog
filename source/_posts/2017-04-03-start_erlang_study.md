title: Erlangの学習を始めた
date: 2017-04-03 00:00
tags: Erlang
---

前からErlangを学習したいと思ってたけど、他にやりたいことがあって
なかなか手をつけられずようやくって感じです。

Erlangで何かサービスを作りたいとか業務で使いたいとかは全然なくて
言語として学習してみたいという興味が強くて始めました

始める動機としては以下があります。
- 純粋な関数型言語を覚えたい
- ネットワークプログラミングに知見を増やたい
- 並列、並行の処理の知見を増やたい
って感じです。

純粋な関数型言語を覚えたいってのは、プログラムの実装バリエーションが増えるからです。
以前lispを勉強したとき、オブジェクト指向や手続き型にはあまり意識したことがない
実装パターンなどを触れることができ、クロージャや再帰のパターン
副作用がない関数など、プログラミングする上での実装の幅が広がりました。
そのときに、純粋な関数型言語も一度は学習していみたいと思ってました。

ネットワークプログラミングは去年から学習を進めているところで
TCPレイヤーでのプログラムミングをC, Go, Pythonで浅く広く
やってってるのでErlangでも覚えたいってとこです。

並列、並行の処理では、thread, goroutineを触ってきて
Erlangのプロセスの挙動などもどうゆうものか知りたい感じです。

始める動機書いてみたけど、Erlangのことはふわっとしか知らないので
Erlangの人で有名な[@voluntas](https://twitter.com/voluntas)さん以下の記事を参考にして飛行機本から始めました!

http://voluntas.hatenablog.com/entry/20110319/1300525884

> まずは飛行機本を買いましょう、飛行機本を買えないのであれば Erlang を学ぶのはあきらめた方が良いくらい良著です

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51bZh24YhqL._SL160_.jpg" alt="プログラミングErlang" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">プログラミングErlang</a><div class="amazlet-detail" style="margin-top:20px;">Joe Armstrong <br />オーム社 <br />売り上げランキング: 188,900<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

### <a href="https://gist.github.com/fujimisakari/26a3c5307df5f11b987f33eb785684ad">学習メモ Index</a>

- <a href="https://gist.github.com/fujimisakari/ec8950e52919d2672662986488d4b934">Erlangプログラミングについて</a>
- <a href="https://gist.github.com/fujimisakari/b023d4a6f9c2e47ccf911098579971e0">プログラミングErlang - 基本1</a>
- <a href="https://gist.github.com/fujimisakari/d7274b00b3001ec273c088b6fd9723cf">プログラミングErlang - 基本2</a>
- <a href="https://gist.github.com/fujimisakari/abc6b00d12289964089acc45c7fbd9df">プログラミングErlang - 例外</a>
- <a href="https://gist.github.com/fujimisakari/5081385537c63c64a82b187c00f3e21b">プログラミングErlang - 用語</a>
- <a href="https://gist.github.com/fujimisakari/7699692785324f6d97ad3dcd8ce2beef">プログラミングErlang - 属性</a>
- <a href="https://gist.github.com/fujimisakari/7a04813fa73591a24294e14524173f81">プログラミングErlang - 式</a>
- <a href="https://gist.github.com/fujimisakari/e9992fb77be077bce9886ef930ecdfce">プログラミングErlang - 構文</a>
- <a href="https://gist.github.com/fujimisakari/68e5783913ebfa7dd190d3ea3f0127e0">プログラミングErlang - コンパイルと実行</a>
- <a href="https://gist.github.com/fujimisakari/91339d3bdab90549dbdffd7879682520">プログラミングErlang - 並行プログラミング</a>
- <a href="https://gist.github.com/fujimisakari/7899f1cf27366abf3a8a31e5b95ab51e">プログラミングErlang - 並行プログラミングにおけるエラー</a>
- <a href="https://gist.github.com/fujimisakari/22115a70bf43473bc8845f143e6b9043">プログラミングErlang - 分散プログラミング</a>


### 参考記事
https://gist.github.com/voluntas/7278561
