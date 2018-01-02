title: テスト駆動開発を読んで
date: 2017-12-04 00:00
tags: TDD

---

ほしいと思ってたんですが気付いたらamazonとジュンク堂には無く、第2刷でようやく手に入れました。

<br />

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274217884/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51hsd-b1RTL._SL160_.jpg" alt="テスト駆動開発" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274217884/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">テスト駆動開発</a></div><div class="amazlet-detail">Kent Beck <br />オーム社 <br />売り上げランキング: 4,007<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274217884/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

<br />

自分はTDDについての知識は、ネット上の誰かの記事でつまみ読み程度でこうした本を読んで勉強するのは初めてでした。そして、普段の開発でテストを書いてるかというとそんなに書いてなく必要に応じて書く感じでした。率先して書いてなかった理由としてあまりテストの知識を持ってなく勉強してキレイなテストを書けるようになってからっと思ってたので、時が来たって感じでた。

読んでみると最初にすごくビックリしたのがTDDの概念を誤認してたことです。まずテストを書いて開発を進めるテストファーストな開発方法がTDDと思ってたのですがそれは間違いでした。TDDは開発者が設計の治具としてテストコードを同時に書きながら開発と改善を回していくのが目的でした。なので、この本にはテストとリファクタを回しながらどうやって動作するキレイなコードになるかまでが書かれています。

コードの設計やロジックを美しく書くことは日々意識してますが、設計がよければテストも書きやすくなるみたいな思想でしたがそれは間違っていることに気付せられます。自分の場合、アーキテクチャ駆動でまず「キレイな」を最初に取り組み、そのうえであちこち設計の辻褄を合せながらどうにか「動作する」を実現させていました。TDDは、最初に「動作する」に取り組み、その後で「キレイな」に取り組むのでこれまで行っていたアーキテクチャ駆動の開発とは正反対でとても興味深い発見でした。

TDDの開発手法以外にもテスト自体のことも触れられており、「仮実装」、「三角測量」、「明白な実装」や「良いテストを見分ける方法」などテストをどうキレイに機能させるかというアプローチは今後テストを書く上で非常に参考になりそうでした。

そして、付録として[t_wada](https://twitter.com/t_wada)さんの訳者解説が非常に良かったです。
とても印象に残った文で
> テスト駆動開発とは練習によって獲得できる技術です。最初はテストコードを書くのを難しいと感じることもあるでしょう。時間がかかることもあるしょう。「量は質に転化する」と言われています。誰でも最初から良いテストコードをすらすら書けるわけではありません。量をこなすうちに、テストを書くコツが身につき、テストから考え、テストコードをストレスなく書けるようになります。(付録C 訳者解説より引用)

テストと関連ありませんが、これまでプログラマとしてもっと成長したいと思い、業務などとは関係ない分野も好きだから勉強してましたがどこかで意味あんのかなって思ったり、もっと効率良く勉強する方法を探した方がいいのではなど、疑心暗記になることがありました。これを読んで直接は影響なくとも成長するために幅広くやることに間違いはないと思えるようになりもっと質の高いプログラムを書けるようになりたいという気持ちになりました。

まとめとして、この本はもっとプログラミングの質を上げたいと思っている人に特にオススメと感じました。テストを通じてコードをキレイに書くためのパターンがいくつも紹介されてるので実装のヒントになったり新しい発見をできるのではと思います。
