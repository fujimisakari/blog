title: HeadFirstデザインパターンを読んで
date: 2015-01-10 01:28:03
tags:
- java
- DesignPatterns

---

年末にデザインパターンを勉強したので備忘録としてブログに残していこうかと。
勉強した本はこちら

<div class="amazlet-box" style="margin:30px 0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4873112494/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/519s0OfidCL._SL160_.jpg" alt="Head Firstデザインパターン ―頭とからだで覚えるデザインパターンの基本" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4873112494/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Head Firstデザインパターン ―頭とからだで覚えるデザインパターンの基本</a></div><div class="amazlet-detail" style="margin-top:20px">Eric Freeman Elisabeth Freeman Kathy Sierra Bert Bates <br />オライリージャパン <br />売り上げランキング: 57,159<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4873112494/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

- 学習メモ
  - <a href="https://gist.github.com/fujimisakari/043638e2e47337a16e21552acc386b2a">HeadFirstデザインパターン Index</a>
  - <a href="https://gist.github.com/fujimisakari/5797dfb18f4fc350e206eced83474d2e">Strategyパターン</a>
  - <a href="https://gist.github.com/fujimisakari/c1c1b9b10f101a2b2c62425e2cc4f820">Decoratorパターン</a>
  - <a href="https://gist.github.com/fujimisakari/d5bb42bc8a2990960dec46fbc318a308">Observerパターン</a>
  - <a href="https://gist.github.com/fujimisakari/1a4591b8902f292ca9b0e193f866608c">AbstractFactoryパターン</a>
  - <a href="https://gist.github.com/fujimisakari/69ba6278a24de3dc34ce34bbbe8a273c">FactoryMethodパターン</a>
  - <a href="https://gist.github.com/fujimisakari/2149e54c2e0db573017ac42e0f6a9772">Commandパターン</a>
  - <a href="https://gist.github.com/fujimisakari/b38f173158234bad23ca7a47ec8a498d">Singletonパターン</a>
  - <a href="https://gist.github.com/fujimisakari/9dd755295805b6d14be8867e1b733f8b">Adapterパターン </a>
  - <a href="https://gist.github.com/fujimisakari/21ace327b49776c30e76168d02811d79">Facadeパターン </a>
  - <a href="https://gist.github.com/fujimisakari/a36bbfe3e7d06d4d598ef82884a9e9eb">TemplateMethodパターン</a>
  - <a href="https://gist.github.com/fujimisakari/9e1b28e56c51313b3ac662ce6221f724">Iteratorパターン </a>
  - <a href="https://gist.github.com/fujimisakari/a4e5504b27e4addd46c3f80ccc7ab7a0">Compositパターン</a>
  - <a href="https://gist.github.com/fujimisakari/83700cd2d3576181de23143958545df0">Stateパターン</a>

所感としては、デザインパターンを知らず、どうすればプログラミングレベルを
上げることができるか迷ってる方にオススメと思いました。

自分は要求された機能に対して実装できるプログラミングスキルは付いて来たと感じてたけれど、
出来上がったコードに対して、これは良いコードなのか？と判断できませんでした。
とりあえず、コード設計的には以下を意識してました
- 1つの関数が肥大化しないよう細かく関数を分ける
- 関数は1画面内に収まる行数にする
- 重複した処理は存在しないようにする
- controler層で業務処理を行わないようにし、処理の役目を明確にさせる

上記を守りつつコードを書くとそれなりに良いものは出来てると感じてたけど
可読性、保守性の高いコードになってるか今ひとつ自信が持てませんでした。
この原因は、毎回上記意識していることを守りつつ機能を実装しているだけで
出来上がったコードがどういう設計の意図か自分で明確になっていないためでした。

デザインパターンを知るとコード設計の意図が明確になります。
各パターンにはそれぞれどうゆう場合に適してるか意図があるため、機能を実現するために適した
パターンを組合せてそれぞれのコードに意図を持たせる設計ができるようになります。
そしてデザインパターンを沿って実装すると、デザインパターン知ってる人であればかなり可読性は高くなります。
また知らない人が触った場合でも、各コード役割が明確なのでおのずと、
- ここは必ずインタフェースで呼び出さなければならい
- 追加実装があった場合は既存コードは触らず拡張対応しなればならない
- カプセル化できてるので内部に独自処理を入れない

等のルールに自然と従うようになると思います。

こんな感じで、デザインパターンは知ってるのと知らないのでコード設計が大きく変りました。
今までいろいろプログラミング向上するための本を見たけど、デザインパターンを知ることは
かなり手応えを感じたし、もっと成長できるなって思いました
