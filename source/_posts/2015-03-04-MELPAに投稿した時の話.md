title: MELPAに投稿した時の話
date: 2015-03-04 01:13:35
tags: Emacs
---

10日ほど前に、EmacsのパッケージマネジャーのMELPA(Milkypostman’s Emacs Lisp Package Archive)に
helm-xcdocというものを作成しpull requestした時の話しをつらつらと。

まず、helm-xcdocはEmacsのhelmを利用してxcodeのドキュメント検索
そしてViewer(eww)で閲覧できるツールっぽいもの。
https://github.com/fujimisakari/emacs-helm-xcdoc

最近iOSのアプリを開発しててEmacsでobjective-cの勉強してるけど
ドキュメント検索するツールで良さげなもの見つからなかった。
ただ、IMAKADOさんの[xcode-document-viewer.el](https://github.com/imakado/emacs-xcode-document-viewer/blob/master/xcode-document-viewer.el)がやりたいことに近かったけど
anythingとw3mで古めだったので、自分のelispの勉強がてら作成することにした。
objective-cの勉強したかったのにかなり脱線してるけど...

作成するならMELPAにpull requestするくらいのものにしようと思ったので
現在投稿されてる中から、やりたいことに近いコードを確認した。
っで、[syohex](https://github.com/syohex)さんのhelm-gtag, helm-agがやりたいことに近かったし
すごくコードが整ってるように見えたのでかなり参考にさせてもらった。

一週間ぐらいでできたので、MELPAへpull requestしたところ数時間後に
なんとsyohexさんからhelm-xcdocの修正取り込みの[pull request](https://github.com/fujimisakari/emacs-helm-xcdoc/commit/fe779cc0a0b79fb6690972d54f36e3f847e39e2f)が来た。
この時、OSSって偉大だなって思った。
自分みたいな理解が浅い新参者が投稿しても、こうゆう大御所の方が
チェックして修正してpull requestで教えてくる。
こうやってOSSはコードの品質とかが守らていってるんだろうなっと感じた。
自分もこうゆうエンジニアになりたいと思う今日この頃でした。
