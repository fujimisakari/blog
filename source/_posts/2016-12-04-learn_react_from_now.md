title: これからReactを学ぼうとしてる人へ
date: 2016-12-04
tags: 
- React
- AdventCalender

---

この記事は[AdventCalandar2016のReact](http://qiita.com/advent-calendar/2016/react)の4日目の記事です。
今年の夏にReactを勉強したのでその時の感じたことやハマッたことなどの知見を共有できたらと思います。

まず、自分のJSレベルとしてはWebアプケーション開発でjQueryもしくは素のJSで
UIの何らかのイベントに対してDOMやサーバー(ajax)に対して、アクションを設定するレベルです。
あと、個人でAngularJSを利用してSPAのWebアプリを開発したことがありました。
Reactを学ぶにあたっては、チュートリアル以上のことはできるようになりたかったので
勉強がてらSPAのWebアプリを開発してみようと思いました。

## 環境の選定

最初にReactを開発する上でいろんな組み合わせがネット上で紹介されてますが
以下のtomoyaさんの記事を参考に環境選定しました(ほぼそのまま利用)

http://d.hatena.ne.jp/tomoya/20160403/1459665374

- React
- Redux
- react-router
- material-ui
- axios
- ES2015
- Babel
- webpack
- ESLint
- Airbnb JavaScript Style Guide
- karma
- mocha

## 理解が必要なもの

事前にすべてを理解する必要は無いと思いますが
開発するのであれば以下の理解が必要になってきます。

- Redux(Flux)の設計概念
- ES6(ES2015)の文法
- JSXについて
- babelが何をしてくれてるのか
- Webpackのやってくれてること

自分はRedux、JSXだけ知ってれば大丈夫だろう思って進めてましたが
最終的はその他の理解が必要でした。


## チュートリアル

とりあえず、よくあるtodoやcounterアプリをチュートリアルとして作りました。
Reduxでの設計やES6を使った開発は自分にとっても新鮮で楽しく書けたものの
まだ動いた動いたぐらいしか感想はなくて、とりあえずなんかもう少し大きめなアプリを
作って見ないと全然Reactを書いた感触は掴めないという印象でした。


## Webアプリ開発

開発では何を作るかは重要ではなかったのですがAPI Documentの自動生成ツールを作ることにしました。
実装しようと思った機能としては以下になります
- 外部サーバーからDocumentの元となるjsonを受けとって、リクエストパラメータやレスポンス結果例をAPI毎に表示してくれる
- リクエストパラメータ情報を使って外部サーバーにリクエストテストできる


### いざ開発しようとすると

まずReact、Reduxに関してはチュートリアルレベルしか知見がないので
もう少し実務開発に近い実装の情報を探したんですが、実はそんなに無いです。
なので、githubにあるリポジトリを見て回わり知見を増やしていくのが一番良いと思います。

リポジトリを見て回わり筋がよさそうな以下を参考にしました
reactGo: https://github.com/reactGo/reactGo
material-ui: https://github.com/callemall/material-ui


### componentsとcontainersの責務に迷う

Reduxの設計思想では、components側は主にUI部分のパーツ化に専念し、
containers側はcomponentsにデータをバインドすることに専念するように理解してたのですが
チュートリアルのコードやgithub上のコードは実装がさまざまでした。
UI部分のパーツは必ずcomponents側に定義してcontainers側から呼び出す実装にしてたり、
containers側にcomponents相当を一部書いてたりしてどれを適用すれば良いのか迷います。
とりあえず、前者で考えました。


### デバッグ方法

React開発でのデバッグは、画面構成とStore情報をカジュアルに確認できるツールが必要なります。

- 画面構成について
React Developer Tools(Chrom拡張機能)を利用しました。
Reactだと仮想DomでJSが画面を作ってるため、html要素で確認できません。
このツールを使うと仮想Domの要素を確認できるようになります。

- Store情報について
redux-devtoolsを利用しました。
Reactは現在の保持しているデータはStoreに集約されています。
これを利用すると何かのイベント毎にstore情報が変化するの常に確認できるようなります。


### Material-UI

GoogleのマテリアルデザインをReactのコンポーネントとして利用できるモジュールです。
Documentがかなり整備されており、コンポーネントの実装サンプルがほぼすべてあると思います。
Documentに載っていないAd-hocな使い方をしたい場合は、
リポジトリを落してコードを見ながらできるできないの判断すると良いと思います。


### ReactのStoreの非同期処理

ReactのStoreの操作は同期処理なりますので
非同期処理で処理を行いたい場合はredux-thunkを使います。
Storeの生成時にthunkでラップしてあげると非同期可能なStoreなります。
今回の開発では、外部サーバーからjsonを受けとるときに利用しました。
先にUI画面が作られデータ無い状態でも、jsonを受けとりStoreに保存されたタイミングで
画面の該当部分だけ再描画をしてくれます。


### Formの作り方

Material-UIのサンプルコードをそのまま利用して開発してたら
Formコンポーネントに入力(Actionイベント)したタイミングで
Formの親コンポーネントのActionイベントも走るようになりハマりました...
ReduxFormを使いましょう。


### End hook

UI画面の生成後にイベントを起したかったんですが設定方法がわかりませんでした。
componentDidMount()が期待してる機能に近いと思ったんですが
Component単位なので画面生成の最後に実施できるものではありませんでした。
スマートではないと思いますが、index.htmlに`<script src="/hoge.js"></script>`を追加しました。


## 感想

一通りReact+Reduxをさわった感覚としては簡単では無いよね？って一番感じました。
AngularJSで開発した時より学習コストが高く感じました。
ただ、設計思想や実装などを一度わかってしまうとコードはわかりやすく可読性が高く感じれます。
自分が持ってなかった新しい設計思想でしたので、
いろんな設計パラダイムの知見を広げるために価値があるコストと思いました。

今回開発したコード
https://github.com/fujimisakari/api-doc


## 参考サイト
http://d.hatena.ne.jp/tomoya/20160403/1459665374
http://mae.chab.in/archives/2885
http://www.hirooooo-lab.com/entry/development/react-redux-setup-environment
http://www.hirooooo-lab.com/entry/development/react-redux-materialui
