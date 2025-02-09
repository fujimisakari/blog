title: APIリクエストの各認証方式
date: 2020-12-29 00:00
tags:
- Security

---

APIリクエストの認証っていろいろあって、よくわからなくなるので整理してみました。
（認証フローとかは省いてます）

## 認証/認可について

認証を整理するためには、認証/認可の役割は知っとく必要があります。

### 認証（Authentication）とは
「誰であるか」を本人確認すること。
ID、PWでの認証が一般的な方法ですが、APIだとどのような認証方法があるかは後述してます。

### 認可（Authorization）とは
「何の権限を持っているか」を確認、実証すること。
認可する過程で、認証処理が含まれてるので混乱しやすいです。
たとえば、OAuthで認可する場合、クライアント（サードパーティAPP）はユーザーから権限を与えてもらい認可を得ます。この権限を与える行為は事前にユーザー認証しないと行えないので、ややこしくなってます。

## さまざまな認証方式

自分がよく見る認証方式をまとめてみました。

### ■ 標準化されている認証方式

認証方式を標準化をしている団体はいくつもあって、有名どこだとIETF（RFC）やOASIS（SAML）、OpenID財団（OpenID）ってとこでしょうか。

#### ・Basic（[RFC 2617](https://tex2e.github.io/rfc-translater/html/rfc2617.html)）
ユーザ名とパスワードをBase64方式でエンコードして送る。
TLSではデータは見ませんが、平文と同じなんでユーザー認証として安全な方法ではないのでので、開発環境など内部システム認証によく使われてたりします。

#### ・Digest（[RFC 2617](https://tex2e.github.io/rfc-translater/html/rfc2617.html)）
サーバーとクライアントで共有シークレット（パスワード）を知っていることが前提の方式。
クライアントでユーザー名、パスワード、ランダムな文字列をMD5でハッシュ化し、サーバーに送信する。 サーバー側でもハッシュ値を計算し、 クライアントから送信されたハッシュ値と合致するかを検証する。Basic最大の弱点である平文でパスワードを送信することなく行うことができるメリットはありますが、MD5ハッシュなので強度が強くないです。

#### ・Bearer（[RFC 6750](https://openid-foundation-japan.github.io/rfc6750.ja.html)）
RFC 6750は認可サーバーにより発行されたアクセストークンを、Web APIに渡す方法を定めた仕様です。
OAuth 2.0の認可機構として設計されたものですが、OAuthに限らず汎用的なHTTP認可で使ってよいと書いてあります。

次の3つの方法を定義しています。
- ヘッダーのAuthorizationに埋め込む方法
- リクエストボディーへ埋め込む方法（access_tokenパラメータ）
- クエリーパラメーターとして渡す方法（access_tokenパラメータ）

#### ・OAuth1.0（[RFC 5849](https://openid-foundation-japan.github.io/rfc5849.ja.html)）
OAuthは、2007年に標準化されたユーザの同意のもと任意のサービスへ認可情報を移譲する仕様です。
クライアント（サードパーティAPP）へのアクセストークン付与可否をユーザに確認する仕組みですが、認可する過程で認証処理が含まてるので、OAuthを認証と扱っているケースは多数見受けられます。
※ RFC 5849で定められていますが、この仕様はOAuth2.0（RFC 6749）の策定をもって廃止されました。

#### ・OAuth2.0（[RFC 6749](https://openid-foundation-japan.github.io/rfc6749.ja.html)）
OAuth2.0は、OAuth1.0の脆弱性の対策を施して2012年に改定されたものです。
個人的には、このOAuthの認可が認証コンテキストを複雑化させている印象でした。
また、OAuth 2.0を認証利用することは問題があります（[車が通れるほどのどでかいセキュリティー・ホールができる](https://www.sakimura.org/2012/02/1487/)）。
implicit flowを使って「認証」すると、トークンの正当性を確かめる術がないので、なりすましできることがわかります。

#### ・OpenID Authentication（[OpenID Authentication 2.0 - 最終版](https://openid-foundation-japan.github.io/openid-authentication.html)）
OAuthが認可を目的にした仕様でしたが、OpenID Authenticationは認証を目的とした仕様になります。
ユーザ認証された旨の情報、認証をどのように行ったかの情報、当該ユーザの属性情報を、クライアント（サードパーティAPP）に対して転送する仕組みです。認証のみを提供しているため、権限を認可するような仕組みはありません。
各サービスに散在したID/PASS管理の煩雑さを解決策する仕様として広がっていきましたが、「認証だけでなく機能（権限）まで連携させたい」という思想が多く、OAuthが一気に普及しました。
※ 2014年にOpenID Connectによって置換えられました。

#### ・OpenID Connect（[OpenID Connect Core 1.0](https://openid-foundation-japan.github.io/openid-connect-core-1_0.ja.html)）
> OpenID Connect 1.0 is a simple identity layer on top of the OAuth 2.0 protocol.

[Webページ](https://openid.net/connect/)には、OpenID ConnectはOAuth2.0プロトコル上にシンプルなアイデンティティレイヤーを付与したものとのこと（[実際は全然違うようですw](https://qiita.com/TakahikoKawasaki/items/f2a0d25a4f05790b3baa#oauth-20-%E3%81%A8-openid-connect)）

OpenIDは、OAuth認証の手軽さから普及に至りませんでしたが、OAuth認証による認証レベルは低く問題がありました。そこでOAuth認証の手軽さとOpenIDの高い認証レベルを備えたOpenID Connectが策定されました。
これによりOAuthによる認可と同時に、OpenID Connectによる認証もできるようになりました。

### ■ セキュリティトークン（ワンタイムパスワード）
セキュリティトークンとは「ワンタイムパスワード」を生成する機械やソフトウェアのことで、ワンタイムパスワードとは、一度限りの使い捨てのパスワードのことになります。
強固なセキュリティが必要なWebサービスでは通常の認証ではセキュリティの観点から安全とは言い切れません。そのため、2要素認証（or多要素認証）サポートをしているところが多く、通常認証+ワンタイムパスワードの組み合わせはよく見かけます。
セキュリティトークンの種類はいくつかあって、物理カード型とアプリ（Google Authenticator）は自分もよく使ってます。
また、文脈は違いますがAPI通信でも一時的なワンタイムパスワードを発行して、認証利用しているとこはよく見かけますね。

### ■ APIキー認証
APIキーは、アカウント情報などはなくアプリケーションを識別する暗号化された単純な文字列です。
クライアントがRequest内にAPIキーを含めて送る認証方式で、仕込み先はヘッダーやクエリー、bodyなどサービスによって異なります。一般公開データに匿名でアクセスする場合に便利で広く利用されています。
非常にシンプルに実装できる一方、APIキーはクライアントからアクセス可能で簡単に盗まれる可能性があったり、キーがあれば誰でも認証を通過できることから、安全性が低い認証方式です。

## 参考
- [OAuth 2.0 + OpenID Connect のフルスクラッチ実装者が知見を語る](https://qiita.com/TakahikoKawasaki/items/f2a0d25a4f05790b3baa)
- [OAuth & OpenID Connect 関連仕様まとめ](https://qiita.com/TakahikoKawasaki/items/185d34814eb9f7ac7ef3)
- [OAuthの概要とそれを取り巻く環境](https://www.nttpc.co.jp/technology/oauth.html)
- [「OpenID Connect」を理解する](https://www.atmarkit.co.jp/ait/articles/1209/27/news138.html)
- [【OAuth 2.0 / OIDC】アクセストークンとIDトークンの違い ＋ OIDC誕生の歴史](https://yyh-gl.github.io/tech-blog/blog/id_token_and_access_token/)
- [セキュリティトークンとは？仕組みやメリット、種類について徹底解説](https://cybersecurity-jp.com/security-measures/30358)
- [Web API認証方式のパターン](https://architecting.hateblo.jp/entry/2020/03/27/033758)
