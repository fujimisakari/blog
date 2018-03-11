title: ユーザー認証フローのまとめ
date: 2017-07-17 00:00
tags: Security

---

ユーザー認証関連で認証フローパターンやToken種類などいろいろあるけど
その時ググッて調べてもすぐ忘れてしまうので以下についてまとめみる。

- ユーザー認証パターン
- Access Tokenの種類
- ハッシュについて

## ユーザー認証パターン

よく見るサーバサイド認証フローを以下からの利用パターンで見てみる
- App(Naitive Application)
- Web(Web Application)
- SPA(Single Page Application)

### ID/PW

<img src="/image/security/id-pw.png" />

ID/PWはもっとも一般的なユーザー認証で認証時にAccess Token発行 or Session開始して通信を行います。
Appの場合では、発行されたAccess TokenをAuthorization Headerで利用してステートレスなAPI通信を行う。
Webの場合では、Sessionで認証を維持しCookie(SessionID)を利用してステートフルな通信を行う。
SPAの場合では、AccessToken、Sessionどちらでも実装可能。

### OAuth2

OAuth2を場合は、`Web/SPA`と`App`の場合で分けて説明します。

#### ・Web/SPAの場合

<img src="/image/security/oauth2.png" />

Webの場合、OAuth2で認可後はServer側でAccess Tokenを発行せずCookieを使って
Sessionで認証を維持しステートフルな通信を行うのがよく見るパターンです。
SPAの場合、Server側で発行したAccess Tokenを利用してステートレスなAPI通信、Sessionを使った通信どちらも実装可能です。

#### ・Appの場合

<img src="/image/security/oauth2-app.png" />

Native AppからOAuth2認証フローを踏むときはWeb/SPAとはアプローチが異ります。

iOSに限った話ですが、自前のアプリから認証時に自社サービスではない外部サービスの
ID/PWは入力させることはフィッシングに該当するので行うことはできません。
(WebViewを使って、Server側で用意してたOAuth2の認証ページを使おうとしたら審査で落ちました)

iOSではあらかじめ認証済みアカウント情報(FacebookやTwitter等)をローカルストレージに保持しているので、
そのアカウント情報を参照します(System設定で自前アプリからの参照を許可する必要がある)
そして、Sign up(or Login)時にそのアカウント情報をServerへ送り認可情報として扱い
必要があれば、Resource Serverへ追加情報を取得します。
それ以降は、Server側で発行したAccess Tokenを利用してステートレスなAPI通信を行います。
(もはやOAuth2認証していないですが...)

### OpenID Connect

<img src="/image/security/openid-connect.png" />

OpenID ConnectはOAuth2.0を拡張したプロトコルで認証フローもほぼ同じです。
大きな違いは、OAuth2.0が認可(リソースのアクセス権限付与)だったのに対して
OpenID Connectは認可・認証を行うことができ、認証についてはID Tokenよって実現します。

ID Tokenは、認証に関するClaim(属性情報) を含んだJson情報をJWT(JSON Web Token)でハッシュ化したものです。
Claim(属性情報)には、ユーザーの識別子や有効期限など含まれており、OpenIDProvider側の秘密鍵で
署名されたセキュリティトークンとなってます。
そのため、受け取ったID Tokenが偽装されていないか公開鍵で検証する必要があります。
この検証を通してユーザーがxxxさんである認証がすることできます。
App/Web/SPAの利用パターンとしては、OAuth2と同様かなと思ってます

## Access Tokenの種類

### ランダムな文字列

サーバでランダムな文字列を発行して、DB に保存し、Access Token として利用する方法です。
最低限、以下のような項目が必要になります。
- User ID
- Access Token
- 有効期限

ランダムな文字列は、ユーザ毎に規則性があるハッシュ値で生成すると脆弱性につながるため
ユーザ毎のハッシュ値 + 乱数saltを組み合わせてTokenを生成することが推奨されてます。
また、パフォーマンスよりセキュリティを重視するのであれば、
Tokenにストレッチング(ハッシュ計算を繰り返し行う手法)を適用することでより強固なTokenになります。

### JWT(Json Web Token)

JWT(ジョットと呼ぶ)は、署名付きJsonデータをハッシュ化しTokenとして認証に利用する方法です。
大きな特徴として、URL Safeであること(URLで利用できる文字列形式)と
DBなどのストレージにToken情報を保存する必要がないです。
Client側はJWT TokenをLogin時もしくは事前にリクエスト前に取得しておき、
Authorization HeaderもしくはCookieに含めServer側とのやりとりで認証に利用します。

ユースケースとしては、WebサイトなどClient側のローカルストレージにTokenが保存できない場合は
Cookieを介して、Naitive AppやSPA(React/Angulaer)などでローカルストレージに保存できる場合は
Authorization Headerを介して使われてます。

#### ・JWT Tokenの作成

payloadと秘密鍵を使ってJWTのアルゴリズムに基き署名付きのTokenを生成します。
(実際の運用ではPayloadに有効期限も含めます)

```python
>>> payload = {'user_id': 1, 'username': 'fujimisakari'}
>>> secret_key = 'abcdefg123'
>>> token = jwt.encode(payload , secret_key, algorithm='HS256').decode('utf-8')
>>> token
'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoxLCJ1c2VybmFtZSI6ImZ1amltaXNha2FyaSJ9.ym4QKCnJovRuos_aZJ5eMwVFBQXwDtJtb56wuozFKCs'
```

JWT Tokenは3つのハッシュ値を`.(ピリオド)`で結合されてものになります。
```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoxLCJ1c2VybmFtZSI6ImZ1amltaXNha2FyaSJ9.ym4QKCnJovRuos_aZJ5eMwVFBQXwDtJtb56wuozFKCs
------------------------------------|----------------------------------------------------|-------------------------------------------
             1.ヘッダ部                                  2.payload部                                        3.署名部
1: ヘッダーのbase64でエンコードしたもの
2: payloadをbase64でエンコードしたもの
3: ヘッダーとpayloadのハッシュ値を結合し秘密鍵でハッシュ化し、base64でエンコードしたもの
```

#### ・JWT Tokenの認証

JWT Tokenを使った認証は、Tokenからpayload部をbase64でデコードしJsonデータを復元します。
そして、そのjsonデータと秘密鍵を使ってリクエストで受け取ったJWT Tokenと同様のものかで認証します。

```python
>>> token
'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoxLCJ1c2VybmFtZSI6ImZ1amltaXNha2FyaSJ9.ym4QKCnJovRuos_aZJ5eMwVFBQXwDtJtb56wuozFKCs'
>>> payload = jwt.decode(token, None, False)
>>> payload
{'user_id': 1, 'username': 'fujimisakari'}
>>> secret_key = 'abcdefg123'
# 認証成功
>>> jwt.decode(token, secret_key, algorithms='HS256')
{'user_id': 1, 'username': 'fujimisakari'}
# 認証失敗
>>> jwt.decode(token, 'hogefugefizzbuzz1234', algorithms='HS256')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/Users/fujimisakari/.pyenv/versions/django-jwt/lib/python3.5/site-packages/jwt/api_jwt.py", line 78, in decode
    jwt, key=key, algorithms=algorithms, options=options, **kwargs
  File "/Users/fujimisakari/.pyenv/versions/django-jwt/lib/python3.5/site-packages/jwt/api_jws.py", line 135, in decode
    key, algorithms)
  File "/Users/fujimisakari/.pyenv/versions/django-jwt/lib/python3.5/site-packages/jwt/api_jws.py", line 206, in _verify_signature
    raise DecodeError('Signature verification failed')
jwt.exceptions.DecodeError: Signature verification failed
```

## ハッシュについて

あるデータが与えられ、そのデータからあるアルゴリズムでランダムな文字列を算出するまでのこと。
「あるアルゴリズム」がハッシュ関数で、MD5, SHA-1, SHA-512などの種類がある。
「ランダムな文字列」がハッシュ値と呼ぶ。

### ハッシュが持つ特性
- 同じデータを入力すると必ず同じハッシュ値を出力する
- どんなデータを入力しても決った長さのハッシュ値を出力する
- 別のデータに対してはほぼ別のハッシュ値を出力する
- ハッシュ値から元のデータを算出することは、ほぼできない

### ハッシュ使用時の注意

#### ・ファイルの同一性チェック
ハッシュ値は衝突する可能性があります。
この問題に対して対策は、ハッシュアルゴリズム単体ではとくにありません。
ですが、複数のアルゴリズム(MD5, SHA-1, SHA-256)で比較させて一致するなど工夫して同一性を担保することができます。

#### ・推測しにくいURLの生成
URLのパラメータにハッシュ値を指定した場合は、推測されやすいので注意が必要です。
このような問題には、本データにsalt(任意の長い文字列)を結合させてハッシュ化することで対策することができます。

#### ・ログインパスワードの保存
セキュリティレベルの低いパスワードが含まれている場合
攻撃者がハッシュ化されたパスワードのリストを入手したとき
よく利用されるパスワードリストから見つけられる可能性があります。

対策としては、こちらもsaltと結合させてユーザごとに計算した
ハッシュ値を保存することで対策することができます。

## 参考
- http://mnakajima18.hatenablog.com/entry/2016/05/04/205713
- http://qiita.com/ledmonster/items/0ee1e757af231aa927b1
- http://openid-foundation-japan.github.io/openid-connect-core-1_0.ja.html
- http://dev.classmethod.jp/server-side/json-signing-jws-jwt-usecase/
- https://www.slideshare.net/kura_lab/openid-connect-id?ref=http://mnakajima18.hatenablog.com/entry/2016/05/04/205713
