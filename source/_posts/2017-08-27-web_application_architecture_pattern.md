title: Webアプリケーションの設計パターン
date: 2017-08-27 00:00
tags:
 - Python
 - DesignPatterns

---

Webアプリケーションを開発する上での設計パターンついて
ある程度自分の中で知見がまとまったのでつらつら書いてみる。

ここで指してる設計パターンが何なのかというと、
機能を開発するための業務ロジック設計ではなく
システム全体の構造の大枠を定義するための設計パターンになります。


## なぜ設計が必要なのか

設計方針を決めないまま開発していると、個々それぞれの実装経験やドメイン知識などが異なるので
スキルに応じたフリーライティングとなり実装パターンがバラつきコードの可読性や保守性が下ってきてます。
これを防ぐために、システム構造の大枠の設計を定義します。

設計を導入することにより、どの機能はどこの書けばよいのかが明確になるので
誰が実装してもある程度同様の実装方針となります。
また、局所的な視点から見て不恰好なコードがあっても、
全体的な視点から見ればコードのレイアウトがきれいに整った構造になるので
設計方針さえブレなれば開発規模を拡大しても可読性や保守性は保持することができます。


## 現在の設計が機能しているかの判断ポイント

自分の経験上ですが、業務で扱う設計は以下のケースがあると思います
- 設計パターンを組み合せて利用してるケース
- 設計パターン + オレオレ設計のケース
- 完全オレオレ設計のケース
- そもそも設計自体を入れていないケース

設計自体を入れていないケース以外はどれが良いケースというのはケースバイケースなので明言できませんが
個人的には以下について思う節があった場合は設計があまりうまく機能していないと思ってます
- 新しいドメインロジックを追加しようと思ってるけど何処に定義すればいいか迷ったりわからない
- 参照方法が幾通りある(例えば、DBデータを取得するのに、domain経由だったり、model経由だったりしてる)
- 重複してるコードが多いと感じる
- 各レイヤ間やドメイン間の依存関係が多い
- コントローラに業務ロジックなどのベタ書きが多いと感じる
- この処理は何処に定義するなどのルールが決まっていない


## 利用している設計について

チーム開発では王道の「レイヤー化アーキテクチャ」を利用してます。

レイヤ化とは、依存の原則を強い制約を適用したアーキテクチャです。
機能をはっきり定義されたモジュールに分割し、明快に設計されたインタフェースを
モジュール間に定めることで依存関係も最小限に収めることができます。

大きな特徴としては
- 上位下位の関係になるように全体を分割する
- 上位レイヤは直下のレイヤにしかアクセスしないよう強制する
- 双方向の依存は禁止

こうすることで依存方向を一方向に限定できます
直下レイヤを飛び越えた下位レイヤへのアクセスは禁止です
当然ながら上位レイヤーへのアクセスも禁止です
また下位レイヤーを安定するように設計することで安定したほうへの依存も達成できます。

### レイヤ化では、主な3つのレイヤに分けられます

#### ・プレゼンテーションレイヤ
WebアプリケーションではHTTP処理を行うレイヤになります。
リクエストを受けてテンプレートへレンダリング、レスポンスを返す一連の流れを実装します。
処理的なとこでは、Get、Postパラメータの取得、リダイレクト処理などHTTP関連を扱い
ビジネスロジックはこのレイヤーでは定義せずHTTPの処理の流れのみがシンプルに確認できるようにします。

#### ・ドメインレイヤ
データベースへのデータの登録、取得などの操作を行ったり、
あるいはデータの計算、整形などシステム機能の中核ロジックを定義する場所です。
ドメインロジックやビジネスロジックとも呼ばれコード量も多く一番複雑になるレイヤです。

#### ・データソースレイヤ
データベースへのアクセスロジック、取得したレコードのエンティティオブジェクト化、
SQLを隠蔽するマッパ機能を実装するレイヤで、リポジトリとなるデータへの操作を直で行う処理が定義されます。


## レイヤー化アーキテクチャの設計パターンについて

上記でレイヤー化アーキテクチャについての概要を書きましたが
各レイヤ毎に設計パターンがいくつか存在します。
そして、システム構造の大枠の設計はこのパターンの組み合せで成り立っています。

実際にはフレームワーク自体がすでに設計パターン取り入れてる場合もありますが
設計パターンを知っておくと全体的なコードのレイアウトを揃えるために
何が必要なのかを気付くことができたりもします。

### プレゼンテーションレイヤ

このレイヤのパターン詳細は割愛します
というのも、Webアプリケーションを開発する際は何かしらのフレームワークは利用すると思いますが
そのフレームワークがコントローラやビューのいずれかの設計パターンを実装されていることがほとんどですので
意図的に設計パターンを変更する機会はそこまでないのではっという感じです

- コントローラ系のパターン
  - モデルビューコントローラ
  - ページコントローラ
  - フロントコントローラ

- ビュー系のパターン
  - トランスフォームビュー
  - テンプレートビュー
  - ツーステップビュー

### ドメインレイヤ

日本語訳では問題領域とされてて、非常に抽象的な表現なのですが
責務、役割、関心事などの単位で機能(ドメインロジック)を実装するレイヤです。
プレゼンテーション層やデータソース層から分離して実装するので外部の層から影響されてはならず
ドメインでカプセル化された機能のみに注目して実装を行なわなければならないです。

実装では機能として粒度の大きな単位をドメインモジュールで分割していき
その中でさらに粒度の小さいドメインロジックをコンポーネントとして分けていきます。
ディレクトリ構造もプロジェクトによってまちまちだと思いますが良く見る構造だと

**横割りのフラット構造**
```
domain
├── module1
│   ├── component1-1(service)
│   ├── component1-2
│   └── component1-3
├── module2
│   ├── component2-1(service)
│   └── component2-2
└── module3
    ├── component3-1(service)
    ├── component3-2
    ├── component3-3
    └── component3-4
```

**横割りのネスト構造**
```
domain
├── module1
│   ├── component1-1(service)
│   ├── component1-2
│   └── component1-3
├── module2
│   ├── component2-1(service)
│   ├── component2-2
│   └── module3
│       ├── component3-1(service)
│       ├── component3-2
│       └── module4
│           ├── component4-1(service)
│           └── component4-2
└── module5
    ├── component5-1(service)
    ├── component5-2
    └── component5-3
```

**縦割り構造**
```
domain
├── service
│   ├── component1
│   ├── component2
│   └── component3
├── model
│   ├── component1
│   ├── component2
│   ├── component3
│   ├── component4
│   └── component5
├── entity
│   ├── component1
│   ├── component2
│   ├── component3
│   └── component4
└── dao
    ├── component1
    ├── component2
    └── component3
```

とかがあります。
ドメインレイヤは複雑性が高くなりがちで設計時点では今後どのような利用のされ方になるかを測ることが難く
王道のパターンなども無いので、チームの方針や設計者の経験よって採用されてる感じです。

ドメインのパターンは以下に分類されます

#### ・トランザクションスクリプト
これパターンは、ドメインロジックを一連の手続きで定義していくパターンとなります。
もっともシンプルで一番良く見るパターンではないでしょうか。
個人としては、ドメインのインタフェースとなるサービスレイヤをこのパターンで実装してます。
サービス内の一連の手続きの流れの中はほぼドメインモデルの呼び出しに集約させて、
複雑にならないよう極力ビジネスロジックは定義せずに実装します。

```py
def get_all_player_cards(player):
    cards = {}
    cards['attack'] = AttackPlayerCard.get_cards(player)
    cards['defense'] = DefensePlayerCard.get_cards(player)
    cards['special'] = SpecialPlayerCard.get_cards(player)
    return cards

def get_strongest_attack_player_card(player):
    player_cards = get_player_cards(player)
    player_cards.sort(key=operator.attrgetter('attack'), reverse=True)
    return player_cards[0]
```

#### ・ドメインモデル
このパターンは、目的や役割、関心などの括りをドメインモデルとして切り分けてオブジェクトで扱います。
そのため、データをモデル化するオブジェクトもあれば、ビジネスで使用するルールを把握するオブジェクトなどもあります。

このドメインモデルの実装は結構難易度が高いです。
あまりドメイン知識がなく目的や役割、関心などでドメインモデルを切り分けていっても
ただ依存関係が絡み合った結合度が高いカオスな構造が出来上ってしまいます。
ドメインモデルベースで実装する場合は、まずデザインパターン形式に落しながら
ドメイン知識を身に付けて実装していくのがベターかなと思います。
あとドメインモデルを使う際は、サービスレイヤをセットで使用するようにします。

```py
# Service
def send_mail(user, message):
    mailhandle = MailHandle()
    mailhandle.add(AdminMailHandler(user.name, message))
    mailhandle.add(UserMailHandler(user.name, user.email))
    mailhandle.send()


# Domain Model
class MailHandle(object):

    def __init__(self):
        self.mail_handlers = []

    def add(self, mail_handler):
        self.mail_handlers.append(mail_handler)

    def send(self):
        mailer = MailService()
        for m in self.mail_handlers:
            to_address = m.get_to_address()
            from_address = m.get_from_address()
            title = m.get_title()
            body = m.get_body()
            mailer.async_send(to_address, from_address, title, body)


class MailStrategy(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def get_to_address(): pass

    @abstractmethod
    def get_from_address(): pass

    @abstractmethod
    def get_title(): pass

    @abstractmethod
    def get_body(): pass

class AdminMailHandler(MailStrategy):

    def __init__(self, user_name, msg):
        self.user_name = user_name
        self.message = msg

    def get_to_address():
        return 'admin@xxxx.com'

    def get_from_address():
        return 'support@xxxx.com'

    def get_title():
        return '問い合わせがありました'

    def get_body():
        return '{} 様より 問い合わせがありました \n {}'.format(self.user_name, self.message)

class UserMailHandler(MailStrategy):

    def __init__(self, user_name, to_address):
        self.user_name = user_name
        self.to_address = to_address

    def get_to_address():
        return self.to_address

    def get_from_address():
        return 'support@xxxx.com'

    def get_title():
        return '問い合わせ受け付けました'

    def get_body():
        return '{} 様 \n 問い合わせ受け付けました'.format(self.user_name)
```

#### ・テーブルモジュール
このパターンは、データベース内のテーブルをドメインクラスで模倣するオブジェクトを用意するパターンです。
テーブルモジュールの特徴は、テーブルモジュールのオブジェクトが一意性の概念を持たないことです。
メンバ変数にテーブルレコード相当に該当するデータセット情報を保持しており、
このデータセット情報に対して、findやcreate, udpate, deleteなど振る舞い用意してアクセスすることになります。

テーブルモジュールは複雑なビジネスロジックを実装するは苦手ですが、異なるデータソースからのデータを
1つのオブジェクトで扱えたりするので、データ操作が中心の振る舞いであれば得意です。
個人的なとこでは、RDBを利用した実装ではドメインモデルとアクティブレコードを組み合わせるパターンがほとんどで
テーブルモジュールはNoSQLなどデータスキーマを持たないデータがリポジトリの場合に使ったりします。

```py
# Domain Layer
class UserTableModule(object):

    def __init__(self, user):
        self._user = user
        self._profile_kvs = ProfileKVS(user)
        self._favorite_kvs = FavoriteKVS(user)

    @property
    def user(self):
        return self._user.to_dict()

    @property
    def profile(self):
        return self._profile_kvs.get()

    def create_profile(self, birthday, living, gender, comment):
        init_data = {'birthday': birthday,
                     'living': living,
                     'gender': gender,
                     'comment': comment,
                     'type': 1,
                     'created_at': datetime.datetime.now().strftime('%Y-%m-%d %H:%M'),
                     'updated_at': datetime.datetime.now().strftime('%Y-%m-%d %H:%M')}
        self._profile_kvs.save(init_data)

    def update_profile(self, profile_data):
        profile_data['updated_at'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
        self._profile_kvs.save(profile_data)

    def get_favorite(self, data_id):
        return self._favorite_kvs.get_by(data_id)

    def get_favorites(self):
        return self._favorite_kvs.get_list()

    def create_favorite(self, target_id):
        init_data = {'data_id': str(uuid4()),
                     'target_id': target_id,
                     'created_at': datetime.datetime.now().strftime('%Y-%m-%d %H:%M')}
        return self._favorite_kvs.save(init_data)

    def delete_favorite(self, data_id):
        return self._favorite_kvs.delete(data_id)


# DataSource Layer
class ProfileKVS(object):

    def __init__(self, user):
        self._kvs = user.get_kvs(KVS_PROFILE_KEY)

    def get(self):
        return self._kvs.get()

    def save(self, profile_data):
        self._kvs.set(profile_data)

class FavoriteKVS(object):

    def __init__(self, user):
        self._kvs = user.get_kvs(KVS_FAVORITE_KEY)

    def get_list(self):
        return self._kvs.get([])

    def get_by(self, data_id):
        return self._kvs.get(data_id, None)

    def save(self, data_id, data):
        data_map = self._kvs.get()
        data_map[data_id] = data
        self._kvs.set(data_map)

    def delete(self, data_id):
        data_map = self._kvs.get()
        if data_map.get(data_id, False):
            del data_map[data_id]
        self._kvs.set(data_map)
```

#### ・サービスレイヤ
サービスレイヤはドメインロジックを構成する1つでドメインレイヤのインタフェースとなる部分で利用されます。
このレイヤを導入する背景には以下が考えられます
- ドメインモジュール内のコンポーネントを外部と依存(直接参照)させないようにする
- 純粋なドメインロジックに外部レイヤからの呼び出しロジックも共存させると再利用の可能性が低下する

サービスレイヤの実装パターンは2種類あります

**ドメインファサード手法**
ドメインモデル上の薄いファサードのセットとして実装します。
ビジネスロジックはすべてドメインモデルによって実装されるので
サービス内ではビジネスロジックは一切実装しません。

**操作スクリプト手法**
外部レイヤからの利用できる操作をスクリプトとして実装します。
この手法では、ビジネスロジックは実装できるものの薄いロジックのみにとどめて
重いドメインロジックはカプセル化したドメインモデルなどに任せます。

### データソースレイヤ

#### ・テーブルデータゲートウェイパターン

テーブルごとにデータへアクセスするためオブジェクトを用意するパターンです。

- 特徴としては
  - テーブルごとに1つのインスタンスを所持して、このインスタンス以外からデータへアクセスは行わない
  - データベースからデータを取得するための数種類のfind、update、 insert、 deleteなどのメソッドから構成されるインタフェースを備えている
  - データの受け渡しのみが役割であるためステートレスです
  - データにアクセスするオブジェクトの振る舞いから短縮してDAOパターンとも呼ばれるてます
  - ドメインパターンのテーブルモジュールパターンと組み合わせて利用されるケースが多いです

```py
class PersonGateWayMixin(object):

    @classmethod
    def _query_with_exception(cls, sql):
        try:
            cls.db.execute(sql)
        except:
            return False
        return True

    @classmethod
    def find_all(cls):
        sql = 'SELECT * FROM person'
        rows = cls.db.execute(sql)
        return rows if rows else []

    @classmethod
    def find(cls, id):
        sql = 'SELECT * FROM person WHERE id = {}'.format(id)
        rows = cls.db.execute(sql)
        return rows[0] if rows else None

    @classmethod
    def find_by_age(cls, age):
        sql = 'SELECT * FROM person WHERE age = {}'.format(age)
        rows = cls.db.execute(sql)
        return rows if rows else []

    @classmethod
    def create(cls, **kwargs):
        sql = 'INSERT INTO person VALUES ({}, {}, {})'
        sql = sql.format(kwargs['lastname'], kwargs['firstname'], kwargs['age'])
        return cls._query_with_exception(sql)

    @classmethod
    def update(cls, **kwargs):
        sql = 'UPDATE person SET lastname = {}, firstname = {}, age = {} WHERE id = {}'
        sql = sql.format(kwargs['lastname'], kwargs['firstname'], kwargs['age'], kwargs['id'])
        return cls._query_with_exception(sql)

    @classmethod
    def delete(cls, id):
        sql = 'DELETE FROM person WHERE id = {}'.format(id)
        return cls._query_with_exception(sql)
```

#### ・行データゲートウェイ

テーブルの1レコードを模倣するオブジェクトを用意するパターンです

- 特徴としては
  - テーブルの1レコードごとに1インスタンスを生成します
  - オブジェクトにはテーブルの列と紐づくデータフィールドとアクセッサが定義されている
  - オブジェクトの振る舞いは、insert, update, deleteを持ちます
  - オブジェクトの振る舞いには、ドメイン系のロジックは定義しません。

**find(検索)について**
このパターン内を利用する際にfind(検索)をどこに定義するかという問題があります。
明確な答えはないのですが考え方としては、以下のパターンで実装されてる傾向です
- 行データゲートウェイオブジェクトに静的findメソッドを追加して実装
- findオブジェクトを定義して結果を行データゲートウェイオブジェクトにバインドさせる

```py
# Domain(Service) Layer
def get_person(id):
    return PersonFinder.find(id)

def get_person_by(age):
    return PersonFinder.find_by(age)


# DataSource Layer
class PersonFinder(DBdriver):

    @classmethod
    def find(cls, id):
        sql = 'SELECT * FROM person WHERE id = {}'.format(id)
        rows = cls.db.execute(sql)
        return PersonGateway(rows[0]) if rows else None

    @classmethod
    def find_by(cls, age):
        sql = 'SELECT * FROM person WHERE age = {}'.format(age)
        rows = cls.db.execute(sql)
        if not rows:
            return []
        return [PersonGateway(r) for r in rows]

class PersonGateway(DBdriver):

    def __init__(self, **kwargs):
        self.id = kwargs.get('id', None)
        self.lastname = kwargs['lastname']
        self.firstname = kwargs['firstname']
        self.age = kwargs['age']

    def _query_with_exception(self, sql):
        try:
            self.db.execute(sql)
        except:
            return False
        return True

    def create(self):
        sql = 'INSERT INTO person VALUES ({}, {}, {})'
        sql = sql.format(self.lastname, self.firstname, self.age)
        return self._query_with_exception(sql)

    def update(self):
        sql = 'UPDATE person SET lastname = {}, firstname = {}, age = {} WHERE id = {}'
        sql = sql.format(self.lastname, self.firstname, self.age, self.id)
        return self._query_with_exception(sql)

    def delete(self):
        sql = 'DELETE FROM person WHERE id = {}'.format(id)
        return self._query_with_exception(sql)
```

#### ・アクティブレコード

行データゲートウェイパターンとほぼ同じです。
異なる点としては、ドメインロジックを定義することが可能というです。
そのため、findメソッドやテーブルデータを処理するビジネスロジックを定義したりします。

```py
class PersonGateway(DBdriver):

    def __init__(self, **kwargs):
        self.id = kwargs.get('id', None)
        self.lastname = kwargs['lastname']
        self.firstname = kwargs['firstname']
        self.age = kwargs['age']

    @property
    def fullname(self):
        return '{} {}'.format(self.lastname, self.firstname)

    def _query_with_exception(self, sql):
        try:
            self.db.execute(sql)
        except:
            return False
        return True

    def create(self):
        sql = 'INSERT INTO person VALUES ({}, {}, {})'
        sql = sql.format(self.lastname, self.firstname, self.age)
        return self._query_with_exception(sql)

    def update(self):
        sql = 'UPDATE person SET lastname = {}, firstname = {}, age = {} WHERE id = {}'
        sql = sql.format(self.lastname, self.firstname, self.age, self.id)
        return self._query_with_exception(sql)

    def delete(self):
        sql = 'DELETE FROM person WHERE id = {}'.format(id)
        return self._query_with_exception(sql)

    @classmethod
    def find(cls, id):
        sql = 'SELECT * FROM person WHERE id = {}'.format(id)
        rows = cls.db.execute(sql)
        return cls(rows[0]) if rows else None

    @classmethod
    def find_by(cls, age):
        sql = 'SELECT * FROM person WHERE age = {}'.format(age)
        rows = cls.db.execute(sql)
        if not rows:
            return []
        return [cls(r) for r in rows]
```


## まとめ

ここまでPofEAA(Patterns of Enterprise Application Architecture)に元づいた設計パターンを
自分の経験を踏まえて紹介してきましたが設計手法はこれ以外にもたくさんあります。
すべて覚える必要はありませんが代表的なものをいくつか知っておくことで他の設計パターンに出会った時に
そのパターンの特徴を早く抑えることができコード全体の規則性に気付くことができるようになります。
規則性を理解できるようになると見える世界も変ってくるので、どこにロジックをかけばよいのかが
明確にイメージできるようになり悩んだりする時間も短縮されるのでコーディングの速さも上がります。
このように設計を理解していくと、これまで見えていなかった視野が広がったことを実感できるようになりました。
そして、こういった技術は陳腐化することはなく、どの言語でも共通して通じるのでものですので
学習する価値は非常に高いものと感じてます。


## 参考

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4798105538/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51TVM1CFHKL._SL160_.jpg" alt="エンタープライズ アプリケーションアーキテクチャパターン (Object Oriented SELECTION)" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4798105538/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">エンタープライズ アプリケーションアーキテクチャパターン (Object Oriented SELECTION)</a><div class="amazlet-detail" style="margin-top:20px;">マーチン・ファウラー <br />翔泳社 <br />売り上げランキング: 322,401<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4798105538/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>
