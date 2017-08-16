title: MySQLパフォーマンスチューニング
date: 2017-08-06 00:00
tags: DB

---

開発時にMySQLのパフォーマンスで気を付けてることのまとめました。

- メモリ設定
- テーブル定義
- クエリー発行
- アプリケーション側
- まとめ


## メモリ設定

MySQLは、データを保存するために利用するメモリとして
データキャッシュ(バッファプール)、ログバッファの2つ持ってます。

データキャッシュはSELECT文のキャッシュするための領域で
ログバッファは更新処理(INSERT, DELETE, UPDATE, MERGE)時の依頼を溜めてる領域です。
更新処理は即時反映でなくログバッファに溜めて、あとでまとめてストレージに書き込んでるので。

上記を踏まて、目的によって以下のデフォルトメモリのサイズをチューニングします。

- データキャッシュ(バッファプール)
  - `innodb_buffer_pool_size`: 128Mバイト
- ログバッファ
  - `innodb_log_buffer_size`: 8Mバイト

ログバッファのチューニングとしては、通常のWebアプリケーションでは
一気に8M以上の書き込まれることは無いと思うので必要ないかなって印象です。
逆に更新処理が多いバッチ系のアプリケーションであれば調整すると効果が期待できそうです。

データキャッシュでは、単独のDBサーバだったら物理メモリの60〜70%ぐらい割り当てて
あとは、DBサーバのメモリー使用量で枯渇しないかなど見ながら微調整していきます。

## テーブル定義

### INDEXについて

#### ・INDEX効果

TABLEに対して発行される、SELECT句の種類に応じたINDEXは作らずINDEX効果が期待できるもののみを厳選します。

一般的には、INDEX効果が効く場合としては検索結果数が全体の5〜10%前後というのが目安で
それ以上の結果が返ってくる場合は大きな効果を期待できずテーブルフルスキャンの方が速いと言われてたりします。
とはいえ、WHERE句によって検索結果数は変わることがほとんどなので、
大規模なデータ量となるテーブルなのか、もしくはカーディナリティ(値の種類がどれくらいかの指標)が
比較的高い検索結果になるのかでINDEXを貼る価値を考えたりします。

#### ・INDEX数

INDEXを貼ると「INSERT, UPDATE, DELETE」の速度が低下するので更新頻度が多いテーブルほどINDEXは少いほうがいいです。
データ量とかにもよるけど、テーブルあたり4つぐらいまでがひとつの目安と思ってて
これ以上貼ってたら、ちょっときな臭いと思ってボトルネックになっていないか調査してみていいと思います。
あと、INDEX数が多くなるとデータ量が増えるのでデータキャッシュに載せれるデータが減ってしまうデメリットもあります。

#### ・INDEX確認

たまにUNIQUE制約とINDEXを同時に同一カラムに定義してることがあるので
TABLEを作成した後は、INDEXが重複して定義していないか確認します。
`SHOW INDEX FROM <TabelName>;`

### パーティショニング

実際の運用ではデータが青天井で勢いよく増加つづけるテーブルが出てきます(解析系や履歴系とか)
このようなテーブルはデータ量に比例してパフォーマンスが落ちるのでパーティショニングします。

よく利用するパーティショニングの種類は、Rangeパーティショニングで`id`や`date`を利用して
テーブルの1日のデータ増加量よって単位は異りますが100万〜1000万ぐらい間でパーティショニングを切ってました。
パーティショニングを分ける単位の基準としては、1日のデータ増加量から逆算して2ヶ月くらいで
1パーティションを使いきるぐらいを目安としていました。

## クエリー発行

### INDEXが期待できる条件を理解しておく

#### ・INDEXが効くクエリー

- =, >, <, AND, IN, ORDER BY, GROUP BY, BETWEEN
- LIKE述語の前方一致のみ
  - `SELECT * FROM account WHERE name like 'fuji%';`
- 索引列で右側で演算や関数を行っている場合
  - `select * from account where 50 > point * 1.1;`

#### ・INDEXが効かないクエリー

- 否定形(<>, !=, NOT IN)
- LIKE述語の中間一致、後方一致
  - `SELECT * FROM account WHERE name like '%fuji%';`
  - `SELECT * FROM account WHERE name like '%fuji';`
- WHERE句で左側で演算や関数を行っている場合
  - `SELECT * FROM account WHERE point * 1.1 > 100;`
  - `SELECT * FROM account WHERE LENGTH(point)b = 10;`
- WHERE句でIs Null述語を使ってる
  - `SELECT * FROM account WHERE name IS NULL;`

### *(ワイルドカード)のSELECT句

カラム数が多いテーブル or 大きなデータを持ってるカラムが存在するテーブルなどでは
「*」を指定するのはできるだけ避けて必要なカラムのみを定義をするようします。
この手のSELECT句はIOにとても時間を取る上、データキャッシュに無駄なリソースが取られてしまいます。

### JOINの仕組みを理解しておく

JOINを利用する場合は仕組みを理解しておくことが重要と思ってます。
MySQLのJOINアルゴリズムにはNested Loop Join(NLJ)が採用されてます。
NLJは、外部表(JOINする側)が内部表(JOINされる側)を1行ずつループして処理するアルゴリズムで
JOINの回数が増えると急激に重くなります。

外部表100行と内部表100行の状態でINDEXが効かないJOINした場合
`100行 + 100行x100行 = 10100行`のような計算量になります。
逆に外部表と内部表に一意なINDEXが効いた状態でJOINした場合
`100行 + 100行(100x1) = 200行`となり無駄なく高速に処理できます。

JOINは外部表と内部表のかけ算になるのでパフォーマンスの観点から見ると
一意なINDEXが効いている前提で利用するのがベターと思ってます。
多段JOINや一意なINDEXが効かないJOINはついてはバッチ処理など
パフォーマンスを求めれない処理などであれば利用するのはありかと思います。

### サブクエリーの使いどこ

まず、サブクエリーは以下の問題点があるので積極的には使われない傾向があります。

- 可読性が悪い
- SELECT句にサブクエリー分のコストが上乗せされるので実行コストが増える
- サブクエリー結果からINやテーブル結合する場合、サブクエリー結果にインデック情報などを持たないので最適なパフォーマンスを得られない
- サブクエリー結果がすべてメモリに乗らなかった場合はSwapが発生し、急激な性能劣化が起きる
- サブクエリーが実行されて初めて結果がわかるので、事前にEXPLAINで実行計画を確認できない

ということ問題があるので、SQL側で頑張って1クエリーで完結させようとせずに
アプリケーション側から素直に本クエリー分とサブクエリー分を発行して
データを組み立てた方がシンプルに読みやすいコードになります。

#### ・サブクエリーでパフォーマンスを改善できる場合

サブクエリーでパフォーマンスを改善の期待できるのは、
テーブル結合を行う前に対象行数を小さく絞り込むことができる場合です。

典型的なケースでは、1:Nの関係持った所属テーブルとユーザテーブルがある状況下で
所属毎のユーザ数を取得したいケースがあります。

- パターン1: 所属テーブルとユーザテーブルをJOINしてからGROUP BYで絞る
```sql
SELECT
  g.id,
  g.name,
  SUM(u.group_id) AS user_count
FROM
  `group` AS g
INNER JOIN
  user AS u
ON
  g.id = u.group_id
GROUP BY
  g.id
```
  
- パターン2: サブクエリで事前にユーザテーブル対象を絞ってから所属テーブルとをJOINする
```sql
SELECT
  g.id,
  g.name,
  u.user_count
FROM
  `group` AS g
INNER JOIN
  (SELECT
     group_id,
     SUM(group_id) as user_count
   FROM
     user
   GROUP BY
     group_id) AS u
ON
  g.id = u.group_id
```

パターン1とパターン2では結合行数が異ります。
パターン1では、ユーザテーブルのレコード数分の結合が行われますが
パターン2では、サブクエリで事前にユーザテーブルの結合対象を所属テーブル分に絞ってます。
そのため、パターン2の方が結合コストが低くパフォーマンス改善が期待できるようになります。

ただ、数百万件ぐらいの件数がないと、パターン1 < パターン2のパフォーマンスにならないと思います。
パターン2は結合コストが低いもののクエリー2回発行してて、パターン1は1クエリーしか発行してないので
件数が少いケースではパターン1が有利になります。


## アプリケーション側

### アプリケーション側でグルグル検索をしない(N+1)

for文とかで都度クエリーを発行するとDBへの負荷が高いです。
解決策のアプローチとしては、JOINで1回のクエリーで取得する方法がありますが
多段JOINで複雑なSQLになる場合は、アプリケーション側で`IN`を利用したクエリー結果で
データを組み立てるのも1つの手です。(コードの文法は適当です)

- Before
```python
# usersの回数分、SELECTが2回づつ発行される
for user in users:
    group = `SELECT * FROM group WHERE user_id = user.id`
    detail = `SELECT * FROM user_detail WHERE user_id = user.id`
```

- After
```python
# SELECTが2回だけしか実行されない
all_groups = {g.user_id: g for g in `SELECT * FROM group`}

user_ids = [user.id for user in users]
user_details = {d.user_id: d for d in `SELECT * FROM user_detail WHERE user_id IN (user_ids)`

for user in users:
    group = all_groups[user.id]
    detail = user_details[user.id]
```

### アプリケーション側のキャッシュを利用する

DBへのクエリーを減らすためにアプリケーション側のミドルウェア(redisやmemcachedなど)
を利用してクエリー結果をキャッシュします。

よくキャッシュ対象としてされるのは
- 頻繁に発行されるクエリー
- Primaryキーでの参照クエリー
- fixtureデータ系のフルスキャンクエリー
- 時間がかかる重いクエリー
などがあります。

基本はリードスルーのみでキャッシュ生成を行います。
ライトスルーは個人的には限定的パターンしか使いません。
というのも、ライトスルー対応をするとロールバックしたときの対応などを考慮して
トランザクションの一番最後にキャッシュ生成ポイントを仕込まなければならず
アプリケーションの実装が複雑になりがちになります。
また、デグレでDBとキャッシュのデータ不整合が起きやすくなります。
そのため、重いクエリーが発行されれるのでパフォーマンス的にどうしても
キャッシュで簡潔したい場合のみに限定して利用しています。

### SQL確認

アプリケーション側のデバッグツールでもSQLは確認できますが
一部クエリーをフィルターして出力してたりするので、DBの生SQLを確認したほうがよかったりします。

確認するポイントとしては、
- 1リクエストで何回クエリーが発行されるかを見ます(30回以上であれば改善します)
- 1リクエストで重複してるクエリーを発行していないか確認します
- スロークエリーが発行されていないか確認します。(1秒以上のものがあれば改善します)

MySQLの設定ファイルに以下を定義するとtmpから生SQLが確認できるようになります。

```sh:my.cnf
[mysqld]
general_log=1
general_log_file=/tmp/sql.log
slow_query_log=1
slow_query_log_file=/tmp/slow_sql.log
long_query_time = 1
```

自分は開発中、`tail -f`でこのログを確認しながらパフォーマンス確認をしてます


## まとめ

開発する際にパフォーマンスで気にしてることを棚卸し的に書いてみたけど、
実際はDBのI/Oを減らすことばかりに気に取られないようにしてる。

どちらかというと、アプリケーションコードの可読性や保守性の方を一番大事にしてて
パフォーマンス重視のクエリーのために複雑なSQLを書いたり、
複雑なORMを作ると読みとくのに時間を取られるようになるしメンテされなくなる。

なので、できるだけアプリケーションの実装はシンプルになるよう心掛けて
パフォーマンスに問題があればはじめて複雑なことを着手する感じです。


## 参考

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4873115892/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/41qHKrFZi0L._SL160_.jpg" alt="SQLアンチパターン" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4873115892/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">SQLアンチパターン</a></div><div class="amazlet-detail">Bill Karwin <br />オライリージャパン <br />売り上げランキング: 4,987<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4873115892/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

<br />

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4774173010/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51pl3HrLCjL._SL160_.jpg" alt="SQL実践入門──高速でわかりやすいクエリの書き方 (WEB+DB PRESS plus)" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4774173010/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">SQL実践入門──高速でわかりやすいクエリの書き方 (WEB+DB PRESS plus)</a></div><div class="amazlet-detail">ミック <br />技術評論社 <br />売り上げランキング: 44,873<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4774173010/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>
