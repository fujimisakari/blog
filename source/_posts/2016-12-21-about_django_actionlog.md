title: django-actionlogの紹介
date: 2016-12-21 19:23:18
tags:
- Python
- AdventCalender

---

この記事は[DjangoのAdventCalandar2016](http://qiita.com/advent-calendar/2016/django)の21日目の記事です。
Djangoのサードパーティの[django-actionlog](https://pypi.python.org/pypi/django-actionlog)について紹介します。


## django-actionlogについて

django-actionlogはRequest毎に任意の出力形式でDjangoのシステム関連ログを残すことができます。

対応出力形式は以下の3つがあります
- 標準出力
- File出力
- fluentd


## ユースケース
大きなユースケースとしては以下で利用してます
- パフォーマンスログを残す
- User毎のアクセスログとして
- 任意のログを残す
- 開発時のrequest毎パフォーマンス確認

### パフォーマンスログを残す
django-actionlogを利用すると、リクエスト毎に以下のパフォーマンスログが残せます。

- 正常時
  - time
  - url
  - view_name
  - request_id
  - status_code
  - method
  - user
  - sql_count
  - sql_time
  - python_time
  - total_time

- エラー時
  - time
  - url
  - view_name
  - status_code
  - method
  - user
  - ex_type
  - ex_message

運用時ではログをElasticSearchなどに流してグラフ(kibanaとか)で可視化して確認するケースが考えられます。
このような情報はNewRelicでも確認できますが統計された情報としてでしか確認できません。
django-actionlogの場合、Request毎の情報を記録してるので同じURLのリクエストでもキャッシュが適用前と適用後の比較、
特定の条件時のみ多数クエリーが発行されるケースなどを確認することができるようになります。

### User毎のアクセスログ
ログはUser情報もあるのでUser毎のシステムへのアクセスログ、調査用として利用できます。
エラーなどが発生したとき、Userがアクセスして遷移してきた足跡を追跡したりします。

### 任意のログを残す
システムを運用してる最中に特定の条件時のみ、どのような変数が来ていたかなど確認したくなるケースがあります。
このような時は以下のような感じで任意のログの残すことが可能です。

```python
from django-actionlog import actionlog

if is_save():
    actionlog.log({'user': user.name, 'label_name': 'hoge_save', 'foo': 'bar', 'fizz': 'buzz'...})
```

実際に利用した用途として、かなりレアなケースでしか発生しないデータ不整合の調査をするため
周辺にactionlogを配置して問題発生時にlabel_nameで簡単にデータの状態を調査できるようにしました。

### 開発時のrequest毎パフォーマンス確認
開発時は`sql_count`、`sql_time`、`python_time`、`total_time`を確認するため、actionlogを標準出力するようにしてます。
この辺はdjango-debug-toolbarでも確認できますが、以下の理由でdjango-actionlogの方を利用しました
- django-debug-toolbarはajaxなどで非同期リクエストがあった時の計測ができない
- 確認のたびにtoolbarをポチポチクリックするのがめんどう
- 画面を常にフルで利用したい


## 導入手順

導入は簡単です。

pipでパッケージのインストール
```sh
$ pip install django-actionlog
```

settings.pyにmiddlewareの追加とactionlogの出力形式のいずれかを追記
```python
MIDDLEWARE_CLASSES = (
    ...
    'django_actionlog.middleware.ActionLogMiddleware',
    ...
)

# 標準出力の場合
ACTION_LOG_SETTING = {'handler_type': 'stdout'}
or
# ファイル出力の場合
ACTION_LOG_SETTING = {'handler_type': 'file', 'logfile': '/tmp/my_action.log'}
or
# fluentdの場合
# デフォルトでは、localhost:24224のfluentdエージェントへdjango.actionlogのタグでコネクションします
ACTION_LOG_SETTING = {'handler_type': 'fluentd'}
# fluentdエージェントをカスタマイズする場合
ACTION_LOG_SETTING = {'handler_type': 'fluentd', 'host': 'example.com', 'tag_name': 'my_service.foo'}
```

### User情報の注意点
デフォルトだと、`request.user`でObject参照して、そのObjectが`actionlog_name`プロパティを
持っていたら`user.actionlog_name`を参照し、無ければ`user.id`でUser情報を残す仕様なってます。

```python
login_obj_name = ACTION_LOG_SETTING.get('login_obj_name', 'user')
if hasattr(request, login_obj_name):
    login_obj = getattr(request, login_obj_name)
    login_check_func = ACTION_LOG_SETTING.get('login_check_func', 'is_authenticated')
    if getattr(login_obj, login_check_func)():
        if hasattr(login_obj, 'actionlog_name'):
            user_name = login_obj.actionlog_name
        else:
            user_name = login_obj.id
```

User情報が`request.user`以外で定義されてる場合は`ACTION_LOG_SETTING`にプロパティ名を追加します。
```python
ACTION_LOG_SETTING = {'login_obj_name': 'player', ...}
```

### ローカルで利用するとき注意点

ローカルで開発するときにdjango-debug-toolbarと併用はできません。
django-debug-toolbarでもDB関連データ(SQLのカウントや時間)を計測できますが
DB関連の計測はdjango.dbのconnection.cursorにCursorクラスをカスタマイズしたWrapperクラスを作成して参照を上書く必要があります。
そのため、django-debug-toolbarとdjango-actionlogを併用した場合ではどちら一方のWrapperクラスしか当てることはできないです。


## fluentd + Amazon Elasticsearch Service
実際の運用では、Amazon Elasticsearchと連動させてkibana上からアクセス調査、パフォーマンス確認を行ってます。
fluentdとElasticsearchを連携させたい場合はtd-agent.confに以下を追加します。

```sh
# Request毎のログ
<match django.actionlog.footprint>
  type "aws-elasticsearch-service"
  type_name "footprint"
  logstash_prefix footprint
  logstash_format true
  include_tag_key true
  reload_connections false
  tag_key "@footprint"
  flush_interval 3s

  <endpoint>
    url https://search-xxxxxxxxxxxxxxxxxx.ap-northeast-1.es.amazonaws.com
    region ap-northeast-1
    access_key_id xxxxxxxxxxxxxxxxxx
    secret_access_key xxxxxxxxxxxxxxxxxx
  </endpoint>
</match>

# 任意で仕込んだログ
<match django.actionlog.**>
  type "aws-elasticsearch-service"
  type_name "actionlog"
  logstash_prefix actionlog
  logstash_format true
  include_tag_key true
  reload_connections false
  tag_key "@actionlog"
  flush_interval 3s

  <endpoint>
    url https://search-xxxxxxxxxxxxxxxxxx.ap-northeast-1.es.amazonaws.com
    region ap-northeast-1
    access_key_id xxxxxxxxxxxxxxxxxx
    secret_access_key xxxxxxxxxxxxxxxxxx
  </endpoint>
</match>
```

## Kibanaでパフォーマンスの確認
パフォーマンスの確認はKibanaのダッシュボードに以下のグラフ作成して確認してます

<img src="/image/python/django-actionlog-sample.png" width="1028" height="632" />

左側はviews毎に時間がかかったリクエストを降順で一覧表示させて、そこから調べたいviewsを絞り
直近リクエストを比較して毎回この時間かかるのか特定のケースだけなのかを調査する感じになります。

右側はviews毎のアクセスランキングです。アクセス頻度が高いviewsがわかるので
パフォーマンス改修する際の優先度を付けやすくなります。
