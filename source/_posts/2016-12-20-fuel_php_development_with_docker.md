title: DockerでFuelPHPの開発環境構築
date: 2016-12-20 19:23:18
tags:
- PHP
- Docker
- AdventCalender

---
この記事は[DockerのAdventCalandar2016](http://qiita.com/advent-calendar/2016/docker)の20日目の記事です。
すでにさんざんありますがDockerでのFuelPHP開発環境の構築方法は紹介します。

今回のPHP開発環境は以下で実装します。
Webサーバ: Nginx
APP: FuelPHP, PHP-FPM
DB: MySQL
KVS: memcached, Redis

構築環境はMacのDocker-machine上にDocker-composeで上記の区切りでコンテナを別に立てます
あとDBとKVSについてはデータを永続化させるよう対応します。


## コンテナ構成

<img src="/image/docker/php_development_with_docker.png" width="728" height="532" />

NginxのコンテナにAppコンテナをマウントし、静的コンテンツへのアクセスはNginxで返します。
また、APPコンテナで生成されたPHP-FPMのUnixSocketもマウントしており動的コンテンツは
UnixSocketを通してPHP-FPMにリクエストする流れになります。
MySQLコンテナ、Redisコンテナ, MemcachedコンテナはAPPコンテナにlinkさせコンテナ同士で通信する形になり、
Storageコンテナは、MySQL、Redis, Memcachedのデータを持ちます


## ファイル構成

```
fuel_php_with_docker
│
├── README.md
├── app
│   ├── Dockerfile
│   ├── env
│   └── php-fpm.d
│       ├── docker.conf
│       ├── www.conf
│       └── zz-docker.conf
├── docker-compose.yml
├── fuel_project
│   └── ...
├── mysql
│   ├── Dockerfile
│   └── charset.cnf
├── nginx
│   ├── Dockerfile
│   ├── application.conf
│   └── nginx.conf
└── storage
    └── Dockerfile
```

各コンテナ別にディレクトリを作っていて、Dockerfileを準備してます。
fuel_projectディレクトリは、Appコンテナへマウントされます。

上記の全ファイルはgithubに置いてます
https://github.com/fujimisakari/fuel-php-docker.git


## docker-compose.yml

``` sh
fuel_app:
  container_name: "fuel_app"
  build: ./app
  restart: always
  env_file: ./app/env
  links:
    - fuel_mysql:db
    - fuel_redis:redis
    - fuel_memcached:memcached
  volumes:
    - /var/run
    - ./fuel_project:/usr/src/app
    - ./app/php-fpm.d:/usr/local/etc/php-fpm.d

fuel_nginx:
  container_name: "fuel_nginx"
  build: ./nginx
  restart: always
  ports:
    - "80:80"
  links:
    - fuel_app:app
  volumes:
    - ./nginx/nginx.conf:/etc/nginx/nginx.conf
    - ./nginx/application.conf:/etc/nginx/conf.d/application.conf
  volumes_from:
    - fuel_app

fuel_mysql:
  container_name: "fuel_mysql"
  build: ./mysql
  restart: always
  ports:
    - "3306:3306"
  volumes_from:
    - fuel_storage

fuel_redis:
  container_name: "fuel_redis"
  image: redis:latest
  restart: always
  ports:
    - "6379:6379"
  volumes_from:
    - fuel_storage
  command: redis-server --appendonly yes

fuel_memcached:
  container_name: "fuel_memcached"
  image: memcached:latest
  restart: always
  ports:
    - '11211:11211'
  volumes_from:
    - fuel_storage

fuel_storage:
  build: ./storage
```

PHP-FPMの設定ファイル郡はローカルで用意したものマウントさせました。
DockerでPHP-FPMをコンテナに分けて実行しようとすると以下のエラーが発生しました

- PHP-FMPプロセスが`fuel/app/logs`以下へログを書き込もうとするときに権限を持っておらずPermission deniedになる
- NginxとPHP-FMPをUnixSocketでストリームさせるとき、Nginxコンテナ側でUnixSocketファイルに対してアクセス権限なくてエラーになる

これらを踏まえてphp-fpm.dファイルを編集しました

www.confは以下となるよう編集。
```
# Dockerでは、MacからマウントされたファイルディレクトリはUID:1000 GID:staff(50)になりますが
# PHP-fmpプロセスは、デフォルトUID:www-data GID:www-dataになってるので権限エラーではじかれるため
user = 1000
group = staff

# UnixSocketを利用するため
listen = /var/run/php-fpm/php-fpm.sock
listen.mode = 0777
```

zz-docker.confは、UnixSocketを利用するのでlistenをコメントアウト
```
#listen = [::]:9000
```


## Appを起動させる

### 1. hostsファイルの更新
docker-machineのIPアドレスを毎回URLに直打ちしたくないので以下を追記

```
$ sudo vim /etc/hosts
192.168.99.100 docker-machine
```

### 2. Dockerのファイル郡を準備

```sh
git clone https://github.com/fujimisakari/fuel-php-docker.git
```


### 3. FuelPHPプロジェクトのセットアップ

FuelPHPはoilコマンド(管理コマンド)を利用して、FuelPHPのAPPを作成するので
oilコマンドのバイナリをインストールして、FuelPHPのAPPを作成します。

```sh
$ curl get.fuelphp.com/oil | sh
$ oil create fuel_project
```

このままでは、起動時にtimezoneの設定が参照できずエラーになるので`Asia/Tokyo`を追記します。

```diff
--- a/fuel_project/app/config/config.php
+++ b/fuel_project/app/config/config.php
@@ -93,7 +93,7 @@ return array(
         * default_timezone             optional, if you want to change the server's default timezone
         */
        // 'server_gmt_offset'  => 0,
-       // 'default_timezone'   => null,
+       'default_timezone'   => 'Asia/Tokyo',
```

### 4. 各コンテナの起動

すべてのコンテナをビルド付きで起動させます。(通常時は--buildは不要)

```sh
$ docker-compose up --build
```

`http://docker-machine/index.php` でアクセスできるようになる


## 参考
- http://qiita.com/kotarella1110/items/634f6fafeb33ae0f51dc
- https://serverkurabe.com/php-cgi-module/
