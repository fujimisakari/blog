title: TCPのSocket Closeでハマった
date: 2020-01-25 00:00
tags:
- Go
- TCP

---

TCP通信開発に慣れていないのとTCP状態遷移を理解できていなかったのでSocket Closeしようとしてハマってしまった。

前提
- やろうとしていたこと
  - TCP ServerとClientを開発していて、Serverが落ちたときClientからリトライで再接続するような仕組み
- ハマったっこと
  - Clientからリトライで再接続するとき、`bind: address already in use` のエラーで失敗し続けてしまい原因がわからなかった

## 再現させるために同等のミニマムコードを用意

### Server側
Clientからの接続を受けてすぐCloseさせる
```go
package main

import (
	"fmt"
	"net"
)

func main() {
	listener, _ := net.Listen("tcp", ":7070")
	for {
		conn, err := listener.Accept()
		if err != nil {
			fmt.Println("failed to connection")
			continue
		}
		conn.Close()
	}
}
```

### Client側
Serverへ2回接続をトライする
```go
package main

import (
	"fmt"
	"net"
	"time"
)

func newConn() {
	remote, _ := net.ResolveTCPAddr("tcp", "0.0.0.0:7070")
	local, _ := net.ResolveTCPAddr("tcp", "0.0.0.0:10001")
	_, err := net.DialTCP("tcp", local, remote)
	if err != nil {
		fmt.Println(err)
	}
}

func main() {
	fmt.Println("take1")
	newConn()
	time.Sleep(5 * time.Second)
	fmt.Println("take2")
	newConn()
}
```

#### 実行してみる

2回目のServer接続時にすでに使用済みエラーとなり、接続に失敗してしまう

```
$ go run client/main.go
take1
take2
dial tcp 0.0.0.0:10001->0.0.0.0:7070: bind: address already in use
```

#### TCP状態遷移を確認してみる

① まずServerのみを起動した状態

```
$ netstat -antlp | grep -e 7070 -e 10001
tcp6       0      0 :::7070                 :::*                    LISTEN      20256/main
```

② 1回目のServer接続直後の状態
- Client側が `CLOSE_WAIT`
  - (ここが重要だった)Server側が接続をCloseしてもClient側は自動でCloseされないので、Client側のSocketのClose処理待ちしてる状態
- Server側は `FIN_WAIT2`
  - Client側へClose処理(FIN)を送りACKをもらった状態。Client側の`CLOSE_WAIT`が完了するのを待っている

```
$  netstat -antlp | grep -e 7070 -e 10001
tcp        1      0 127.0.0.1:10001         127.0.0.1:7070          CLOSE_WAIT  22909/main      
tcp6       0      0 :::7070                 :::*                    LISTEN      20256/main      
tcp6       0      0 127.0.0.1:7070          127.0.0.1:10001         FIN_WAIT2   -
```

③ 2回目のServer接続失敗し、Clientコマンド終了直後の状態
- Client側は、コマンドのプロセスが終了してるので `CLOSE_WAIT` がなくなっている
- Server側は、Client側の終了により`FIN_WAIT2` → `TIME_WAIT`(終了待ち)の状態へ切り替わっている

```
$  netstat -antlp | grep -e 7070 -e 10001
tcp6       0      0 :::7070                 :::*                    LISTEN      20256/main      
tcp6       0      0 127.0.0.1:7070          127.0.0.1:10001         TIME_WAIT   -
```

④ さらに60秒後
- Server側は、`TIME_WAIT`(終了待ち)の期限を越えたのでSocketはCloseされている
```
$ netstat -antlp | grep -e 7070 -e 10001
tcp6       0      0 :::7070                 :::*                    LISTEN      20256/main      
```

この調査から、Server側が接続をCloseしてもClient側が自動でCloseされないし自前でやる必要があることがわかった。
(これに気づくのに2,3時間かかった...)

## コードを修正して再実行

Client側のコードに、Close処理を追加

```go
package main

import (
	"fmt"
	"net"
	"time"
)

func newConn() {
	remote, _ := net.ResolveTCPAddr("tcp", "0.0.0.0:7070")
	local, _ := net.ResolveTCPAddr("tcp", "0.0.0.0:10001")
-	_, err := net.DialTCP("tcp", local, remote)
+	conn, err := net.DialTCP("tcp", local, remote)
	if err != nil {
		fmt.Println(err)
	}
+	conn.Close()
}

func main() {
	fmt.Println("take1")
	newConn()
-	time.Sleep(5 * time.Second)
+	time.Sleep(65 * time.Second)
	fmt.Println("take2")
	newConn()
}
```

#### 実行してみる

2回目のServer接続も成功した!

```
$ go run client/main.go
take1
take2
```

#### TCP状態遷移を確認してみる

① まずServerのみを起動した状態

```
$ netstat -antlp | grep -e 7070 -e 10001
tcp6       0      0 :::7070                 :::*                    LISTEN      32548/main      
```

② 1回目のServer接続直後の状態
- Client側にClose処理を追加したので、前回 `CLOSE_WAIT`だったが `TIME_WAIT`に変わってる
```
$ netstat -antlp | grep -e 7070 -e 10001
tcp        0      0 127.0.0.1:10001         127.0.0.1:7070          TIME_WAIT   -               
tcp6       0      0 :::7070                 :::*                    LISTEN      32548/main      
```

③ 2回目のServer接続へ成功し、Clientコマンド終了直後の状態
- Client側、Server側共に`TIME_WAIT`(終了待ち)の状態へ切り替わっている

```
$ netstat -antlp | grep -e 7070 -e 10001
tcp        0      0 127.0.0.1:10001         127.0.0.1:7070          TIME_WAIT   -               
tcp6       0      0 :::7070                 :::*                    LISTEN      32548/main      
tcp6       0      0 127.0.0.1:7070          127.0.0.1:10001         TIME_WAIT   -               
```

④ さらに60秒後
- `TIME_WAIT`(終了待ち)の期限を越えたのでSocketはCloseされている
```
$ netstat -antlp | grep -e 7070 -e 10001
tcp6       0      0 :::7070                 :::*                    LISTEN      20256/main      
```


## TCP状態遷移の備忘録

**TCP状態遷移図**

<img src="https://qiita-user-contents.imgix.net/https%3A%2F%2Fqiita-image-store.s3.amazonaws.com%2F0%2F2813%2F2169c473-a7d3-56fd-d748-282c14a84674.jpeg?ixlib=rb-1.2.2&auto=compress%2Cformat&gif-q=60&s=dbc5139eaa0657f4032e707cac03238e" width=30%>

図は[@mogulla3](https://qiita.com/mogulla3) さんの [TCPの状態遷移](https://qiita.com/mogulla3/items/196124b9fb36578e5c80) の記事から参照

クローズにはアクティブクローズとパッシブクローズがあり、名前の通りアクティブの方はクローズを開始した側、パッシッブの方はクローズを受けた側になる。今回だとCloseを先に開始してるServer側がアクティブで、Client側がパッシッブの状態遷移となる。

## 所感

TCP状態遷移の理解が低いまま実装を進めたのでハマってしまったが、こうゆう場合はググって解決するよりは遠回りと思わずしっかり理解して解決方法を出すとよかった。これまで断片的に理解していた点が線として繋がりを感じれた。


## 参考リンク

https://qiita.com/mogulla3/items/196124b9fb36578e5c80
https://qiita.com/kuni-nakaji/items/c07004c7d9e5bb683bc2
https://blog.ybbo.net/2013/05/29/tcp%E3%81%AEclose_wait%E3%81%A8%E3%81%AF%EF%BC%9F/
