title: Goでネットワークプログラミング
date: 2016-12-06
tags:
- Go
- AdventCalender

---

この記事は[AdventCalandar2016のGo(その2)](http://qiita.com/advent-calendar/2016/go2)の6日目の記事です。
Goとネットワークプログラミングの勉強でかねてSocket通信のいくつかのパターンを実装してみました。
あまり解説は無くほとんどコードですw

実装パターンは以下になります。
- シングルクライアントでの通信
- マルチクライアントでの通信
- マルチクライアントでselectとchanelを組み合せた通信

## シングルクライアントでの通信

```go
import (
	"fmt"
	"net"
	"os"
)

func newListener() *net.TCPListener {
	service := ":7777"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	checkError(err, "Resolve Error")
	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err, "Listen Error")
	fmt.Printf("Server Run Port: %s\n", service)
	return listener
}

func acceptLoop(listener *net.TCPListener) {
	fmt.Println("Ready For Accept")

	conn, err := listener.Accept()
	checkError(err, "Accept Error")
	defer conn.Close()
	fmt.Printf("[%s]Accept\n", conn.RemoteAddr())

	buf := make([]byte, 1024)
	for {
		n, err := conn.Read(buf)
		if n == 0 {
			break
		}
		checkError(err, "Read Error")
		fmt.Printf("[%s]Recv:%s\n", conn.RemoteAddr(), string(buf[:n]))
		conn.Write([]byte("OK\n"))
	}
}

func checkError(err error, msg string) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", msg, err.Error())
		os.Exit(1)
	}
}

func main() {
	listener := NewListener()
	defer listener.Close()

	acceptLoop(listener)
}
```
まず、シングルクライアントでシンプルなソケット通信を実装してみました。
newListenerでサーバーソケットを生成して、acceptLoopでクライアントからの
接続待ち(Accept)、接続後はデータ受信(Read)、データ送信(Write)のループが行われます。
このAccept、Read、Writeはすべてブロッキング処理なので完了するまで次の処理が行われません。
この実装では1つのクライアントが処理を行っている間は他のクライアントからは処理は行えません。


## マルチクライアントでの通信

```go
import (
	"fmt"
	"net"
	"os"
)

func newListener() *net.TCPListener {
	service := ":7777"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	checkError(err, "Resolve Error")
	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err, "Listen Error")
	fmt.Printf("Server Run Port: %s\n", service)
	return listener
}

func acceptLoop(listener *net.TCPListener) {
	fmt.Println("Ready For Accept")

	for {
		conn, err := listener.Accept()
    	checkError(err, "Accept Error")
		go handleClient(conn)
	}
}

func handleClient(conn net.Conn) {
	buf := make([]byte, 1024)
	for {
		n, err := conn.Read(buf)
		if n == 0 {
			fmt.Printf("[%s]Recv:EOF\n", conn.RemoteAddr())
			break
		}
		CheckError(err, "Read Error")
		fmt.Printf("[%s]Recv:%s\n", conn.RemoteAddr(), string(buf[:n]))
		conn.Write([]byte("OK\n"))
	}
}

func checkError(err error, msg string) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", msg, err.Error())
		os.Exit(1)
	}
}

func main() {
	listener := newListener()
	defer listener.Close()

	acceptLoop(listener)
}
```

Goroutineを利用してマルチクライアントを実現しています。
ほかの言語でマルチクライアントを実現する際は、I/O多重化(select, epoll, etc)で受け付けて
時間がかかる処理をマルチスレッド or マルチプロセスを利用した非同期I/Oにした組み合せで
実装する例を良く目にしますが結構複雑な処理になってしまいます。
Goroutineを利用した場合、シンプルでわかりやすく且つ並列性を確保した処理パフォーマンスが高いプログラムを書くことができます。
ただプログラム異常時にきちんとエラーハンドリングしなれば全体が影響を受けることもあります。


## マルチクライアントでselectとchanelを組み合せた通信

```go
import (
	"bufio"
	"fmt"
	"net"
	"os"
)

func newClient(connection net.Conn) *Client {
	writer := bufio.NewWriter(connection)
	reader := bufio.NewReader(connection)

	client := &Client{
		conn:     connection,
		incoming: make(chan string),
		outgoing: make(chan string),
		reader:   reader,
		writer:   writer,
	}

	go client.read()
	go client.write()

	return client
}

type Client struct {
	conn     net.Conn
	incoming chan string
	outgoing chan string
	reader   *bufio.Reader
	writer   *bufio.Writer
}

func (client *Client) read() {
	for {
		line, err := client.reader.ReadString('\n')
		checkError(err, "ReadString Error")
		client.incoming <- line
		fmt.Printf("[%s]Read:%s", client.conn.RemoteAddr(), line)
	}
}

func (client *Client) write() {
	for data := range client.outgoing {
		client.writer.WriteString(data)
		client.writer.Flush()
		fmt.Printf("[%s]Write:%s\n", client.conn.RemoteAddr(), data)
	}
}

func newListener() *net.TCPListener {
	service := ":7777"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	checkError(err, "Resolve Error")
	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err, "Listen Error")
	fmt.Printf("Server Run Port: %s\n", service)
	return listener
}

func newTCPServer() *Server {
	listener := newListener()
	server := &Server{
		listener: listener,
		clients:  make([]*Client, 0),
		conn:     make(chan net.Conn),
		incoming: make(chan string),
		outgoing: make(chan string),
	}
	return server
}

type Server struct {
	listener *net.TCPListener
	clients  []*Client
	conn     chan net.Conn
	incoming chan string
	outgoing chan string
}

func (server *Server) acceptLoop() {
	defer server.listener.Close()

	fmt.Println("Ready For Accept")
	for {
		conn, err := server.listener.Accept()
		checkError(err, "Accept Error")
		server.conn <- conn
	}
}

func (server *Server) listen() {
	fmt.Println("Ready For Listen")

	go func() {
		for {
			select {
			case conn := <-server.conn:
				server.addClient(conn)
			case data := <-server.incoming:
				server.response(data)
			}
		}
	}()
}

func (server *Server) addClient(conn net.Conn) {
	fmt.Printf("[%s]Accept\n", conn.RemoteAddr())
	client := newClient(conn)
	server.clients = append(server.clients, client)
	go func() {
		for {
			server.incoming <- <-client.incoming
			client.outgoing <- <-server.outgoing
		}
	}()
}

func (server *Server) response(data string) {
	server.outgoing <- data
}

func checkError(err error, msg string) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", msg, err.Error())
		os.Exit(1)
	}
}

func main() {
	server := newTCPServer()
	server.listen()
	server.acceptLoop()
}
```

Go勉強するなら、GoroutineとChanelをもっと使いたいよねってことで、もう少し並列性を意識したTCPServerを実装してみました。
他の言語では、selectはファイルディスクリプタがレディになったかを監視する用途で利用しますが
Goの場合はChanelがレディなったかを監視する用途で利用します。
また通常、非同期I/Oでリクエスト処理しようとするQueueやリングバッファに詰め込んで
ロックをかけながら取り出す必要がありますのでコードが複雑になりがちです。
GoのChanelを利用すればロック処理を書かずに並列性を保ちながらシンプルなコードでデータ受け渡しが可能です


## 参考リンク
https://astaxie.gitbooks.io/build-web-application-with-golang/content/ja/08.1.html
https://gist.github.com/drewolson/3950226
http://qiita.com/awakia/items/f8afa070c96d1c9a04c9
http://blog.matsumoto-r.jp/?p=2030
http://ascii.jp/elem/000/001/276/1276572/
