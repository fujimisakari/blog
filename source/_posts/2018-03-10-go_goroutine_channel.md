title: ゴルーチン、チャネルを利用した並行パターン
date: 2018-03-10 00:00
tags: Go

---

最近以下の書籍でゴルーチン、チャネルを利用した並行・並列ロジックに勉強しましたので整理してみます。

<br />

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4621300253/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/41BaAiMmrnL._SL160_.jpg" alt="プログラミング言語Go (ADDISON-WESLEY PROFESSIONAL COMPUTING SERIES)" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4621300253/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">プログラミング言語Go (ADDISON-WESLEY PROFESSIONAL COMPUTING SERIES)</a><div class="amazlet-detail" style="margin-top: 20px">Alan A.A. Donovan Brian W. Kernighan <br />丸善出版 <br />売り上げランキング: 162,350<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4621300253/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4908686033/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/515xkIcDgXL._SL160_.jpg" alt="Goならわかるシステムプログラミング" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4908686033/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Goならわかるシステムプログラミング</a><div class="amazlet-detail" style="margin-top: 20px">渋川よしき <br />Lambda Note <br />売り上げランキング: 62,948<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4908686033/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

<br />

もともと並行プログラミングはGoなども含め勉強したことがありましたが、ゴルーチンやチャネルなどの理解度が低いまま扱っており、今読んでるコード、書いているコードがどうゆう並行ロジックパターンに元づいて実装してるのかロジック像が掴みきれず時間を取ってしまう状況でした。そのため、この並行ロジック辺りのパターンをある程度まとめてみることにしました。

実装パターンは次のように整理してみました。

- 基本的な並行パターン
- バッファなしチャネルパターン
- バッファありチャネルパターン
- 並列forループパターン
- タスク生成と処理を分けるパターン(Producer-Consumerパターン)
- パイプラインパターン(fan-outパターン)
- selectを利用したチャネル多重化パターン
- selectとワーカープールを組み合せた多重化パターン
- Feture/Promiseパターン

## 基本的な並行パターン

Goでまず一番最初に学ぶ並行パターンです。
`main()`も(メイン)ゴルーチンなので、無名関数でそれとは別のゴルーチンを生成して並行処理を実現してます。

```go
package main
 
import (
    "fmt"
    "time"
)
 
func main() {
    fmt.Println("-- 1 --")
    go func() {
        fmt.Println("-- 2 --")
        time.Sleep(time.Second)
        fmt.Println(("-- 3 --"))
    }()
    time.Sleep(2 * time.Second)
}
```


## バッファなしチャネルパターン

このパターンは重い処理(ネットワークIOやファイルIOなど)を非同期で実行したい場合など利用されます。
重い処理自体は無名関数ゴルーチンが非同期で行いますが終了時はチャネルを通じて別ゴルーチンで受信させますので処理フローは同期化が保証されてます。この特定条件の際に読み込み・書き込みがブロックされる特性は、並行処理制御の手法としてさまざまな形で使えます。サンプルは最も基本的な「別のgoroutineの終了を待つ」パターンですが、`sync.Mutex`のLockなどで制御するしくみもあります。

```go
package main

import (
	"log"
	"time"
)

func main() {
	done := make(chan struct{})
	log.Println("start")
	go func() {
		time.Sleep(1 * time.Second)
		log.Println("done")
		done <- struct{}{}
	}()
	log.Println("between")
	<-done
}
```


## バッファありチャネルパターン

このパターンは、同時に実行できるタスク数(非同期I/O)を制限したい場合の利用するパターンです。
要素に数を指定した生成したチャネルをキューとしてい扱い、タスク(ゴルーチン)実行の開始時にチャネルへ値を送信(エンキュー)して終了時に受信(デキュー)するサイクルで、チャネル送信時に上限数まで逹していた場合は実行が待たされます(チャネルの送受信操作はFIFO)。また、チャネルをキューとして扱いましたがセマフォ相当にも例えられたりします(チャネル要素数がセマフォ計数)。


```go
package main

import (
	"fmt"
	"net/http"
	"sync"
)

func fetch(sem chan struct{}, url string) {
	sem <- struct{}{}
	defer func() { <-sem }()

	http.Get(url)
	fmt.Println("fetched", url)
}

func main() {
	sem := make(chan struct{}, 10)

	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			url := "http://blog.fujimisakari.com"
			fetch(sem, url)
		}()
	}
	wg.Wait()
}
```


## 並列forループパターン

ループ毎の処理すべてを並列に実行したい場合に利用するパターンです。
waitを入れずに実行すると、`main()`(メインゴールチン)が終了してしまうので`sync.WaitGroup`を使って各ゴルーチン終了まで待ちます。
(ループの内部処理が小さすぎると、オーバヘッドのほうが大きくなり効率があがらないことがある)

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var messages = []string{
		"test1",
		"test2",
		"test3",
		"test4",
		"test5",
	}

	var wg sync.WaitGroup
	for _, msg := range messages {
		wg.Add(1)
		// レキシカルスコープ参照だと最後のループ変数をゴルーチンが評価するので引数で渡すようにすること
		go func(m string) {
			defer wg.Done()
			printer(m)
		}(msg)
	}
	wg.Wait()
}

func printer(msg string) {
	time.Sleep(1 * time.Second)
	fmt.Println(msg)
}
```


## タスク生成と処理を分けるパターン(Producer-Consumerパターン)

このパターンは、`バッファありチャネルパターン`と同様に同時に複数のタスク(非同期I/O)を実行したい場合の利用するパターンです。
`バッファありチャネルパターン`との違いはタスク数の制限方法がバッファチャネルでなく、ワーカー数になってることです。タスク生成(Producer)時にチャネルを通じてワーカープール(Consumer)へ処理を送信していきます。ワーカー数を増やことで安全に処理速度をスケールすることができます。


```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func worker(sem <-chan int, wg *sync.WaitGroup) {
	for num := range sem {
		time.Sleep(1 * time.Second)
		fmt.Println("process", num)
		wg.Done()
	}
}

func main() {
	sem := make(chan int)
	var wg sync.WaitGroup

	// Consumer
	for i := 0; i < 20; i++ {
		go worker(sem, &wg)
	}

	// Produder
	for i := 1; i <= 200; i++ {
		wg.Add(1)
		go func(x int) {
			sem <- x
		}(i)
	}
	wg.Wait()
	close(sem)
}
```


## パイプラインパターン(fan-outパターン)

このパターンはタスク分割したゴルーチンを順序付けて処理したい場合に利用するパターンです。
1つのゴルーチン内のチャネル送信(出力)が別のゴルーチン内のチャネル受信(入力)になるように、複数のゴルーチンを接続して実装します。
終ったチャネルは閉じていないと送信するとパニックになりますし、受信すると待たされることなくゼロ値を生成するので
ループが終わりのないゼロ値を受け取りを繰り返すことになり`fatal error: all goroutines are asleep - deadlock!`となってしまいます。

```go
package main

import (
	"fmt"
)

func double(naturals <-chan int, doubles chan<- int) {
	for {
		x, ok := <-naturals
		if !ok {
			close(doubles)
			break
		}
		doubles <- x * 2
	}
}

func squarer(doubles <-chan int, squares chan<- int) {
	for {
		x, ok := <-doubles
		if !ok {
			close(squares)
			break
		}
		squares <- x * x
	}
}

func main() {
	naturals := make(chan int)
	doubles := make(chan int)
	squares := make(chan int)

	go func() {
		for i := 1; i < 10; i++ {
			naturals <- i
		}
		close(naturals)
	}()
	go double(naturals, doubles)
	go squarer(doubles, squares)

	for result := range squares {
		fmt.Println(result)
	}
}
```


## selectを利用したチャネル多重化パターン

複数のチャネルを同時に扱いたい場合に利用します。
selectは複数のチャネルのレディ(受信/送信可能)の状態をノンブロッキングで同時に監視することができ、レディになったもの返してくれます。複数のcaseがレディ状態の場合はランダムで1つ選ばれます。それにより、すべてのチャネルが平等に選択されることが保証されます。レディになってない場合はselectスコープをブロッキングし続けてくれるのでforループなどでも無駄なCPUリソースを消費しません。(defalutがあればブロッキングはしません)

```go
package main

import (
	"fmt"
	"sync"
)

type Server struct {
	req chan string
	res chan string
}

func serverStart(server Server) {
	worker := make(chan string, 30)
	result := make(chan string)
	go listen(server, worker, result)
	go response(worker, result)
}

func listen(server Server, worker chan<- string, result <-chan string) {
	for {
		select {
		case req := <-server.req:
			worker <- req
		case res := <-result:
			server.res <- res
		}
	}
}

func response(worker <-chan string, result chan<- string) {
	for request := range worker {
		go func(req string) {
			result <- fmt.Sprintf("response from %s", req)
		}(request)
	}
}

func request(server Server, reqNum int, wg *sync.WaitGroup) {
	defer wg.Done()
	server.req <- fmt.Sprintf("reqest %d", reqNum)
	fmt.Println(<-server.res)
}

func main() {
	server := Server{req: make(chan string), res: make(chan string)}
	serverStart(server)

	var wg sync.WaitGroup
	for i := 1; i < 1000; i++ {
		wg.Add(1)
		go request(server, i, &wg)
	}
	wg.Wait()
}
```


## selectとワーカープールを組み合せた多重化

このパターンは、selectのチャネル多重化と目的は変わらないです。
selectでのチャネル受信後は、そのまま同期処理、ゴルーチンで並行処理、ワーカープールで処理するなど考えれますが後者の実装となります。

```go
package main

import (
	"fmt"
	"runtime"
	"sync"
)

type Server struct {
	req chan string
	res chan string
}

func serverStart(server Server) {
	worker := make(chan string, 30)
	result := make(chan string)
	go listen(server, worker, result)
	for i := 0; i < runtime.NumCPU(); i++ {
		go response(i, worker, result)
	}
}

func listen(server Server, worker chan<- string, result <-chan string) {
	for {
		select {
		case req := <-server.req:
			worker <- req
		case res := <-result:
			server.res <- res
		}
	}
}

func response(id int, worker <-chan string, result chan<- string) {
	for reqest := range worker {
		go func(req string) {
			result <- fmt.Sprintf("id=%d response from %s", id, req)
		}(reqest)
	}
}

func request(server Server, reqNum int, wg *sync.WaitGroup) {
	defer wg.Done()
	server.req <- fmt.Sprintf("reqest %d", reqNum)
	fmt.Println(<-server.res)
}

func main() {
	server := Server{req: make(chan string), res: make(chan string)}
	serverStart(server)

	var wg sync.WaitGroup
	for i := 1; i < 1000; i++ {
		wg.Add(1)
		go request(server, i, &wg)
	}
	wg.Wait()
}
```


## Feture/Promiseパターン

このパターンは、目的は`パイプラインパターン`と同様でチャネルの送受信を通じて処理結果の取得を必要になるところまで後回しにする手法です。
実装では、Featureが「今はまだ得られないけど将来得られるはずの入力」、Promiseが「将来、値を提供するという約束」で表現されおり、並列デザインパターンとしてもあるので馴染がある人にとってはわかりやすい表現なのかも。

ちなみにゲーム開発などでも画面表示時のゲームコンポーネントLoadingを効率よくするために Feture/Promiseパターンでシーケンス制御しながら各タスクでの必要オブジェクト郡を並列Loadすることで遅延を短縮させ最適化をやってたりします。(バトル画面とかで、フィールドオブジェクト生成 → バトル初期値設定 → プレイヤー、敵オブジェクトを生成 → バトルシーントランザクション → バトル開始 みたいな流れ)

```go
package main

import (
	"fmt"
	"strings"
	"sync"
	"time"
)

func loadGameStage() chan string {
	promise := make(chan string)
	go func() {
		fmt.Println("load stage")
		time.Sleep(2 * time.Second)
		promise <- "done stage"
	}()
	return promise
}

func loadGameResource(featureStage <-chan string) chan []string {
	promise := make(chan []string)
	go func() {
		resource := []string{<-featureStage}
		var mu sync.Mutex
		var wg sync.WaitGroup
		wg.Add(3)

		go func() {
			defer wg.Done()
			fmt.Println("load character")
			time.Sleep(1 * time.Second)
			mu.Lock()
			resource = append(resource, "done character")
			mu.Unlock()
		}()

		go func() {
			defer wg.Done()
			fmt.Println("load field material")
			time.Sleep(1 * time.Second)
			mu.Lock()
			resource = append(resource, "done field material")
			mu.Unlock()
		}()

		go func() {
			defer wg.Done()
			fmt.Println("load property")
			time.Sleep(1 * time.Second)
			mu.Lock()
			resource = append(resource, "done property")
			mu.Unlock()
		}()

		wg.Wait()
		promise <- resource
	}()
	return promise
}

func loadGameMenu(featureResource <-chan []string) chan []string {
	promise := make(chan []string)
	go func() {
		resource := <-featureResource
		fmt.Println("load menu frame")
		time.Sleep(1 * time.Second)
		meue := append(resource, "done menu frame")
		promise <- meue
	}()
	return promise
}

func main() {
	featureStage := loadGameStage()
	featureResource := loadGameResource(featureStage)
	featureMenu := loadGameMenu(featureResource)
	fmt.Println(strings.Join(<-featureMenu, "\n"))
}
```


## 所感

並行プログラミングは少しは経験があったのである程度わかるだろうと踏むでたんですが全然まだまだでした。
本を見て理解したつもりになってたのですが、実際コード書いてみるとdeadlock!、書くたびにdeadlock! なんとなくコツを掴んできたぞと思ったらdeadlock!のループでした。Emacs以来のツンデレ感を味わったのですが今回整理したことで少しは仲よくなれた気がします。Goの並列プログラミングを習得するにはまだエラーハンドリングやロック機構、テスト方法など覚えることが盛りだくさんですが学んだパターンをベースにこれもっと幅を広げていけたらと思ってる次第です。


## 参考リンク

- http://ascii.jp/elem/000/001/475/1475360/
- https://mattn.kaoriya.net/software/lang/go/20160706165757.htm
- https://qiita.com/hayajo/items/4cd75f87e35e60ae11a9
