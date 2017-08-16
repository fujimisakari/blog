title: Erlangでネットワークプログラミング - TCP
date: 2017-05-20 00:00
tags: Erlang

---

ErlangでTCPを使ったコードのメモ。

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51bZh24YhqL._SL160_.jpg" alt="プログラミングErlang" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">プログラミングErlang</a><div class="amazlet-detail" style="margin-top:20px;">Joe Armstrong <br />オーム社 <br />売り上げランキング: 188,900<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>
の14章あたり。

## TCPの特徴
- 相手と相互に連絡を取り合って接続するので、信頼性のあるバイトストリームを提供できる
- 送信中のパケットが途中で損失した場合でも再送するので相手に配送されるメッセージが保証される
- 送信時に分割したメッセージが順序どおり配送される

## TCP通信でのデータ構成について

TCPソケットのデータはただのバイトのストリームです。
そのため、データは伝送時に適当なサイズの断片に分割されるため、
1つの要求や応答のデータの大きさをわかるようにしなればならないので
それを表現できる何らかの取り決めをしなければなりません。

Erlangの場合、そのデータの長さを示すN(1,2,4のいずれか)バイトを
前に付ける必要があり、この長さはクライアント側とサーバ側で一致していなればならない。
また、このようなデータのエンコード/デコードの規則は意識しなくても
ErlangのBIFである`term_to_binary`と`binary_to_term`を使えば簡単に実現できる

## TCPサーバを実装してみる
- 逐次サーバ
- 並列サーバ

### 逐次サーバ

逐次処理のみを許可してるSocket通信の実装。
Server側の待ち受け用のソケットはオープン状態で接続を待ってるけど
クライアントが接続中の場合は処理を終えるまで、
別クライアントからの接続要求は待ち行列に置かれる。
待ち状態の接続の数が待ち受け可能な数を越えると接続は拒否される。

```erlang
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            gen_tcp:send(Socket, Str),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            gen_tcp:close(Socket)
    end.
```

### 並列サーバ

逐次サーバとコードが似てるけど、クライアントの接続要求毎にspawnでプロセスを
生成するので複数のクライアントと並列で接続が可能となる。
並列サーバは、数千個もの接続を作る能力があり、同時接続数に制限を設ける場合は
現在アクティブな接続の数を示すカウンターを用意すればいい。

```erlang
start_parallel_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            gen_tcp:send(Socket, Str),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.


nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            gen_tcp:close(Socket)
    end.
```

## TCPサーバの制御に関する実装

Erlangソケットは以下のいずれかの手法でTCPソケットをオープンできる
- アクティブメッセージ受信(ノンブロッキング)
- パッシブメッセージ受信(ブロッキング)
- ハイブリット手法(限定ブロッキング)

### アクティブメッセージ受信(ノンブロッキング)

`{active, true}`オプションでListenすることでアクティブモードとなる。
このモードでは、メッセージの受信をサーバプロセスが制御できないので
行儀の悪いクライアントが数千個ものメッセージをシステムに送りつけてきてもその流れはを止めることができない。
サーバがクライアントの要求についていけると確信できる場合だけの利用にした方がいい。

以下の実装の場合では、Client側でSendしてるRpc1、Rpc2, Rpc3のCommandが同時時にサーバ側で処理されるイメージ。

```erlang
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            gen_tcp:send(Socket, Str),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

nano_client_eval() ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary("Rpc1 Commnad")),
    ok = gen_tcp:send(Socket, term_to_binary("Rpc2 Commnad")),
    ok = gen_tcp:send(Socket, term_to_binary("Rpc3 Commnad")),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            gen_tcp:close(Socket)
    end.
```

### パッシブメッセージ受信(ブロッキング)
`{active, false}`オプションでListenすることでパッシブモードとなる。
このモードでは、アクティブモードとは逆でメッセージの受信をサーバプロセスが制御できる。
サーバはgen_tcp:recvを呼び出すタイミングでメッセージを読み込むので
リソース的に処理しきれない大量のメッセージの流れがあったとしてもクラッシュすることはない。

以下の実装の場合では、Client側でSendしてるRpc1、Rpc2, Rpc3のCommandが
サーバ側で1つづつ読み込まれて処理されていくイメージ。

```erlang
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, false}]),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            gen_tcp:send(Socket, Str),
            loop(Socket);
        {error, closed} ->
            io:format("Server socket closed~n")
    end.

nano_client_eval() ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary("Rpc1 Commnad")),
    ok = gen_tcp:send(Socket, term_to_binary("Rpc2 Commnad")),
    ok = gen_tcp:send(Socket, term_to_binary("Rpc3 Commnad")),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            gen_tcp:close(Socket)
    end.
```

### ハイブリット手法(限定ブロッキング)
パッシブモードを使うのが正しいやり方だと思うかもしれないけど、
1つのソケットからのメッセージしか待つことができないので
複数のソケットからのメッセージを待たなければならないサーバの場合は役に立たない。

こういった場合は、ブロッキングとノンブロッキングのいいとこ取りしたハイブリッド手法を使う。
この手法ではソケットは`{active, once}`オプションを指定してListenする。

このハイブリット手法は、次のデータを受信するタイミングを任意で行えるようになる。
次のメッセージを受信する準備できた時に`inet:setopts`を呼び出せばいいだけ。
この呼び出しが行われるまで次のメッセージはブロックされるので
複数のソケットでメッセージ待ちながら、大量のメッセージの流れも制御できるようなる。

```erlang
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, once}]),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            gen_tcp:send(Socket, Str),
            %% 準備ができたら次のメッセージの受信を有効にする
            inet:setopts(Socket, [{active, once}]),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.


nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            gen_tcp:close(Socket)
    end.
```
