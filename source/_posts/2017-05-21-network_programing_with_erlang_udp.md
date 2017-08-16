title: Erlangでネットワークプログラミング - UDP
date: 2017-05-21 00:00
tags: Erlang

---

TCPに続いてErlangでUDPを使ったコードのメモ。

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51bZh24YhqL._SL160_.jpg" alt="プログラミングErlang" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">プログラミングErlang</a><div class="amazlet-detail" style="margin-top:20px;">Joe Armstrong <br />オーム社 <br />売り上げランキング: 188,900<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/4274067149/fujimisakar03-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>
の14章あたり。

## UDPの特徴

- クライアントがサーバにデータを送信するのにサーバへの接続を確立する必要はない
- 送信したデータには信頼性がない
  - 届かなかったりする
  - 到着順が狂ったりする
  - 二重で届く可能性がある
- 受け取ったデータは壊れていないことが保証される

### メリット

- 接続時にやり取りが不要なので通信が軽く、
  多数のクライアントがサーバに小さなメッセージを送るようなアプリケーションに適している
- パケットの到着順の制御や再送付制御がないため、
  音声や動画のようにデータの確実性よりリアルタイム性を重視する場合に向く
- ディスクリプタをコネクションごとに必要としないため、
  マルチクライアント向けサーバプログラムでディスクリプタが枯渇する心配がない
- ブロードキャスト・マルチキャストといった、一対多の通信手段が用意されている

### デメリット

- 接続時のやり取りがないため、それを監視して動的にポートを開けたりするファイヤフォールが使えない
- データの到着順の制御や再送制御が必要な場合は自分で実装する必要がある
- 送信元の判別が難しい
- 切断検知が難しい

## UDPサーバを実装してみる

### 階乗サーバ

送られてきた数を階乗計算して返す簡易サーバ。
接続確立する必要もなく、データの到着順の制御や再送制御など入れてないので
ソケットをオープンしてメッセージを送り、応答を待ちクローズするシンプルに実装になってる。

```erlang
start_server() ->
    spawn(fun() -> server(4000) end).

server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("server opend socket: ~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:format("server received: ~p~n", [Msg]),
            N = binary_to_term(Bin),
            Fac = fac(N),
            gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
            loop(Socket)
    end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost", 4000, term_to_binary(N)),
    Value = receive
                {udp, Socket, _, _, Bin} = Msg ->
                io:format("client received: ~p~n", [Msg]),
                binary_to_term(Bin)
            after 2000 ->
                0
            end,
    gen_udp:close(Socket),
    Value.

```

### 階乗サーバ(重複時の対応)

UDPパケットが重複して配送された場合の対応を上記の階乗サーバへ追加で実装する。
重複して配送された場合、2番目の問い合わせに対する応答が
実は最初の問い合わせに対する応答だったということもありえる。
このような場合、クライアント側が要求時に一意なリファレンスを含め、
サーバがそのリファレンスを返してきたかどうかのチェックで要求、応答の保証ができる。
Erlangの場合、BIFの`make_ref`を呼び一意のリファレンスを生成できる。

```erlang
start_server() ->
    spawn(fun() -> server(4000) end).

server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("server opend socket: ~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:format("server received: ~p~n", [Msg]),
            {Ref, N} = binary_to_term(Bin),
            Fac = fac(N),
            gen_udp:send(Socket, Host, Port, term_to_binary({Ref, Fac})),
            loop(Socket)
    end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Ref = make_ref(),
    Bl = term_to_binary({Ref, N}),
    ok = gen_udp:send(Socket, "localhost", 4000, Bl),
    wait_for_ref(Socket, Ref).

wait_for_ref(Socket, Ref) ->
    receive
        {udp, Socket, _, _, Bin} = Msg ->
            io:format("client received: ~p~n", [Msg]),
            case binary_to_term(Bin) of
                {Ref, Val} ->
                    Val;
                {_SomeOtherRef, _} ->
                    wait_for_ref(Socket, Ref)
            end
    after 2000 ->
        0
    end.
```
