title: ErlangからPythonに外部接続
date: 2017-05-14 00:00
tags:
- Erlang
- Python

---

Erlangの勉強がてら、ErlangでPythonスクリプトに接続するRPCプログラムを書いてみました。

## 概要
ErlangクライアントプロセスからPythonプロセスの標準入力にバイナリデータを送り
Python内部のプログラム実行結果を標準出力でErlangプロセスへ返す流れになります。

構成としては以下のようになってます。

```
+- ERTS (Erlang RunTime System) ---+       ******　OS(Python) のプロセス　******
|                                  |       *                                *
|  +-----------+         |>>>>>>>>>>-->>-->(標準入力)　　                     *
|  | プロセス　　|<------->| ポート　　|       *                                *
|  +-----------+         |<<<<<<<<<<--<<--<(標準出力/標準エラー出力)　　　       *
|                                  |       *                                *
+----------------------------------+       **********************************
```

Erlangで外部プログラムに接続する場合、ErlangクライアントプロセスとPythonプロセスの
仲介役を担うErlangポートを利用します。(Erlangポートは接続プロセスとも呼ばれます)
双方の通信にはErlangポート介して、BERT-RPCプロトコルの仕様に沿ったバイト型のデータで通信を行います。


## 使用するプロトコル(BERT-RPC)
ErlangからPythonプロセスに外部接続してプログラムを実行するためには、
両側でRPCプロトコルに厳密に従うインタフェースを実装しなければなりません。
今回は、BERT-RPCのプロトコルに従って実装してます。
BERT-RPCのBERTはBinary ERlang Termの略で、Erlangの`term_to_binary/1`を
利用したバイナリデータ交換の仕組みを使って通信を行っています。
そのため、Python側ではBERT-RPCの仕様に合せてたバイナリデータのエンコード、デコードを実装しています。


## Erlang側

この実装では、PythonプロセスとErlangプロセスの仲介役をするErlangポートの生成と
ErlangプロセスからPythonプログラムの関数を実行するインタフェースを実装しています

実装内容としては
- `start()`にてPythonスクリプトを実行するErlangポートのプロセスを生成する
  - `[{packet, 2}, binary]`で2バイトのパケット長ヘッダの付加とバイト型の通信にする
- Pythonプログラムの`twice()`, `sum()`, `hello()`を遠隔で実行するインタフェースを用意
- Pythonプログラムとの通信の入力、出力のバイナリデータをエンコード、デコードする

```erlang
%% porttest.erl

-module(porttest).
-compile(export_all).

start() ->
    spawn(fun() ->
          register(porttest, self()),
          process_flag(trap_exit, true),
          Port = open_port({spawn, "./erlang_port/my_protocol.py"}, [{packet, 2}, binary]),
          loop(Port)
      end).

stop() ->
    porttest ! stop.

twice(X) -> call_port({twice, X}).
sum(X, Y) -> call_port({sum, X, Y}).
hello(Name) -> call_port({hello, Name}).

call_port(Msg) ->
    porttest ! {call, self(), Msg},
    receive
    {porttest, Result} ->
        io:format("received: '~p' ~p ~n", [Result, self()])
    end.

loop(Port) ->
    receive
    {call, Caller, Msg} ->
        Port ! {self(), {command, encode(Msg)}},
        receive
        {Port, {data, Data}} ->
            Caller ! {porttest, decode(Data)};
        X ->
            io:format("unknown message: [~w]~n", [X]),
            throw('Unknown message received.')
        end,
        loop(Port)
    end.

encode({twice, X}) -> term_to_binary({twice, X});
encode({sum, X, Y}) -> term_to_binary({sum, X, Y});
encode({hello, Name}) -> term_to_binary({hello, Name}).

decode(Data) -> binary_to_term(Data).
```

## Python側

この実装では、Erlang側から送られてきたデータより
関数と引数を指定して実行し、結果をErlangへ送り返す実装をしてます

各クラスについて
- ErlangTermsMixin
  - Erlang側との通信で入出力データをプロトコルの規約に合せてエンコード、デコードする
- Port
  - バイナリデータの標準入力と標準出力のインタフェースを実装
- Protocol
  - Erlangから送られてきたデータを実行、結果を送り返す処理のMainLoopの実装


```python
#! /usr/bin/env python
# -*- coding:utf-8 -*-

import os
import sys
import errno

from struct import pack, unpack


class ErlangTermsMixin(object):

    def decode(self, term):
        version = ord(term[0])
        if version != 131:
            raise ValueError('unknown protocol version: {}'.format(version))
        return self._decode_term(term[1:])

    def _decode_term(self, term):
        tag = ord(term[0])
        tail = term[1:]

        if tag == 97:
            # SMALL_INTEGER_EXT
            return ord(tail[:1]), tail[1:]
        elif tag == 107:
            # STRING_EXT
            length, = unpack('>H', tail[:2])
            tail = tail[2:]
            return [ord(i) for i in tail[:length]], tail[length:]
        elif tag == 100:
            # ATOM_EXT
            length, = unpack('>H', tail[:2])
            tail = tail[2:]
            name = tail[:length]
            tail = tail[length:]
            return name, tail
        elif tag == 104:
            # SMALL_TUPLE_EXT, LARGE_TUPLE_EXT
            arity = ord(tail[0])
            tail = tail[1:]

            lst = []
            while arity > 0:
                term, tail = self._decode_term(tail)
                lst.append(term)
                arity -= 1
            return tuple(lst), tail
        raise ValueError('unsupported data tag: {}'.format(tag))

    def encode(self, term):
        encoded_term = self._encode_term(term)
        return '\x83' + encoded_term

    def _encode_term(self, term):
        if isinstance(term, tuple):
            arity = len(term)
            if arity <= 255:
                header = 'h{:c}'.format(arity)
            elif arity <= 4294967295:
                header = pack('>BI', 105, arity)
            else:
                raise ValueError('invalid tuple arity')
            return header + ''.join(self._encode_term(t) for t in term)
        elif isinstance(term, unicode):
            try:
                bytes_data = term.encode('latin1')
            except UnicodeEncodeError:
                pass
            return pack('>BH', 107, len(term)) + bytes_data
        elif isinstance(term, str):
            length = len(term)
            if length > 4294967295:
                raise ValueError('invalid binary length')
            return pack('>BI', 109, length) + term
        # must be before int type
        elif isinstance(term, (int, long)):
            if 0 <= term <= 255:
                return 'a{:c}'.format(term)
            elif -2147483648 <= term <= 2147483647:
                return pack('>Bi', 98, term)
            raise ValueError('invalid integer value')
        raise ValueError('unsupported data type: {}'.format(term))


class Port(ErlangTermsMixin):

    PACK_FORMAT = '>H'
    PACKET_BYTE = 2

    def __init__(self):
        self.in_d = sys.stdin.fileno()
        self.out_d = sys.stdout.fileno()

    def read(self):
        data = self._read_data(self.PACKET_BYTE)
        length, = unpack(self.PACK_FORMAT, data)
        data = self._read_data(length)
        return self.decode(data)[0]

    def _read_data(self, length):
        data = ''
        while len(data) != length:
            try:
                buf = os.read(self.in_d, length)
            except IOError as e:
                if e.errno == errno.EPIPE:
                    raise EOFError('read error, EPIPE')
                raise IOError('read error, io error')
            if not buf:
                raise EOFError('read error, buffer')
            data += buf
        return data

    def write(self, message):
        data = self.encode(message)
        data = pack(self.PACK_FORMAT, len(data)) + data
        length = len(data)
        if not length:
            return

        try:
            n = os.write(self.out_d, data)
        except IOError as e:
            if e.errno == errno.EPIPE:
                raise EOFError('write error, EPIPE')
            raise IOError('write error, io error')
        if n == 0:
            raise EOFError('write error, no data')

    def close(self):
        os.close(self.in_d)
        os.close(self.out_d)


class Protocol(object):

    def __init__(self):
        self.port = Port()

    def handle(self, message):
        name = message[0]
        args = message[1:]

        handler = getattr(self, 'handler_{}'.format(name), None)
        if handler is None:
            return 'Error', 'Dose not exsit handler'

        try:
            response = handler(*args)
        except TypeError:
            response = 'TypeError', 'function_clause'
        return response

    def run(self):
        while True:
            try:
                message = self.port.read()
                response = self.handle(message)
                self.port.write(response)
            except ValueError as e:
                response = 'ValueError', e.message
                self.port.write(response)
            except EOFError as e:
                response = 'EOFError', e.message
                self.port.write(response)
                break


class MyProtocol(Protocol):

    def handler_twice(self, x):
        return 2 * int(x)

    def handler_sum(self, x, y):
        return int(x) + int(y)

    def handler_hello(self, name):
        return 'Hello, {}'.format(name)

if __name__ == '__main__':
    MyProtocol().run()

```

## 感想

実装してみて一番感じたことですが、ErlangやPythonの言語スキルよりも
プロトコルやバイトストリームについての知識が必要でした。
この辺の知識は、ふわっとは理解してるつもりでしたが実際に手を動かしてみると
バイトヘッダーにプロトコル仕様のタグ情報やサイズを含めること、
ErlangとPythonのバイトオーダーやパケットサイズを合せる必要あること、
デバッグの方法など、実際に実装してみないとわからないこと多かったです。
この辺はエンジニアとしてステップアップするために
どこかで習得すべき技術なので良い機会となりました。

github: https://github.com/fujimisakari/erlang-port-with-python


### 参考サイト
http://code.activestate.com/recipes/534162-an-erlang-port-in-python/
http://erlang.org/doc/apps/erts/erl_ext_dist.html
http://erlang.g.hatena.ne.jp/lnznt/20110814/1313283961
http://mattn.kaoriya.net/software/lang/c/20091023004100.htm
https://github.com/hdima/erlport
