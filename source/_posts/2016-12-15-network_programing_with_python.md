title: Pythonでネットワークプログラミング
date: 2016-12-15
tags:
- Python
- AdventCalender

---

この記事は[PythonのAdventCalandar2016](http://qiita.com/advent-calendar/2016/python)の15日目の記事です。
[Goでネットワークプログラミング](http://blog.fujimisakari.com/network_programing_with_go/)の投稿に引き続き、Pythonでネットワークプログラミングの
いくつかのパターンを実装してみましたのでサンプルコードと簡単な解説をしたいと思ってます。

以前、[Linuxネットワークプログラミングバイブル](https://www.amazon.co.jp/Linux%E3%83%8D%E3%83%83%E3%83%88%E3%83%AF%E3%83%BC%E3%82%AF%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E3%83%90%E3%82%A4%E3%83%96%E3%83%AB-%E5%B0%8F%E4%BF%A3%E5%85%89%E4%B9%8B-ebook/dp/B00O8GIL62)を読んで
第5章のIOの多重化の部分が非常に興味深くハンズオンしてきちんと理解したいと思ってたので
C言語で書かれていたロジックをPython3.5.2で書き直してみました。

まず、投稿タイトルのネットワークプログラミングについては、
TCPプロトコルを利用したサーバ側のSocket通信の実装を指してます。
Socket通信にて、シンプルな通信からI/O多重化、非同期I/O、ノンブロッキングI/Oを利用したパターンを紹介し、
これらの実装を実現するための技術として、select、 epoll、マルチプロセス、マルチスレッド、asyncioを利用していきます。
そして、最後にベンチマークをとってます。

Socket通信実装パターンとしては以下があります。
- シンプルなSocket通信
- selectを利用した多重化(I/O多重化)
- epollを利用した多重化(I/O多重化)
- マルチプロセスによる多重化(非同期I/O)
- マルチスレッドによる多重化(非同期I/O)
- プリフォークによる多重化(非同期I/O)
- プリスレッドによる多重化(非同期I/O)
- selectとプリスレッドを組み合せた多重化(I/O多重化 + 非同期I/O)
- aysncioを利用した多重化(ノンブロッキングI/O)


## シンプルなSocket通信

```python
import socket

"""
シングルプロセス・シングルスレッド・シングルクライアント
"""


def create_server_socket(port):
    # ソケットディスクリプタを生成
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # ソケットオプションでソケットの再利用フラグをONに設定
    # ONにしていないとクライアントと通信途中で中断した場合同じアドレスとポートでバインドできなくなります
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    # ソケットにポート番号をバインド
    server_sock.bind(('', port))
    # アクセスバックログ(接続待ちのキュー数)を指定。
    server_sock.listen(5)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')

    while True:
        new_sock, (remote_host, remote_remport) = server_sock.accept()
        print('[FD:{}]Accept:{}:{}'.format(new_sock.fileno(), remote_host, remote_remport))
        data = new_sock.recv(512)
        print('[FD:{}]Recv:{}'.format(new_sock.fileno(), data))
        new_sock.send(data)
        new_sock.close()


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

まず、シンプルなSocket通信を実装してみました。
`create_server_socket()`で接続の受け口となるソケットを生成してます。
`accept_loop()`でクライアントからの接続待ち(accept)、データ受信(recv)、データ送信(send)の一連のループが行われます。
ここでポイントなのが、接続待ち(accept)、データ受信(recv)、データ送信(send)はすべてブロッキング処理というこうです。
ブロッキング処理は名前の通り処理が一時停止して完了するまで次の処理が行わないという意味です。
なので、while内では接続を待ち(accept)が接続を受け付けるまではブロッキングされ次の処理に移れないことになります。
そのため、この実装では1つのクライアントが処理を行っている間は他のクライアントからは処理は行えません。


## selectを利用した多重化(I/O多重化)

```python
import socket
import select

"""
シングルプロセス・シングルスレッド・マルチクライアント(select)
"""


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')
    # max_descriptor = 20
    descriptors = [server_sock]

    while True:
        print('Descriptor Count:{}'.format(len(descriptors) - 1))
        r, _, _ = select.select(descriptors, [], [])
        for sock in r:
            if sock == server_sock:
                # if len(descriptors) > max_descriptor:
                #     sock.close()
                new_sock, (remote_host, remote_remport) = sock.accept()
                print('[FD:{}]Accept:{}:{}'.format(new_sock.fileno(), remote_host, remote_remport))
                descriptors.append(new_sock)
            else:
                data = send_recv(sock)
                if not data:
                    sock.close()
                    descriptors.remove(sock)


def send_recv(sock):
    remote_host, remote_remport = sock.getpeername()
    print('[FD:{}]Client:{}:{}'.format(sock.fileno(), remote_host, remote_remport))

    data = sock.recv(512)
    if data == '':
        print('[FD:{}]Recv:EOF'.format(sock.fileno()))
        return None

    print('[FD:{}]Recv:{}'.format(sock.fileno(), data))
    sock.send(data)
    return data


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

今回の実装では、シンプルなSocket通信の実装とは異なり、
1つのクライアントが処理を行っている間でも他のクライアントが並行に処理できるマルチクライアントに実装になります。
マルチクライアントを実現するために、システムコールの`select`を利用します。
`select`は複数のソケットディスクリプタのレディ(読み込み/書き込み可能)の状態を同時に監視することができ
レディになったもの返してくれます。
また、レディになってない場合はブロッキングし続けてくれCPUリソースを消費しません。
サンプルでは、サーバーソケットFDとクライアントソケットFDが`descriptors`リストに格納され
`select`でレディの状態監視を行いマルチクライアントを実現してます。
sendとrecvに関してはブロッキング処理になるので、この辺りの処理が遅いと全体に影響してきます。



## epollを利用した多重化(I/O多重化)

```python
import socket
import select

"""
シングルプロセス・シングルスレッド・マルチクライアント(epoll)
"""


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')

    epoll = select.epoll()
    epoll.register(server_sock.fileno(), select.EPOLLIN)

    # max_discriptor = 20
    discriptors = {}
    while True:
        events = epoll.poll()
        print('Discriptor Count: {}'.format(len(discriptors)))
        for fileno, event in events:
            if fileno == server_sock.fileno():
                new_sock, (remote_host, remote_remport) = server_sock.accept()
                # if len(discriptors) > max_discriptor:
                #     new_sock.close()
                epoll.register(new_sock.fileno(), select.EPOLLIN)
                discriptors[new_sock.fileno()] = new_sock
                print('[FD:{}]Accept: {}:{}'.format(new_sock.fileno(), remote_host, remote_remport))
            else:
                sock = discriptors[fileno]
                data = send_recv(sock)
                if not data:
                    sock.close()
                    del discriptors[fileno]


def send_recv(sock):
    remote_host, remote_remport = sock.getpeername()
    print('[FD:{}]Client:{}:{}'.format(sock.fileno(), remote_host, remote_remport))

    data = sock.recv(512)
    if data == b'':
        print('[FD:{}]Recv:EOF'.format(sock.fileno()))
        return None

    sock.send(data)
    return data


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

`epoll`は、`select`とはロジックは違いますがほぼ同じ動きをするシステムコールです
違いとしては、`select`にはOSで指定されたFDを監視できる制限数がありますが、`epoll`は配列なので制限数はありません。
また、`select`の内部では監視対象がレディになったあと、どのFDがレディになったか調べる必要がありますが
epollではレディになったディスクリプタだけが通知されるため、どのFDがレディになったか調べる必要がありません。
そのため、監視する対象が多い(10000を越える)場合パフォーマンスに差が出てきます。
なお、`epoll`はMacでは動作できません。Lixnu環境でのみ動作できます。


## マルチプロセスによる多重化(非同期I/O)

```python
import os
import sys
import socket

"""
マルチプロセス・シングルスレッド・マルチクライアント
"""


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')
    while True:
        new_sock, (remote_host, remote_remport) = server_sock.accept()

        pid = os.fork()
        if pid == 0:
            # Child Precess
            server_sock.close()
            send_recv_loop(new_sock)
            new_sock.close()
            sys.exit()
        elif pid > 0:
            # Parent Precess
            print('[PID:{}]Accept:{}:{}'.format(os.getpid(), remote_host, remote_remport))
            new_sock.close()
        else:
            # Fork Fail
            new_sock.close()


def send_recv_loop(sock):
    while True:
        pid = os.getpid()
        data = sock.recv(512)
        remote_host, remote_remport = sock.getpeername()
        print('[PID:{}]Client:{}:{}'.format(pid, remote_host, remote_remport))

        if data == b'':
            print('[PID:{}]Recv:EOF'.format(pid))
            break

        print('[PID:{}]Recv:{}'.format(pid, data))
        sock.send(data)


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

マルチプロセスでの実装としては、コネクション毎にプロセスをforkしてマルチクライアントを実現します。
マルチプロセスで実装すると、ロジック自体はシンプルに書けますので可読性が高くなりますが
プロセス生成は重い処理で数が増えてるくとコンテキストスイッチが多くなりCPUのオーバヘッドが上がります。



## マルチスレッドによる多重化(非同期I/O)

```python
import socket
import threading

"""
シングルプロセス・マルチスレッド・マルチクライアント
"""


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')
    while True:
        new_sock, (remote_host, remote_remport) = server_sock.accept()
        thread = threading.Thread(target=send_recv_thread, args=(new_sock,))
        thread.start()
        print('<{}>Accept:{}:{}'.format(thread.ident, remote_host, remote_remport))


def send_recv_thread(sock):
    while True:
        data = sock.recv(512)
        remote_host, remote_remport = sock.getpeername()
        ident = threading.currentThread().ident
        print('<{}>Client:{}:{}'.format(ident, remote_host, remote_remport))

        if data == b'':
            print('<{}>Recv:EOF'.format(ident))
            break

        print('<{}>Recv:{}'.format(ident, data))
        sock.send(data)
    sock.close()


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

マルチプロセス同様にマルチスレッドでの実装も、コネクション毎にスレッドを生成してマルチクライアントを実現します。
スレッドの生成はプロセス生成よりも軽いもののエラーが起きると全スレッドが落ちてしまうリスク伴います。
また、スレッド間でstatic領域が共用されるので安全にプログラムを動作させる難易度が高くなります。


## プリフォークによる多重化(非同期I/O)

```python
import fcntl
import os
import sys
import socket

"""
マルチプロセス・シングルスレッド・マルチクライアント
"""


global_lock_fd = None


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    pid = os.getpid()
    while True:
        print('[PID:{}]Get Start'.format(pid))
        fcntl.lockf(global_lock_fd.fileno(), fcntl.LOCK_EX)
        print('[PID:{}]Get Lock'.format(pid))
        new_sock, (remote_host, remote_remport) = server_sock.accept()
        print('[PID:{}]Accept:{}:{}'.format(pid, remote_host, remote_remport))
        fcntl.lockf(global_lock_fd.fileno(), fcntl.LOCK_UN)
        print('[PID:{}]Unlock'.format(pid))
        send_recv_loop(new_sock)
        new_sock.close()


def send_recv_loop(sock):
    while True:
        pid = os.getpid()
        data = sock.recv(512)
        remote_host, remote_remport = sock.getpeername()
        print('[PID:{}]Client:{}:{}'.format(pid, remote_host, remote_remport))

        if data == b'':
            print('[PID:{}]Recv:EOF'.format(pid))
            break

        print('[PID:{}]Recv:{}'.format(pid, data))
        sock.send(data)


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    global_lock_fd = open('./mprocess.lock', 'w')
    os.unlink('./mprocess.lock')

    print('Ready For Accept')
    for i in range(10):
        pid = os.fork()
        if pid == 0:
            accept_loop(server_sock)
            sys.exit()

    try:
        os.wait()
    except KeyboardInterrupt:
        server_sock.close()
        global_lock_fd.close()
```

プリフォークでの多重化は、事前に子プロセスをforkで生成してマルチクライアントを実現します。
子プロセス毎にサーバソケットを準備しようとすると同じアドレス・ポートで複数bindできないのでエラーになりますので
サーバソケットは1つで、accept以降を子プロセスで処理する流れになります。
また複数の子プロセスでaccept待ちを行うと重複して反応してしまう可能性がありますので
必ず1つの子プロセスのみが反応するように排他制御を行います。
今回はロックファイルを利用して排他制御を実装しており、その他の処理の「マルチプロセスによる多重化」と同じです。


## プリスレッドによる多重化(非同期I/O)

```python
import time
import threading
import socket

"""
シングルプロセス・マルチスレッド・マルチクライアント
"""


LOCK = threading.Lock()


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_thread(server_sock):
    ident = threading.currentThread().ident
    while True:
        print('<{}>Start'.format(ident))
        LOCK.acquire()
        print('<{}>Get Lock'.format(ident))
        new_sock, (remote_host, remote_remport) = server_sock.accept()
        print('<{}>Accept:{}:{}'.format(ident, remote_host, remote_remport))
        LOCK.release()
        print('<{}>Release Lock'.format(ident))
        send_recv_loop(ident, new_sock)
        new_sock.close()


def send_recv_loop(ident, sock):
    while True:
        data = sock.recv(512)
        remote_host, remote_remport = sock.getpeername()
        print('<{}>Client:{}:{}'.format(ident, remote_host, remote_remport))

        if data == b'':
            print('<{}>Recv:EOF'.format(ident))
            break

        print('<{}>Recv:{}'.format(ident, data))
        sock.send(data)
    sock.close()


if __name__ == '__main__':
    server_sock = create_server_socket(7777)

    print('Ready For Accept')

    thread_list = []
    for _ in range(40):
        thread = threading.Thread(target=accept_thread, args=(server_sock,))
        thread.start()
        thread_list.append(thread)

    for thread in thread_list:
        thread.join()

    server_sock.close()
```

プリスレッドでの多重化は、事前にスレッドを生成してマルチクライアントを実現します。
処理は「プリフォークでの多重化」と同じで、サーバソケットは1つ、accept以降をスレッドで処理する流れになります。
排他制御の処理はスレッドロックの実装でその他の処理は「マルチスレッドによる多重化」と同じです。


## selectとプリスレッドを組み合せた多重化(I/O多重化 + 非同期I/O)

```python
# -*- coding: utf-8 -*-

import socket
import threading

from Queue import Queue
import select

"""
シングルプロセス・マルチスレッド・マルチクライアント(select)
"""


class SendQueue(Queue):
    SENTINEL = object()

    def __iter__(self):
        while True:
            item = self.get()
            try:
                if item is self.SENTINEL:
                    yield None, None
                yield item
            finally:
                self.task_done()

    def close(self):
        self.put(self.SENTINEL)


send_queue = SendQueue()


def create_server_socket(port):
    server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(('', port))
    server_sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')
    max_descriptor = 20
    descriptors = [server_sock]

    while True:
        print('Descriptor Count:{}'.format(len(descriptors) - 1))
        r, _, _ = select.select(descriptors, [], [])
        for sock in r:
            if sock == server_sock:
                if len(descriptors) > max_descriptor:
                    sock.close()
                new_socket, (remote_host, remote_remport) = sock.accept()
                print('[FD:{}]Accept:{}:{}'.format(new_socket.fileno(), remote_host, remote_remport))
                descriptors.append(new_socket)
            else:
                remote_host, remote_remport = sock.getpeername()
                print('[FD:{}]Client:{}:{}'.format(sock.fileno(), remote_host, remote_remport))
                data = sock.recv(512)
                if data == b'':
                    print('[FD:{}]Recv:EOF'.format(sock.fileno()))
                    sock.close()
                    descriptors.remove(sock)
                else:
                    send_queue.put((sock, data))


def send_thread():
    current_thread = threading.current_thread()
    for sock, data in send_queue:
        if not sock:
            break
        print('[FD:{}]<{}>Recv:{}'.format(sock.fileno(), current_thread.ident, data))
        sock.send(data)


if __name__ == '__main__':
    server_sock = create_server_socket(7777)

    thread_count = 40
    thread_list = []
    for _ in range(thread_count):
        thread = threading.Thread(target=send_thread)
        thread.start()
        thread_list.append(thread)

    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        for _ in range(thread_count):
            send_queue.close()
        for thread in thread_list:
            thread.join()

        send_queue.join()
        server_sock.close()
```

I/O多重化と非同期I/Oを組み合わせた実装パターンになります。
`select`や`epoll`を利用してI/O多重化を利用してマルチクライアントを実現しても
接続待ち(accept)、データ受信(recv)、データ送信(send)の一連の処理が同じ流れで処理されるため
sendで時間がかかったるとブロッキングが発生し、他のクライアントに対しの処理が止ってしまいます。
そこで接続待ち(accept)、データ受信(recv)までをI/O多重化で処理し、
データ送信(send)はマルチスレッドで非同期I/Oを利用することで
ブロッキングが発生しないマルチクライアントを実現します。


## aysncioを利用した多重化(ノンブロッキングI/O)

```python
import asyncio
import socket

"""
シングルプロセス・シングルスレッド・マルチクライアント(asyncio)
"""


def create_server_socket(port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.setblocking(False)
    sock.bind(('', port))
    sock.listen(256)
    print('Server Run Port:{}'.format(port))
    return sock


async def accept(loop, sock):
    print('Ready For Accept')

    while True:
        new_socket, (remote_host, remote_remport) = await loop.sock_accept(sock)
        new_socket.setblocking(False)
        print('[FD:{}]Accept:{}:{}'.format(new_socket.fileno(), remote_host, remote_remport))
        asyncio.ensure_future(recv_send(loop, new_socket))


async def recv_send(loop, sock):
    remote_host, remote_remport = sock.getpeername()
    print('[FD:{}]Client:{}:{}'.format(sock.fileno(), remote_host, remote_remport))

    while True:
        data = await loop.sock_recv(sock, 512)
        if data == b'':
            print('[FD:{}]Recv:EOF'.format(sock.fileno()))
            sock.close()
            break

        print('[FD:{}]Recv:{}'.format(sock.fileno(), data))
        await loop.sock_sendall(sock, data)


if __name__ == '__main__':
    event_loop = asyncio.SelectorEventLoop()
    asyncio.set_event_loop(event_loop)
    server_sock = create_server_socket(7777)

    try:
        event_loop.run_until_complete(accept(event_loop, server_sock))
    except KeyboardInterrupt:
        event_loop.close()
        server_sock.close()
```

最後にasyncioを利用した非同期処理でマルチクライアントを実現してます。
asyncioのイベントループでタスクを実行すると任意の順番で処理を制御できるようになります。
このコードではまずacceptのタスクが実行され`loop.sock_accept()`のawaitで接続受け付けが完了するまで待ちます。
接続を受け付けた後、`recv_send()`をacceptタスクとは別タスクで実行するようにします。
別タスクで実行してるので`recv_send()`の完了を待たずして、次の処理に移り`loop.sock_accept()`で再度待つようになります。
`recv_send()`は完了する(接続が切れる)まで、recvとsendをループするタスクになります。
この`accept()`と`recv_send()`の別々のタスクは一見別々のスレッドで動いてるように感じますがシングルスレッドです。
イベントループがawaitの処理を監視していて完了を受けた処理から逐次処理を実行していく協調スレッドになってます。
また、別々タスクをawaitで非同期処理するためにはブロッキング処理だとイベント駆動でタスクのコンテキストを
切り替えれないのでawaitはノンブロッキングで処理を実施されていなければなりません。


## ベンチマーク(Mac)

#### 同時接続数:50、送受信数: 500(100x50)

|                     |     接続時間 |    送受信時間 |       合計時間 |         ランク |
| :-----------------: | :----------: | :-----------: | :------------: | :------------: |
| select              |      0.02013 |       0.00022 |        0.02035 |              1 |
| goroutine           |      0.02115 |       0.00024 |        0.02139 |              2 |
| preThread           |      0.02216 |       0.00036 |        0.02252 |              3 |
| prefork             |      0.02237 |       0.00038 |        0.02275 |              4 |
| select + prethread  |      0.02261 |       0.00016 |        0.02277 |              5 |
| async               |      0.02297 |       0.00027 |        0.02324 |              6 |


#### 同時接続数:150、送受信数: 500(100x50)

|                     |     接続時間 |    送受信時間 |       合計時間 |         ランク |
| :-----------------: | :----------: | :-----------: | :------------: | :------------: |
| goroutine           |      0.04503 |       0.00012 |        0.04515 |              1 |
| asyncio             |      0.05703 |       0.00039 |        0.05742 |              2 |
| select + prethread  |      0.05873 |       0.00044 |        0.05917 |              3 |
| preThread           |      0.06096 |       0.00048 |        0.06144 |              4 |
| select              |      0.06757 |       0.00041 |        0.06799 |              5 |
| prefork             |      0.08339 |       0.00061 |        0.08400 |              6 |


上記の条件でベンチマークを取りました。(goroutineはGoの投稿記事のコードを利用してます)
接続時間、送受信時間、合計時間の値は1回の処理の平均値になります。

同時接続数:50の結果についてはどれも対差はありません。
小規模での開発であればどの技術を選定しても実際はあまり差は出ないことがわかります。

同時接続数:150の場合は差が顕著に出てきます。
マルチプロセスで処理を行ってるpreforkは、コンテキストスイッチのオーバヘッドのせいか接続時間、送受信時間がかなり上ってます。
マルスレッド系もプロセスよりはコンテキストスイッチのオーバヘッドは少ないですが時間の上り幅はやや高めです。
軽量スレッド系のgoroutine、asyncioは高速にコンテキストスイッチしてハードウェアの性能を限界まで生かせるので時間の上り幅が緩やかです。

※ Macのスペック上、同時接続数150で計測したのですが1000ぐらいで試すとより詳細なデータが取れると思います。


## 2016.12.16 追記

### ベンチマーク(Linux)

#### 同時接続数:100、送受信数: 500(100x50)

|                     |     接続時間 |    送受信時間 |       合計時間 |         ランク |
| :-----------------: | :----------: | :-----------: | :------------: | :------------: |
| goroutine           |      0.01988 |       0.00005 |        0.01993 |              1 |
| asyncio             |      0.18910 |       0.00099 |        0.04190 |              2 |
| prefork             |      0.04151 |       0.00004 |        0.07017 |              3 |
| select + prethread  |      0.23038 |       0.00119 |        0.23157 |              4 |
| epoll               |      0.32977 |       0.00139 |        0.33116 |              5 |
| select              |      0.35244 |       0.00131 |        0.35375 |              6 |
| preThread           |      0.42452 |       0.00170 |        0.42622 |              7 |


#### 同時接続数:1000、送受信数: 500(100x50)

|                     |     接続時間 |    送受信時間 |       合計時間 |         ランク |
| :-----------------: | :----------: | :-----------: | :------------: | :------------: |
| prefork             |      0.33203 |       0.00122 |        0.33325 |              1 |
| goroutine           |      0.33314 |       0.00041 |        0.33355 |              2 |
| asyncio             |      0.43914 |       0.00069 |        0.43983 |              3 |
| select              |      0.53973 |       0.00118 |        0.54091 |              4 |
| epoll               |      0.61166 |       0.00113 |        0.61279 |              5 |
| select + prethread  |      0.63557 |       0.00121 |        0.63678 |              6 |
| preThread           |      0.98197 |       0.00114 |        0.98311 |              7 |


計測環境はさくらのVPSなんでMacのスペック比べると高くはないです。CPUは以下になります

```sh
$ cat /proc/cpuinfo | grep 'model name' | uniq
model name      : Westmere E56xx/L56xx/X56xx (Nehalem-C)
```

Linuxでベンチマークとってみていくつか気付きと謎がありました
- preforkはパフォーマンスが低いと思ってたのですがパフォーマンスが以外と高かった
- epollはselectの上位互換なはずなのなぜ負けてるのか
- Kenさんがコメントされてるようにasyncioの中ではepollが選択されてるはずなのになぜ処理時間に差が出てるのか

この辺は今後の課題と調べていく必要がありそうです。
もし詳しい方いらしたらぜひ教授いただけると助かります。


### ベンチマークの計測方法

計測方法を記載してませんでしたので追記します。
ベンチマークは以下のclientスクリプトを実行して計測してます。
同時接続数は`WORKER_COUNT`。
送受信数は`ACTION_COUNT`x`SEND_RECV_COUNT`。
になります。

```python
# -*- coding: utf-8 -*-

import socket
import threading
import time

WORKER_COUNT = 150
ACTION_COUNT = 100
SEND_RECV_COUNT = 50


class WorkerInfo(object):

    def __init__(self):
        self.thread_name = ''
        self.is_active = True
        self.connect_speed = 0.0
        self.send_recv_speed = 0.0
        self.action_count = 0

    def create_send_msg(self, send_recv_count):
        return b'{}:{}:{}'.format(self.thread_name, self.action_count, send_recv_count)

worker_info_list = []


def create_client_socket(port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('', port))
    return sock


def worker_send_recv(worker_info):
    # connect
    connect_start_time = time.time()
    sock = create_client_socket(7777)
    worker_info.connect_speed += time.time() - connect_start_time

    # send and recv
    send_recv_start_time = time.time()
    for cnt in range(SEND_RECV_COUNT, 1):
        send_msg = worker_info.create_send_msg(cnt)
        sock.send(send_msg)
        recv_msg = sock.recv(512)
        if recv_msg != send_msg:
            print('error')
    sock.close()
    worker_info.send_recv_speed += time.time() - send_recv_start_time


def worker(worker_idx):
    worker_info = worker_info_list[worker_idx]
    worker_info.thread_name = threading.current_thread().name
    print(worker_info.thread_name)

    for cnt in range(ACTION_COUNT):
        worker_info.action_count = cnt
        worker_send_recv(worker_info)

    # Average calculation
    worker_info.is_active = False
    worker_info.connect_speed /= ACTION_COUNT
    worker_info.send_recv_speed /= ACTION_COUNT


if __name__ == '__main__':
    # Test Start
    thread_list = []
    for idx in range(WORKER_COUNT):
        worker_info_list.append(WorkerInfo())
        thread = threading.Thread(target=worker, name='thread:{}'.format(idx), args=(idx,))
        thread.start()
        thread_list.append(thread)

    for thread in thread_list:
        thread.join()

    # All Thread Average calculation
    connect_speed = 0
    send_recv_speed = 0
    for idx in range(WORKER_COUNT):
        _worker_info = worker_info_list[idx]
        connect_speed += _worker_info.connect_speed
        send_recv_speed += _worker_info.send_recv_speed

    # Result
    connect_speed = round((connect_speed / WORKER_COUNT), 5)
    send_recv_speed = round((send_recv_speed / WORKER_COUNT), 5)
    total_speed = connect_speed + send_recv_speed
    print('connect_speed: {}'.format(connect_speed))
    print('send_recv_speed: {}'.format(send_recv_speed))
    print('total_speed: {}'.format(total_speed))

```
