title: Pythonでネットワークプログラミング
date: 2016-12-15
tags:
- Python
- AdventCalender

---

[PythonのAdventCalandar2016](http://qiita.com/advent-calendar/2016/python)の15日の記事です。
今回は、Pythonでネットワークプログラミングをサンプルコードと少し解説をしたいと思ってます。

以前、[Linuxネットワークプログラミングバイブル](https://www.amazon.co.jp/Linux%E3%83%8D%E3%83%83%E3%83%88%E3%83%AF%E3%83%BC%E3%82%AF%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E3%83%90%E3%82%A4%E3%83%96%E3%83%AB-%E5%B0%8F%E4%BF%A3%E5%85%89%E4%B9%8B-ebook/dp/B00O8GIL62)を読んで
第5章のIOの多重化の部分が非常に興味深くハンズオンしてきちんと理解したいと思ってたので
C言語で書かれていたロジックをPython3.5.2で書き直してみました。

まず、投稿タイトルのネットワークプログラミングについては、
TCPプロトコルを利用したサーバ側のSocket通信の実装を差してます。
Socket通信にて、シンプルな通信からI/O多重化、非同期I/O、ノンブロッキングI/Oを利用したパターンを紹介し、
これら通信を実現するための技術として、select、 epoll、マルチプロセス、マルチスレッド、asyncioを利用していきます。

サンプルコードのパターンとしては以下があります。
- シンプルなソケット通信
- selectを利用した多重化(I/O多重化)
- epollを利用した多重化(I/O多重化)
- マルチプロセスによる多重化(非同期I/O)
- マルチスレッドによる多重化(非同期I/O)
- プリフォークによる多重化(非同期I/O)
- プリスレッドによる多重化(非同期I/O)
- selectとマルチスレッドを組み合せた多重化(I/O多重化 + 非同期I/O)
- aysncioを利用した多重化(ノンブロッキングI/O)


## シンプルなソケット通信

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
        new_sock.send(b'OK\n')
        new_sock.close()


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

まず、シンプルなソケット通信を実装してみました。
create_server_socket()でサーバーソケットを生成してます。
accept_loop()でクライアントからの接続待ち(accept)、データ受信(recv)、データ送信(send)の一連のループが行われます。
ここでポイントなのが、接続待ち(accept)、データ受信(recv)、データ送信(send)はすべてブロッキング処理というこうです。
ブロッキング処理は名前の通り処理が一時停止して完了するまで次の処理が行わないという意味です。
なので、while内では接続を待ち(accept)が接続を受け付けるまではブロッキングで次の処理に移れないことになります。
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
    server_sock.listen(5)
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
    sock.send(b'OK\n')
    return data


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

今回の実装では、シンプルなソケット通信の実装とは異なり、
1つのクライアントが処理を行っている間でも他のクライアントが並行に処理できるマルチクライアントに実装になります。
マルチクライアントを実現するために、システムコールの`select`を利用します。
`select`は複数のソケットディスクリプタのレディ(読み込み/書き込み可能)の状態を同時に監視でき
レディになったもの返してくれます。
また、レディになってない場合はブロッキングし続けてくれCPUリソースを消費しません。
サンプルでは、サーバーソケットFDとクライアントソケットFDがdescriptorsリストに格納され
`select`でレディの状態監視を行いマルチクライアントを実現してます。
`send`と`recv`に関してはブロッキング処理になるので、この辺りの処理が遅いと全体に影響してきます。



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
    server_sock.listen(5)
    print('Server Run Port:{}'.format(port))
    return server_sock


def accept_loop(server_sock):
    print('Ready For Accept')

    epoll = select.epoll()
    epoll.register(server_sock.fileno(), select.EPOLLIN)

    max_discriptor = 20
    discriptors = {}
    while True:
        events = epoll.poll(1)
        print('Discriptor Count: {}'.format(len(discriptors)))
        for fileno, event in events:
            if fileno == server_sock.fileno():
                new_sock, (remote_host, remote_remport) = server_sock.accept()
                if len(discriptors) > max_discriptor:
                    new_sock.close()
                epoll.register(new_sock.fileno(), select.EPOLLIN)
                discriptors[new_sock.fileno()] = new_sock
                print('[FD:{}]Accept: {}:{}'.format(new_sock.fileno(), remote_host, remote_remport))
            else:
                sock = discriptors[new_sock.fileno()]
                data = send_recv(sock)
                if not data:
                    sock.close()
                    del discriptors[new_sock.fileno()]


def send_recv(sock):
    remote_host, remote_remport = sock.getpeername()
    print('[FD:{}]Client:{}:{}'.format(sock.fileno(), remote_host, remote_remport))

    data = sock.recv(512)
    if data == b'':
        print('[FD:{}]Recv:EOF'.format(sock.fileno()))
        return None

    sock.send(b'OK\n')
    return data


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    try:
        accept_loop(server_sock)
    except KeyboardInterrupt:
        server_sock.close()
```

epollは、selectとはロジックは違いますがほぼ同じ動きをするシステムコールです
違いとしては、selectにはOSで指定されたFDを監視できる制限数がありますが、epollは配列なので制限数はありません。
また、selectの内部では監視対象がレディになったあと、どのFDがレディになったか調べる必要がありますが
epollではレディになったディスクリプタだけが通知されるため、どのFDがレディになったか調べる必要がありません。
そのため、監視する対象が多い(10000を越える)場合パフォーマンスに差が出てきます。
なお、epollはMacでは動作できません。Lixnu環境でのみ動作できます。


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
    server_sock.listen(5)
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
        sock.send(b'OK\n')


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
    server_sock.listen(5)
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
        sock.send(b'OK\n')
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
    server_sock.listen(5)
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
        sock.send(b'OK\n')


if __name__ == '__main__':
    server_sock = create_server_socket(7777)
    global_lock_fd = open('./mprocess.lock', 'w')
    os.unlink('./mprocess.lock')

    print('Ready For Accept')
    for i in range(5):
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
    server_sock.listen(5)
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
        sock.send(b'OK\n')
    sock.close()


if __name__ == '__main__':
    server_sock = create_server_socket(7777)

    print('Ready For Accept')

    thread_list = []
    for _ in range(5):
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


## selectとマルチスレッドを組み合せた多重化(I/O多重化 + 非同期I/O)

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
    server_sock.listen(5)
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
        sock.send(b'OK\n')


if __name__ == '__main__':
    server_sock = create_server_socket(7777)

    thread_count = 5
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
selectやepollを利用してI/O多重化を利用してマルチクライアントを実現しても
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
    sock.listen(5)
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
        await loop.sock_sendall(sock, b'OK\n')


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
このコードではまずacceptのタスクが実行されloop.sock_acceptのawaitで接続受け付けが完了するまで待ちます。
接続を受け付けた後、recv_sendをacceptタスクとは別タスクで実行するようにします。
別タスクで実行してるのでrecv_sendの完了を待たずして、次の処理に移りloop.sock_acceptで再度待つようになります。
recv_sendは完了する(接続が切れる)まで、recvとsendをループするタスクになります。
このacceptとrecv_sendの別々のタスクは一見別々のスレッドで動いてるように感じますがシングルスレッドです。
イベントループがawaitの処理を監視していて完了を受けた処理から逐次処理を実行していく協調スレッドになってます。
また、別々タスクをawaitで非同期処理するためにはブロッキング処理だとイベント駆動でタスクのコンテキストを
切り替えれないのでawaitはノンブロッキングで処理を実施されていなければなりません。


## おわりに
この辺りを勉強すると断片的に理解していた通信やデータの流れが繋って行くことを感じれ
Client → WebServer → WebAPPまでの流れも低レイヤーからイメージできるようになると思いました。
解説してる辺りは、さわりの部分なので[Linuxネットワークプログラミングバイブル](https://www.amazon.co.jp/Linux%E3%83%8D%E3%83%83%E3%83%88%E3%83%AF%E3%83%BC%E3%82%AF%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E3%83%90%E3%82%A4%E3%83%96%E3%83%AB-%E5%B0%8F%E4%BF%A3%E5%85%89%E4%B9%8B-ebook/dp/B00O8GIL62)を読まれる詳細に知ることができ理解がさらに上ると思います。
