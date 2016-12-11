title: "C#でTCP接続テスト"
date: 2015-11-03 10:07:41
tags:
- C#
- PowerShell

---

C#でTCP接続のテストコードを書いたときのメモ。

やりたいこととしては、
サーバーは送られてきたメッセージによって処理を分けて
クライアントへメッセージを返すだけ。

メッセージの受信、処理、返信は非同期で行う。
非同期対応は、.NET Framework4以降から導入されたTaskクラスを利用していて
4以前だとコンパイルエラーになるので注意。

### Server

```
using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Concurrent;
using System.Threading;

public partial class Server
{
    public static void Main()
    {
        var serverTask = Task.Run(() => TCPServer());
        // 指定したすべての Task オブジェクトの実行が完了するまで待機
        Task.WaitAll(serverTask);
    }

    public static async Task TCPServer()
    {
        byte[] token = Encoding.UTF8.GetBytes("1qaz2wsx");
        byte[] SuccessToken = Encoding.UTF8.GetBytes("OK");
        byte[] FailToken = Encoding.UTF8.GetBytes("NG");
        var port = 8888;

        // TCP接続を待機する
        var listener = new TcpListener(new IPEndPoint(IPAddress.Loopback, port));
        listener.Start();

        while(true)
        {
            // クライアントから接続があったら
            var client = await listener.AcceptTcpClientAsync().ConfigureAwait(false);
            using(var stream = client.GetStream())
            {
                try {
                    // クライアントからのデータを受信
                    var buf = await read(stream, token.Length);
                    Func<byte[], string> EncString = b => Encoding.UTF8.GetString(b, 0, b.Length);

                    // クライアントへレスポンス
                    if (EncString(token) == EncString(buf)) {
                        Console.WriteLine("OK");
                        await stream.WriteAsync(SuccessToken, 0, SuccessToken.Length).ConfigureAwait(false);
                    } else {
                        Console.WriteLine("NG");
                        await stream.WriteAsync(FailToken, 0, FailToken.Length).ConfigureAwait(false);
                    }
                } catch(Exception ex) {
                    Console.WriteLine("Exception");
                    var exMessage = Encoding.UTF8.GetBytes(ex.Message);
                    await stream.WriteAsync(exMessage, 0, exMessage.Length).ConfigureAwait(false);

                    var exStackTrace = Encoding.UTF8.GetBytes(ex.StackTrace);
                    await stream.WriteAsync(exStackTrace, 0, exStackTrace.Length).ConfigureAwait(false);
                }
            }
        }
    }

    static async Task<byte[]> read(NetworkStream stream, int rest)
    {
        var buf = new byte[rest];
        var offset = 0;
        while (rest > 0)
        {
            var readed = await stream.ReadAsync(buf, offset, rest).ConfigureAwait(false);
            Console.WriteLine(readed);
            if (readed == 0 || !stream.DataAvailable)
            {
                Console.WriteLine("break");
                break;
            }
            offset += readed;
            rest -= readed;
        }
        Console.WriteLine("read end");
        return buf;
    }
}
```

### Client(C#版)

```
using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Text;

public class Client
{
    public static void Main()
    {
        string sendMsg = "1qaz2wsx"; // OKの場合
        // string sendMsg = "aaaa";  // NGの場合
        string host = "127.0.0.1";
        int port = 8888;

        // サーバーにTCP接続
        TcpClient tcp = new TcpClient(host, port);
        NetworkStream stream = tcp.GetStream();

        // サーバーにデータを送信する
        byte[] sendBytes = Encoding.UTF8.GetBytes(sendMsg);
        stream.Write(sendBytes, 0, sendBytes.Length);
        Console.WriteLine("send message: " + sendMsg);

        // サーバーから送られたデータを受信する
        MemoryStream ms = new MemoryStream();
        byte[] resBytes = new byte[256];
        int resSize = 0;
        do
        {
            resSize = stream.Read(resBytes, 0, resBytes.Length);
            if (resSize == 0)
            {
                break;
            }
            ms.Write(resBytes, 0, resSize);
        } while (stream.DataAvailable || resBytes[resSize - 1] != '\n');

        string resMsg = Encoding.UTF8.GetString(ms.GetBuffer(), 0, (int)ms.Length);
        Console.WriteLine("response message: " + resMsg);

        ms.Close();
        stream.Close();
        tcp.Close();
    }
}
```

### Client(PowerShell版)

```
$client = New-Object System.Net.Sockets.TcpClient("localhost", 8888)
$stream = $client.GetStream()
$buffer = New-Object System.Byte[] $client.ReceiveBufferSize
$enc = New-Object System.Text.UTF8Encoding

try {
    $sendMsg = "1qaz2wsx";  # OKの場合
    # $sendMsg = "aaaa";  # NGの場合
    $asyncRead = $stream.BeginRead($buffer, 0, $buffer.length, $NULL, $NULL)
    $data = $enc.GetBytes($sendMsg)

    $stream.Write($data, 0, $data.length)
    while ($TRUE) {
        if ($asyncRead.IsCompleted) {
            $bytes = $stream.EndRead($asyncRead)
            $result = $enc.GetString($buffer, 0, $bytes)
            Write-Host ("response message: {0}" -F $result)
            break
        }
    }
} catch [System.IO.IOException] {
    Write-Host "TCP Connect Failed" -ForegroundColor Red -BackgroundColor Black
} finally {
    $stream.Close()
    $client.Close()
}
```

## テスト実行
開発環境はMacでやってるので、Mono Frameworkを利用して
コンパイル(mcs)、実行(mono)を行う

### サーバー側
```
$ mcs TestServer.cs && mono TestServer.exe
4
break
read end
NG
8
break
read end
OK
```

### クライアント側
```
$ mcs Client.cs && mono Client.exe
send message: aaaa
response message: NG

$ mcs TestClient.cs && mono TestClient.exe
send message: 1qaz2wsx
response message: OK
```
