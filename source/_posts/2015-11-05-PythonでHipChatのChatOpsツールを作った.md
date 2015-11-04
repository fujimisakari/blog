title: PythonでHipChatのChatOpsツールを作った
date: 2015-11-05 02:41:14
tags: Python
---

PythonでChatOpsツールを作成したので忘れない内にメモを残す

## ChatOpsとは
ChatOpsは、簡単に言うとChat経由で何か操作(オペレーション)ができるようになるというもの。
Chatの発言を監視(ポーリング)しているプロセスが居て、期待の発言がくるとコマンドを実行したり
実行した結果をChatに書き込んだりする。
名称由来のとしては、GitHub社がチャットを利用したオペレーションツールをHubot作って
こういったChatを利用した操作のことChatOpsと呼ばれてたからではという説があるようです

この辺見るとChatOpsについて詳しく説明されてる
http://qiita.com/bouzuya/items/c7d0ad80c357aab6b696


## 今回やりたいこと
- Chatに書き込まれたコマンドを実行する
- 並列処理はしない(複数ビルドとか走らせるとリソースがなくなって逆に遅くなるので)
- 現在の実行中、順番待ちの処理状況を表示させる


### メイン処理

``` python
import sys
import subprocess
import time
import setting
from tools.import_file import import_file
from tools.hipchat import HipChat


class ChatOps(object):

    def __init__(self, token, room_id):
        self._hipchat = HipChat(token, token, room_id)
        self._prosess_queue = []
        self._current_process = None
        self._current_cmdinfo = None

    def prosess_job(self):
        """
        登録コマンドの実行処理(並列実行はしない)
        """
        while True:
            if self._current_process and self._current_process.poll() == 0:
                self._current_process = None
                self._current_cmdinfo = None

            # 現在実行中の処理が無い場合のみ処理を開始する
            if self._prosess_queue and self._current_process is None:
                cmd_info = self._prosess_queue.pop(0)
                self._current_cmdinfo = cmd_info
                self._current_process = cmd_info['func'](cmd_info['user'],
                                                         cmd_info['date'][:19],
                                                         cmd_info['message'].replace('mo_deploy', ''))
            yield None

    def polling_job(self):
        """
        ChatOps部屋のコマンド監視、コマンド登録
        """
        allow_time = 0

        while True:
            if time.time() > allow_time:
                history_list = self._hipchat.get_history()
                if history_list:
                    for history in history_list:
                        if any(history['message'].startswith(x) for x in ['./deploy.sh']):
                            command_info = {'func': self.deploy_process,
                                            'date': history['date'],
                                            'user': history['from']['mention_name'],
                                            'message': history['message']}
                            self._prosess_queue.append(command_info)
                        if any(history['message'].startswith(x) for x in ['chat_status']):
                            self._hipchat.send_notify(self.get_status(), 'yellow')
                allow_time = time.time() + 5  # ポーリング間隔は5秒
            yield None

    def deploy_process(self, user, date, message):
        cmd = message.split()
        return subprocess.Popen(cmd, cwd='./', stdout=1, stderr=1)

    def get_status(self):
        massege = u'============ 実行中 ==============\n'
        if self._current_cmdinfo:
            massege += u'{} / {}   {}\n'.format(self._current_cmdinfo['date'][11:19],
                                                self._current_cmdinfo['user'],
                                                self._current_cmdinfo['message'])
        else:
            massege += u'none.\n'

        massege += u'\n============ 実行待ち ============\n'
        if self._prosess_queue:
            for num, info in enumerate(self._prosess_queue, 1):
                massege += u'{}. {} / {}   {}\n'.format(num,
                                                        info['date'][11:19],
                                                        info['user'],
                                                        info['message'])
        else:
            massege += u'none.\n'
        return massege

    def run(self):
        prosess_job = self.prosess_job()
        polling_job = self.polling_job()

        try:
            while True:
                try:
                    prosess_job.next()
                    polling_job.next()
                    time.sleep(3)
                except KeyboardInterrupt:
                    print '!!!! keyboard interrupted !!!!'
                    raise
        finally:
            if self._current_process:
                self._current_process.terminate()


if __name__ == '__main__':
    access_token = sys.argv[1]
    room_id = sys.argv[2]
    ChatOps(access_token, room_id).run()
```

### Hipchat処理

``` python
import requests
import json


class HipChat(object):

    def __init__(self, token, room_id, timezone='Asia/Tokyo', message_format='text'):
        self._token = token
        self._room_id = room_id
        self._timezone = timezone
        self._not_before_id = None
        self._message_format = message_format

    def send_notify(self, message, color, notify=False, timeout=60):
        url = 'https://api.hipchat.com/v2/room/{}/notification'.format(self._room_id)
        data = {
            'message_format': self._message_format,
            'message': message,
            'color': color,
            'notify': notify,
        }
        headers = {'Content-Type': 'application/json',
                   'Authorization': 'Bearer {}'.format(self._token)}

        res = requests.post(url, data=json.dumps(data), headers=headers, timeout=timeout)
        return res

    def get_history(self, timeout=60):
        is_set_before_id = True if self._not_before_id else False
        url = 'http://api.hipchat.com/v2/room/{}/history/latest'.format(self._room_id)
        data = {
            'auth_token': self._token,
            'timezone': self._timezone,
        }
        if is_set_before_id:
            data['not-before'] = self._not_before_id

        r = requests.get(url, params=data, timeout=timeout)
        try:
            item_list = json.loads(r.text)['items']
            self._not_before_id = item_list[-1]['id']

            if is_set_before_id:
                item_list = item_list[1:]
            else:
                item_list = []
            return item_list
        except:
            return []

```

## 実装について
コマンドの並列処理は行わないけど、ポーリングとコマンド実行は並列で動作させる
必要があるのでPythonのジェネレータを利用しました。

流れとしては、ポーリングして該当コマンドあればlamdaでQueueにコマンドを詰め
QueueにコマンドがあればSubProcessでコマンドを実行します。
コマンドは別プロセスで実行されますのでメイン処理止めることなく実行できます。

またポーリングとQueueからコマンドの取り出しをジェネレータで行ってますので
協調スレッドとなりお互い干渉せず並列で動作してるかのようになってます。
