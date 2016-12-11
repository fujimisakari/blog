title: Emacsのewwでmarkdownをプレビュー表示する
date: 2016-01-16 19:23:18
tags: Emacs

---

Emacsでmarkdownをプレビュー確認したいなーと思い
探してみたら以下が見つかった。

http://qiita.com/garaemon/items/2a551f6da3380950b21c

ただ、もう使ってなw3mだったのでewwでも見れるようにした。

``` lisp
(defun markdown-preview-by-eww ()
  (interactive)
  (message (buffer-file-name))
  (call-process "grip" nil nil nil
                (buffer-file-name)
                "--export"
                "/tmp/grip.html")
  (let ((buf (current-buffer)))
    (eww-open-file "/tmp/grip.html")
    (switch-to-buffer buf)
    (pop-to-buffer "*eww*")))
```

こんな感じになる
![markdown-preview](/image/emacs/markdown-preview-by-eww.png)
