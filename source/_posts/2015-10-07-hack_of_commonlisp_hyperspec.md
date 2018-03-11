title: EmacsのewwでCommonLispのHyperSpecをHack!
date: 2015-10-07 02:29:50
tags:
- Emacs
- CommonLisp

---

1ヶ月ぐらい前からCommonLispを勉強してるけど、HyperSpecのドキュメントを見るとき
いちいちWebページを見ていたら時間かかるし、シームレスじゃないので目も疲れるし、
いいことなかったのでストレスなく快適て閲覧できるようEmacsのewwでHackした。

検索機能としては、インタラクティブ検索とカーソル検索が2つが用意されてて
とりあえず、こんな感じで見れるようになる

![hyperspec](/image/emacs/hyperspec.gif)


## 導入手順

##### 1. 以下が前提となる
・ Emacs24以上
・ Slimeを導入済み


##### 2. HyperSpecドキュメントをローカルにダウンロードして解凍して任意の場所へ配置する。

``` sh
cd ~/.emacs.d/share
wget ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
tar xvfz HyperSpec-7-0.tar.gz
```


##### 3. 設定ファイルへ追記
slimeのlibに置かれてるhyperspec.elをewwで動作するよう以下の関数を上書く。
・ common-lisp-hyperspec
・ common-lisp-hyperspec-lookup-reader-macro
・ common-lisp-hyperspec-format

``` lisp
  ;; HyperSpecをewwで見る設定
  (setq common-lisp-hyperspec-root "~/.emacs.d/share/HyperSpec/")

  (defun common-lisp-hyperspec (symbol-name)
    (interactive (list (common-lisp-hyperspec-read-symbol-name)))
    (let ((name (common-lisp-hyperspec--strip-cl-package
                 (downcase symbol-name))))
      (cl-maplist (lambda (entry)
                    (eww-open-file (concat common-lisp-hyperspec-root "Body/"
                                           (car entry)))
                    (when (cdr entry)
                      (sleep-for 1.5)))
                  (or (common-lisp-hyperspec--find name)
                      (error "The symbol `%s' is not defined in Common Lisp"
                             symbol-name)))))

  (defun common-lisp-hyperspec-lookup-reader-macro (macro)
    (interactive
     (list
      (let ((completion-ignore-case t))
        (completing-read "Look up reader-macro: "
                         common-lisp-hyperspec--reader-macros nil t
                         (common-lisp-hyperspec-reader-macro-at-point)))))
    (eww-open-file
     (concat common-lisp-hyperspec-root "Body/"
             (gethash macro common-lisp-hyperspec--reader-macros))))

  (defun common-lisp-hyperspec-format (character-name)
    (interactive (list (common-lisp-hyperspec--read-format-character)))
    (cl-maplist (lambda (entry)
                  (eww-open-file (common-lisp-hyperspec-section (car entry))))
                (or (gethash character-name
                             common-lisp-hyperspec--format-characters)
                    (error "The symbol `%s' is not defined in Common Lisp"
                           character-name))))

  (defadvice common-lisp-hyperspec (around common-lisp-hyperspec-around activate)
    (let ((buf (current-buffer)))
      ad-do-it
      (switch-to-buffer buf)
      (pop-to-buffer "*eww*")))

  (defadvice common-lisp-hyperspec-lookup-reader-macro (around common-lisp-hyperspec-lookup-reader-macro-around activate)
    (let ((buf (current-buffer)))
      ad-do-it
      (switch-to-buffer buf)
      (pop-to-buffer "*eww*")))

  (defadvice common-lisp-hyperspec-format (around common-lisp-hyperspec-format activate)
    (let ((buf (current-buffer)))
      ad-do-it
      (switch-to-buffer buf)
      (pop-to-buffer "*eww*")))
```

defadviceは画面分割してポップアップでewwのwindowを開くようにしてる。
これでラクになれるはず。
