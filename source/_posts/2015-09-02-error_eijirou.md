title: "「英辞郎 + sdic + sary」でエラーが出るようになった"
date: 2015-09-02 01:01:39
tags:
- Emacs

---

いつの頃からかemacsで「英辞郎 + sdic + sary」を利用するしたとき
```lisp
setq: Symbol's value as variable is void: entries
```
のエラーが出るようになってたので
elisp or shellコマンドのどちらが問題なのか調査した。

まずは、検索コマンドのsaryの動作確認してる
```shell
$ /usr/local/bin/sary '<K>this' ~/.emacs.d/share/eijiro/eijirou.sdic
<K>this</K>【代名】これ、この&lf;  【形-1】この&lf;  ・This cake is great. このケーキ、おいしい。&lf;  【形-2】〈話〉こんな、とある◆打ち解 けた会話で何かを新たに提示。テレパシーでも使えるかのように、思い浮かべているものを指す。記憶・心象を包み隠さない親密感と、多少のなれなれしさを伴う。文法的には a, an で置換可能。&lf;  ・I have this boyfriend named Bob. The funny thing is, he has this dog that likes cat food better than dog food. 私、ボブっていうボーイフレンドがいるんだけど。彼が飼ってる犬がね、おかしいの、ドッグフードよりキャットフードが好きなんだ。&lf;  ・I have this strange feeling. なんかこう変な気持ち。&lf;  【副】これくらいに、こんなに、これほどに&lf;  ・I feel good when it is this cold. このくらい寒いときが身体の調子がいい。&lf;  【＠】ジス、ズィス、【変化】《複》these
<K>this afternoon</K>きょうの午後、今日の午後
<K>this all-too-common attitude</K>よくありがちなこの手の態度［姿勢］
<K>this and that</K>あれやこれや、いろいろなこと
```
コマンドの動作は問題なさそう。

次はelispを調査。
edebugで動作の挙動を確認して行ったところ、init.elに設定しているsdicf.el 内の定義されている
arrayコマンドをsaryコマンドに置換してるところが機能していないことがわかった。
問題の解決対応としては、fset関数のlamdaにシングルクォート付けることで機能できるようなった
```lisp
      (fset 'sdicf-array-init 'sdicf-common-init)
      (fset 'sdicf-array-quit 'sdicf-common-quit)
      (fset 'sdicf-array-search
-           (lambda (sdic pattern &optional case regexp)
+           '(lambda (sdic pattern &optional case regexp)
              (sdicf-array-init sdic)
              (if regexp
                  (signal 'sdicf-invalid-method '(regexp))
```
