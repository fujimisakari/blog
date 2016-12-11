title: プログラミングに役立つelisp10選
date: 2016-12-12
tags:
- Emacs
- AdventCalender

---

この記事は[EmacsのAdventCalandar2016](http://qiita.com/advent-calendar/2016/emacs)の12日目の記事です。
今回はプログラミングに役立つelisp10選ということですが、
メジャーなelispでなく若干マイナーであろうと思うelispをセレクトしました。

10選したelispはこんな感じです。
- highlight-symbol
- expand-region
- クォートやブラッケットを一括で括る
- sort-line
- smooth-scroll
- rainbow-delimiters
- swap-window-positions
- bm-goto
- anzu
- ace-jump


## highlight-symbol
三拍子揃ってるelispと思っており、とても重宝してます。
機能としては、カーソル中のシンボルにハイライト(背景色)を設定してくれ、同じシンボル名であればジャンプ移動が可能です。

<img src="/image/emacs/highlight-symbol.gif" width="720" height="420" />

**所感**
これを使いだしたらプログラミング効率がかなり上がったと思います。
よく利用しているケースとしては以下があります。
コード読むときであれば、変数をハイライトしてどの辺で変数が利用されてるかヒントとして使っています。あとバグ探しとかで。
コード書くときであれば、ロジック思考中に利用されてる変数をハイライトして視覚から頭を整理するときに利用します。
ペアプロしてるときであれば、説明しながらハイライトして相手に伝えていきます。

**設定**
```lisp
(require 'highlight-symbol)
(setq highlight-symbol-colors '("LightSeaGreen" "HotPink" "SlateBlue1" "DarkOrange" "SpringGreen1" "tan" "DodgerBlue1"))
(global-set-key (kbd "C-l C-l") 'highlight-symbol-at-point)
```

## expand-region
カーソル中のシンボルを一瞬でリージョン選択してくれます。

<img src="/image/emacs/expand-region.gif" width="881" height="120" />

**所感**
リージョン選択から文字列をコピー or 上書きだったり
他のelispと連動させるためだったりといろんなユースケースで利用できます。
入れるだけで必ず効率化に繋がると思います.

**設定**
```lisp
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
```

## クォートやブラッケットを一括で括る
リージョン選択しているシンボルに対して、シングルクォート or ダブルクォート or 丸括弧 or 角括弧を一括で括ることができます。

<img src="/image/emacs/quote-bracket.gif" width="881" height="120" />

**所感**
これは標準でありそうと思いきや無さそうでしたので自前で用意しました
expand-regionから連動してプログラム書くとき全般に使ってます。

**設定**
```lisp
(defun region-to-single-quote ()
  (interactive)
  (quote-formater "'%s'" "^\\(\"\\).*" ".*\\(\"\\)$"))

(defun region-to-double-quote ()
  (interactive)
  (quote-formater "\"%s\"" "^\\('\\).*" ".*\\('\\)$"))

(defun region-to-bracket ()
  (interactive)
  (quote-formater "\(%s\)" "^\\(\\[\\).*" ".*\\(\\]\\)$"))

(defun region-to-square-bracket ()
  (interactive)
  (quote-formater "\[%s\]" "^\\(\(\\).*" ".*\\(\)\\)$"))

(defun quote-formater (quote-format re-prefix re-suffix)
  (if mark-active
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (replace-func (lambda (re target-text)(replace-regexp-in-string re "" target-text nil nil 1)))
             (text (funcall replace-func re-suffix (funcall replace-func re-prefix region-text))))
        (delete-region (region-beginning) (region-end))
        (insert (format quote-format text)))
    (error "Not Region selection")))

(global-set-key (kbd "M-'") 'region-to-single-quote)    ; 選択リージョンを''で囲む
(global-set-key (kbd "M-\"") 'region-to-double-quote)   ; 選択リージョンを""で囲む
(global-set-key (kbd "M-9") 'region-to-bracket)         ; 選択リージョンを()で囲む
(global-set-key (kbd "M-[") 'region-to-square-bracket)  ; 選択リージョンを[]で囲む
```


## sort-line
リージョン選択した範囲をM-x sort-lineで昇順Sortしてくれます

<img src="/image/emacs/sort-line.gif" width="611" height="300" />

**所感**
Goのようにimportの並び順を他の言語でも整理したくて探してみると標準機能として入ってました。
コーディングスタイルとかを気にする方とかであればオススメできると思います

**設定**
Emacsに標準で入ってます


## smooth-scroll
Emacsでバッファ内の画面移動をヌルヌルスクロールできるようになります。
(キャプチャーとって見たのですがわかりにくかったので割愛します)

**所感**
このelispを利用するかは好みが出ると思います。
僕は画面移動したときに現在のカーソル位置やバッファ内の位置が
わからなくなることがあったんですがこれを導入することで解消されました。

**設定**
```lisp
(require 'smooth-scroll)
(smooth-scroll-mode t)
```

## rainbow-delimiters
ブラケット類にカラーを付けて対象ブラケットの位置がわかりやすくなります

<img src="/image/emacs/rainbow-delimiters.gif" width="690" height="300" />

**所感**
派手なシンタックスハイライトでプログラミングしてる人にはブラケット部分も可視化できるようになるので良いと思います。
逆な方には目がチカチカするので苦痛かも。

**設定**
```lisp
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
(set-face-foreground 'rainbow-delimiters-depth-1-face "SlateBlue2")
(set-face-foreground 'rainbow-delimiters-depth-2-face "DarkOliveGreen2")
(set-face-foreground 'rainbow-delimiters-depth-3-face "CornflowerBlue")
(set-face-foreground 'rainbow-delimiters-depth-4-face "khaki2")
(set-face-foreground 'rainbow-delimiters-depth-4-face "PaleGreen2")
(set-face-foreground 'rainbow-delimiters-depth-5-face "DarkSlateGray2")
(set-face-foreground 'rainbow-delimiters-depth-6-face "LightSalmon2")
(set-face-foreground 'rainbow-delimiters-depth-7-face "magenta2")
(set-face-foreground 'rainbow-delimiters-depth-8-face "IndianRed4")
(set-face-foreground 'rainbow-delimiters-depth-9-face "DeepPink3")
```


## swap-window-positions
画面分割してあるWindow位置の入れ変えを行います。

<img src="/image/emacs/swap-window-positions.gif" width="728" height="532" />

**所感**
画面分割とかを積極的にやる人には便利なelispと思います。
僕は開発中に画面分割して左右にWindowを並べて、左はクラスや関数などの参照用、右は作業用で開発してるのですが
新しいバッファやファイルを開いたときに左右が逆転するときに利用してます。

**設定**
[http://emacswiki.org/emacs/TransposeWindows]()より参照
```lisp
(defun swap-window-positions ()
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))
(global-set-key (kbd "C-M-s") 'swap-window-positions)
```


## bm
任意の行にマーキングしてジャンプ移動します。

<img src="/image/emacs/bm.gif" width="720" height="420" />

**所感**
bmは検索文字入力などが無くマーキングしている行にジャンプ移動するだけなので
非常に短かいキーストロークで高速にバッファ内を移動できます。
プログラム書く時はまず絞り込み検索でいくつか参照したい行をマーキングして書くと効率が良かったです。

**設定**
```lisp
(require 'bm)
(global-set-key (kbd "M-p") 'bm-previous)
(global-set-key (kbd "M-p") 'bm-next)
```

## anzu
既存の置換よりインテリジェンスな置換が行えます。
マッチ数や入力にマッチする文字列をインタラクティブにハイライトしてくれます。

<img src="/image/emacs/anzu.gif" width="720" height="420" />

**所感**
既存の置換コマンドを置き換え用途に合せて一括やインタラクティブを使い分けてます。
プログラミング中であれば変数名や関数名を変更したときによく利用しますので
インタラクティブに確認しながら置換することが多いです。

**設定**
```lisp
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
```

## ace-jump
トリガー文字を入力して画面表示内の任意の場所にカーソルをジャンプ移動できます。

<img src="/image/emacs/ace-jump.gif" width="720" height="420" />

**所感**
使うと打鍵数が圧倒的に減り、慣れ出すとこれ無しではやっていけなくなります。
ただ、最初は物珍しさから使いますますがいつの間にか存在を忘れてしまうので
何も考えずに自然な流れで使えるようになるまで時間がかかります。

**設定**
```lisp
(require 'ace-jump-mode)
(global-set-key (kbd "C-;") 'ace-jump-word-mode)
(global-set-key (kbd "C-:") 'ace-jump-line-mode)
```

## おわりに
いかがでしたでしょうか？ヘビーにEmacsを利用されている方であればおなじみのものが多かったと思います。
プログラミングに役立つelispを10選しましたが、実際に候補を上げていくと20選以上あり
あらためていろんなelispに支えられて快適なプログラミング環境が整備できてることを実感しました。

僕はVimからEmacsに転身したタイプなのですが転身の際は独特のキーバインド操作やelispなどが難しく
どうしても使いやすいと思えずに3度ほど転身に失敗しVimに戻ってました。
4度目の転身は[Emacsテクニックバイブル](https://www.amazon.co.jp/Emacs%E3%83%86%E3%82%AF%E3%83%8B%E3%83%83%E3%82%AF%E3%83%90%E3%82%A4%E3%83%96%E3%83%AB-%EF%BD%9E%E4%BD%9C%E6%A5%AD%E5%8A%B9%E7%8E%87%E3%82%92%E3%82%AB%E3%82%A4%E3%82%BC%E3%83%B3%E3%81%99%E3%82%8B200%E3%81%AE%E6%8A%80%EF%BD%9E-%E3%82%8B%E3%81%B3%E3%81%8D%E3%81%A1/dp/4774143278)が発売された時でとりあえず本の全部のelipsや設定を入れました。
そして初めて使いやすいかもって思えるようになり、そこから少しずつ自分に合う設定やelispを厳選していき
カスタマイズしていくこと楽しさを感じながら自然と転身できました。
Emacsはどこでも自分流にカスタマイズでき、どの言語でもほぼ同じ操作で開発できるので
今ではこれ以外での開発は考えれないと思わせられるぐらい中毒性を感じてます。
これからもEmacsと長く関われ続けれたらと思ってます。

ではよいEmacsライフを!!
