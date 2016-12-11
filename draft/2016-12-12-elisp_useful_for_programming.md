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
- ace-jump

## highlight-symbol
これは三拍子揃ってるelispでと思っており、とても重宝してます。
機能としては、カーソル中のシンボルにハイライト(背景色)を設定してくれ、同じシンボル名であればジャンプ移動が可能です。

<img src="/image/emacs/highlight-symbol.gif" width="720" height="420" />

**所感**
よく使ってるケースとしては以下があります。
コード読むときであれば、変数をハイライトしてどの辺で変数が利用されてるかヒントとして使っています。
コード書くときであれば、ロジック思考中に利用されてる変数をハイライトしていき視覚から頭を整理するときに利用します。
ペアプロしてるときであれば、説明しながらハイライトして相手に伝えていきます。

**設定**
```lisp
(require 'highlight-symbol)
(setq highlight-symbol-colors '("LightSeaGreen" "HotPink" "SlateBlue1" "DarkOrange" "SpringGreen1" "tan" "DodgerBlue1"))
(global-set-key (kbd "C-l C-l") 'highlight-symbol-at-point)
```

## expand-region
こいつは地味に便利です。
カーソル中のシンボルを一瞬でリージョン選択してくれます。

<img src="/image/emacs/expand-region.gif" width="881" height="120" />

**所感**
リージョン選択から、文字列をコピー or 上書きだったり、ほかのelispと連動させるためだったりと
いろんなユースケースで利用できます。

**設定**
```lisp
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
```

## クォートやブラッケットを一括で括る
リージョン選択しているシンボルに対して、シングルクォート or ダブルクォート or 丸括弧 or 角括弧で括ることができます。

<img src="/image/emacs/quote-bracket.gif" width="881" height="120" />

**所感**
これは標準でありそうと思いきやなさそうでしたので自前で用意しました
expand-regionでリージョン選択して利用する流れでプログラム書くとき全般に使えます。

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
リージョン選択した範囲をM-x sort-lineで昇順でsortしてくれます

<img src="/image/emacs/sort-line.gif" width="611" height="300" />

**所感**
Goのようにimportの並び順を他の言語でも整理したくて探してみると標準機能として入ってました。
コーディングスタイルとかを気にする方とかであれば重宝できると思います

**設定**
Emacsに標準で入ってます


## smooth-scroll
Emacsでバッファ内のスクロール移動をヌルヌルできるようになります。
(キャプチャーとって見たのですがわかりにくかったので割愛します)

**所感**
このelispを利用するかは好みが出ると思います。
僕はスクロール移動したときに現在のカーソル位置やバッファ内の位置が
わからなくなることがあったんですが導入後にそれが解消されました。

**設定**
```lisp
(require 'smooth-scroll)
(smooth-scroll-mode t)
```

## rainbow-delimiters
ブラケットにカラーを付けて対象ブラケットの位置がわかりやすくなります

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
画面分割してあるWindow位置を入れ変えます。

<img src="/image/emacs/swap-window-positions.gif" width="728" height="532" />

**所感**
画面分割とかを積極的にやる人には便利なelispと思います。
僕は開発中に画面分割して左右にWindowを並べて、左はクラスや関数などの参照用、右は作業用で開発してるのですが
新しいバッファやファイルを開いたときに左右が逆転するときに利用してます。

**設定**
http://emacswiki.org/emacs/TransposeWindowsより参照
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
```


## bm-goto
任意の行にマーキングして瞬間移動します。


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

## まとめ
プログラミングに役立つelispを10選しましたが、実際に候補を上げていくと20選以上あり
あらためていろんなelispに支えられて快適なプログラミング環境を整備できてることを実感しました。
ではよいEmacsライフを!!
