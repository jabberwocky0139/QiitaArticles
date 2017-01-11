<!---
	新春 Emacsゆるふわコーデ大会(仮)
-->

名前は[新春 Ubuntuゆるふわコーデ大会](http://qiita.com/jabberwocky0139/items/704cc8c48379c97edd82)に引きずられていますが, 目指すところはちょっと違います. 新年ネタでもありません. 

# あらまし
ちょっと長い前置きです. **内容の半分以上は冗談です.** さらっと読み流して, あまり鵜呑みにしないでください. 

## 戦況の変化
 エディタ界の歴史地区といえば「Emacs vs Vi(m)」ですが, ここに食い込まんばかりの勢いを持ったエディタが現れました. Atomです:

![](images/editor2.png)
青がEmacs, 赤がVim, 黄色がAtom

リリースされてからの伸びがすごいです. ここ数年に限って言えばAtomの一人勝ちと言ってもいいかもしれません. 少なくとも新規参入者の数で言えばAtomの圧勝でしょう. 「テキストエディタなんて古い. これからはIDEだ」と言われて久しいですが, そんな時代にこの伸びは驚異的です[^1].

その理由の一つとして, Emacs・Vimに比べてキーバインドに癖がないことが挙げられます. 「Ctrl-c Ctrl-v でコピペ」「Ctrl-fで検索」といったWindowsのショートカットキーとの親和性が高いことからも敷居の低さが伺えます. 

## Emacs・Vimの勝機
でもプログラマの方々なら「カーソル移動にカーソルキーなんてありえん」と思う方も多いと思います. 自分はEmacs・Vimの強いところは, 多機能であることは当然として, 「ホームポジションから移動せずにカーソル移動ができる」「すべての機能に(マウスを使わず)キーボードから容易にアクセスできる」ことの2点だと思っています[^2]. これを高いレベルで実現できるのはEmacs・Vimの他に無く, これこそが慣れてしまうと他のエディタに戻れなくなる所以なのでしょう.

Atomでもプラグインを駆使すればEmacs・Vimに相当するような機能を追加することはできますが, 20年以上(Emacsは30年以上)に渡り秘伝のタレを熟成させてきた古参にはそれ相応のアドバンテージがあるはずです[^3]...が, それについて語りだすと長くなってしまうので省略. 

## しかしながら
とはいえいくら古参に底力があってもAtomに入信した方がEmacs・Vimに改宗することはあまり期待出来ないように思えますし, またその必要も無いでしょう. 古参が再び日の目を見る時代が来るのかどうかは, 僕にはよくわかりません. 

ちなみにグラフを見る限りではVim人気は下げ止まりの様子が伺えます. 実際最近のVimコミュニティはなかなかの盛り上がりを見せているようで, Emacs派としては羨ましい限りです. 一方Emacsは...どこまで下がるのでしょうか. **(日本の)Emacs界隈は, るびきちさんがひとり気を吐いているような空気を感じます[^2].**

さて, これほどAtomが盛り上がった理由は本当に「敷居の低さ」だけなのでしょうか. 

## ※ただしイケメンに限る
もうこの一言に尽きます. Atomはイケメンだった:

![](images/editor1.png)
[Atom本家より](https://atom.io/)

![](images/editor3.png)
[Vim](https://ja.wikipedia.org/wiki/Vim)・[Emacs](https://ja.wikipedia.org/wiki/Emacs) from Wikipedia

どんなにEmacsが多機能でも, Vimのモードが優れていても, それを主張するのがイケメンでなければどうにもならない...

### 例をあげよう
|        | Atom  | Emacs|Vim|
|--------|-------|------|---|
| 年齢 | 新進気鋭   | おっさん | おっさん |
| 多機能 | 便利   | 煩雑 |煩雑     |
| カスタマイズ | 簡単   | Emacs Lisp(笑 |Vim Script(笑|

参考:[Pixiv](http://dic.pixiv.net/a/%E2%80%BB%E3%81%9F%E3%81%A0%E3%81%97%E3%82%A4%E3%82%B1%E3%83%A1%E3%83%B3%E3%81%AB%E9%99%90%E3%82%8B)

...流石に自虐が過ぎました. とはいえ, 見た目のインパクトはAtomの圧勝です. ちょっと古いバージョンの画像を持ってきたのは不公平な気もしますが, たぶんどう見積もっても負けてます. これはもう整形なくして勝ち目[^4]はない...！

## イケメン『Spacemacs』顕る
ざっくり言うとSpacemacsは「Emacs + Vim = 最強」という理念のもと生まれたプロジェクトです. 

[Vimも秘伝のタレも飲み込むEmacsの超強力ディストリビューションSpacemacsまとめ](http://qiita.com/ryosukes/items/d0ec5094a9d3d636f7bb)

こちらの記事が詳しいです. どれだけ便利かということはとりあえず置いておいて...

![](images/editor4.png)
[Spacemacs](https://github.com/syl20bnr/spacemacs) from Github

**イケメンの息吹を放ち, 圧倒的な説得力を携えています.** 「EmacsかVimはじめてみたい」という方には是非是非おすすめしたいです. Emacs + VimというアイデアではありますがベースはEmacsなので, これを足ががりにもっとEmacs界隈も盛り上がることを期待しています. もう少し日本語の情報が増えてくるといいな.

## Spacemacsやめました
しかし, 自分はSpacemacsをやめてしまいました. 
Spacemacsの設定ファイルである`.spacemacs`は`init.el`とは異なる様式を持っています. デフォルトで様々な機能が有効になって便利である一方, その設定が一部隠蔽されている[^5]ことから`.spacemacs`をEmacs Lispでカスタマイズしようとするとどうもうまくいかない部分があり, 結局しっくり来ず普通のGNU Emacsに戻ってきてしまいました. 

はじめからSpacemacsで入門していればこんな壁には遭遇しなかったでしょう. なまじ`init.el`をこねくり回してきた手前, その遺産が活かしづらいとなると移行を踏みとどまってしまいます. 

## はやくゆるふわになりたい
とはいえSpacemacsのスタイルには惹かれるものがあります. `init.el`をいじってSpacemacsのいいところを取り込み, Emacsをイケメンに...いや, ゆるふわコーデを施しましょう！

## 方針
1. 初めてEmacsに入門する方 
→ Spacemacsをはじめましょう. 以下お読みに必要はありません
2.  Emacsを使っているものの有り難みがよくわからないという方
→ Spacemacsをはじめましょう. 以下お読みに必要はありません
3.  `init.el`が十分熟成しており, かつEmacsをゆるふわにしたい方 
→ **お進みください**
4.  `init.el`が十分熟成しており, かつ無骨なEmacsを愛している方 
→ 以下お読みに必要はありません

いじるポイントは

* パッケージ管理(package.el)
* カラーテーマ(solarized-theme)
* モードライン(powerline)
* タブ(tabbar)
* メニューバー・ツールバー・スクロールバー
* フォント(ゆたぽん)
* スプラッシュ(dashboard)

の7点です. ゆるふわであることに加えてエディタとして機能的であることを重視します.

以下からが本題です. ここまで読んだ方なら, この長い前置きには特に意味がなかったことにお気づきのことでしょう.

# パッケージ管理(package.el)
Spacemacsは様々な機能がデフォルトで有効になっていますが, これは単にEmacs自体の機能だけではなく追加のパッケージに依るものが多いです. Spacemacsの初回起動は少し時間がかかりますが, これは必要なパッケージを`package.el`を通じてインストールしているからです. 

この`package.el`はとっても便利です. どのパッケージが必要かを`init.el`に書いておけば, 起動時に勝手にダウンロードしてきてくれるのです. 今までパッケージは手動でダウンロードして`dotfiles`とかにまとめてGithubで管理してきましたが, もうその必要もありません！

最近では`El-Get`とか`Cask`とかが人気なようですが自分はうまく使いこなせなかったこともあって, Emacs標準の`package.el`に落ち着いています. `(package-install 'パッケージ名)`と入力して`eval-buffer`すればインストールしてくれます. 

```emacs-lisp
;;; list-packageの設定
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;; 起動に時間がかかるようであれば, 普段は以下をコメントアウトしてください
(package-refresh-contents)

(package-install 'helm) ;helmを自動でインストールしてくれます. 
```
これでEmacsの可搬性がすごく高まります.

# カラーテーマ(solarized-theme)
カラーテーマこそ, ゆるふわ度における最も重要な要素であるように思えますが, あんまり明るい色合いだと目が疲れるのでちょっと暗目にします. ついでに画面を見にくくならない程度に透過させるようにします:

```emacs-lisp
(package-install 'color-theme-solarized) ;自動インストール
(load-theme 'solarized-dark t) ;暗め
;; (load-theme 'solarized-light t) ;明るめ

;; 透過
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))
;; 初期値
(set-frame-parameter nil 'alpha 92)
```
# モードライン(powerline)
ここがSpacemacsをイケメンたらしめている最大のポイントかと思います. メジャーモードやマイナーモード, バッファ名, 行数などの情報が詰まっているバーです. これをかっこ良くするのは`powerline`と呼ばれるパッケージです. `powerline`はもともとVimのパッケージらしいです. こういうお互いのいいところを取り入れる姿勢は「Emacs vs Vim」の対立(？)構造におけるポジティブな産物だと思います. 

```emacs-lisp
(package-install 'powerline) ;自動インストール
(require 'powerline)

(defconst color1 "SteelBlue")
(defconst color2 "salmon")

(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background color1
		            :bold t
                    :box nil)

(set-face-attribute 'powerline-active1 nil
                    :foreground "gray23"
                    :background color2
					:bold t
					:box nil
                    :inherit 'mode-line)

(set-face-attribute 'powerline-active2 nil
                    :foreground "white smoke"
                    :background "gray20"
					:bold t
					:box nil
                    :inherit 'mode-line)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fff"
                    :background color1
					:bold t
                    :box nil)

(set-face-attribute 'powerline-inactive1 nil
                    :foreground "gray23"
                    :background color2
					:bold t
					:box nil
                    :inherit 'mode-line)

(set-face-attribute 'powerline-inactive2 nil
                    :foreground "white smoke"
                    :background "gray20"
					:bold t
					:box nil
                    :inherit 'mode-line)

(powerline-center-theme)
```
色合いはお好きに決めてください. 自分は[こちら](https://skalldan.wordpress.com/2012/05/13/emacs-%E3%81%AE%E3%83%A2%E3%83%BC%E3%83%89%E3%83%A9%E3%82%A4%E3%83%B3%E3%82%92-powerline-%E9%A2%A8%E3%81%AB%E3%82%AB%E3%82%B9%E3%82%BF%E3%83%9E%E3%82%A4%E3%82%BA/)や[こちら](http://blog.shibayu36.org/entry/2014/02/11/160945)を参考にさせていただきました.

![](images/editor5.png)
こんなふうになりました. 

# タブ(tabbar)
今まで自分は`elscreen`を使っていたのですが, どうも見た目のカスタムがしづらいようなので`tabbar`に乗り換えました. 使用感は随分違いますが, ゆるふわのためなら致し方ない. `elscreen`は自分でタブを生成して表示するバッファを選べますが, `tabbar`は基本すべてのバッファをタブに表示します[^6]. そもそも自分はバッファをガンガン溜めるような使い方はしていないので`tabbar`でも特に困ることはありませんでした:

```emacs-lisp
(package-install 'tabbar) ;自動インストール

;; tabbar
(require 'tabbar)
(tabbar-mode 1)

;; グループ化しない
(setq tabbar-buffer-groups-function nil)

;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

;; タブの長さ
(setq tabbar-separator '(2.2))

;; キーに割り当てる
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)

;; 外観変更
(set-face-attribute
 'tabbar-default nil
 :family "MeiryoKe_Gothic"
 :family "ゆたココ" 
 :background "#34495E"
 :foreground "#fff"
 :bold nil
 :height 0.95
 )
(set-face-attribute
 'tabbar-unselected nil
 :background "#34495E"
 :foreground "#fff"
 :bold nil
 :box nil
)
(set-face-attribute
 'tabbar-modified nil
 :background color2
 :foreground "gray23"
 :bold t
 :box nil
)
(set-face-attribute
 'tabbar-selected nil
 :background color1
 :foreground "#fff"
 :bold nil
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box nil)
(set-face-attribute
 'tabbar-separator nil
 :height 2.0)

;; タブに表示させるバッファの設定
(defvar my-tabbar-displayed-buffers
  '("*vc-")
  "*Regexps matches buffer names always included tabs.")

(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
```
[こちら](http://blog.bokuweb.me/entry/emcas-nyumon)を参考にさせていただきました. 

![](images/editor6.png)

色合いは`powerline`に合わせました. タブの切り替えはC-\<TAB\>, C-S-\<TAB\>です. Chromeも同じキーバインドを持っています.

# メニューバー・ツールバー・スクロールバー
SpacemacsからGNU Emacsに戻ってきた時に, イケメン度以外の違和感を感じました. Spacemacsではメニューバーとツールバーが無効になっていたのです. Emacsに慣れた方ならどちらも不要でしょう. スクロールバーがなくてもモードラインの右端を見ればバッファのどのへんに居るかはすぐにわかります:

```emacs-lisp
;;; ツールバーを非表示
(tool-bar-mode -1)
;;; メニューバーを非表示
(menu-bar-mode -1)
;;; スクロールバーを非表示
(scroll-bar-mode -1)
```
ゆるふわというよりスマート！

# フォント(ゆたぽん)
[これ](http://net2.system.to/pc/font.html)は最強のゆるふわフォントです. 個人的には「(コーディング)バックスラッシュ」がおすすめ:

```emacs-lisp
(set-face-attribute 'default nil
		    :family "ゆたぽん（コーディング）Backsl"
		    :height 130)
```
フォントサイズはお好みで

# スプラッシュ(dashboard)
〆です. デフォルトのスプラッシュ(\*About GNU Emacs\*バッファ)はすこしごちゃごちゃしている上, Spacemacsのロゴの方がかっこいいです. 変えてしまいましょう. Spacemacsと似たスプラッシュを提供するパッケージが`dashboard`です. これでスプラッシュ画面を簡単にカスタマイズできます. 

## ロゴはどうする？
井上トロさん一択でしょう. GIMPなどで`solarized-theme`とマッチするように加工します:

```emacs-lisp
(package-install 'dashboard) ;自動インストール
;; 起動画面をdashboardで変更
(require 'dashboard)
;; Set the title
(setq dashboard-banner-logo-title "Is Emacs my heartthrob?")
;; Set the banner
(setq dashboard-startup-banner "/home/jabberwocky/.emacs.d/Toro/Toro.png")
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 20)))
```

# まとめ
![](images/editor7.png) 

かくして年末の暇つぶしが終了しました. 方向性がずいぶんとっちらかった駄文になってしまいましたが, ここまでお読み頂きありがとうございました.

[^1]: 出始めで盛り上がってるだけと邪推することもできるかもしれませんが...
[^2]: 異論は認めます. 
[^3]: 熟成させすぎて腐ってる部分もあるかも...そのひとつがEmacsのunexec問題なのかもしれません.
[^4]: 熟練のギークの方にとっては心底どうでもいい話題でしょう.
[^5]: ように当時の自分は感じました.
[^6]: 表示させないものも設定できます. 
