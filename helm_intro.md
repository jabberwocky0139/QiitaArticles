<!---
	初心者〜初級者のためのHelm事始め : 前編
-->

# 対象
Emacs及びHelmの初心者〜初級者を対象にしています. これを書いているのは初級者です.

# あらまし
HelmはEmacsにおける様々なものを検索するための統一的なインターフェイスを提供してくれる非常に大きなパッケージです. とっても便利らしいのだけど, 機能がいっぱいありすぎてちょっと手に余る子だなと思っていました. 実際自分が使えているのはほんの一部の機能だけです. 勉強しようにもどこから手を付けてよいかわからない状態でした. 

そんなときにHelmの主要な機能についてうまくまとめてくれている記事を見つけました. 

[A Package in a league of its own: `Helm`](https://tuhdo.github.io/helm-intro.html)

tuhdoさんという方が書いてくれているようです. ありがたや. これほどHelmの機能について包括的にまとめている日本語の情報源もあまりないなと思ったので翻訳してみました. これを読めばHelm中級者くらいになれるんじゃないかと期待しています. 既に中級者以上の方にも新しい気づきがあるかもしれません. 

また, tuhdoさんはHelm以外にもEmacsに関するガイドをいくつか書いていらっしゃいます.

## 注意点

* 内容は**翻訳に近い要約**です. 情報量で言えば「原文 > この記事」です. 原文と違う表現をしている箇所が多々あります.

* 自分の英語力とEmacs力が圧倒的に足りないので, 誤訳であったりtuhdoさんの意図を汲みとり切れていないところが多々あると思います. **この記事に至らないどころを感じるとすれば, その責任はすべて自分にあります.**

* ぜひ原文の方も目を通してみてください. ここには載せていない, GIFによるデモ動画も載っています. 「ここの訳違うよ！」「こう書いたほうがいいと思う」などあれば, コメント欄や編集リクエストにてやさしく教えて頂けると幸いです.


# A Package in a league of its own: `Helm`

**作者さんたち:**

* Tamas Patrovic (-2007). 最初の開発者さん. このときは`Anything`という名前でした. 

* るびきち (2008-2011). このときもまだ`Anything`.

* Thierry Volpiatto (2011-). 現在のメンテナさん. このときに`Anything`は`Helm`に生まれ変わりました.

**ホームページ:** [GitHub](https://github.com/emacs-helm/helm)

**`Helm`の特徴**

`Helm`はEmacsでインクリメンタルに補完や検索をするためのフレームワークです. Emacsで何か(Bufferやファイルなど)を探しているときに, 役に立ってくれます.

Helmは`anything.el`(Tamas Patrovicさん作)のフォークで, 言わば`anything.el`のご子息です. `anything.el`のレガシーコードをお掃除することに始まり, いろんなツールを用意してくれています. しかもそれらは下方互換に縛られるようなことはありません.

**インストール:**

[Emacs Prelude](https://github.com/bbatsov/prelude)か[Spacemacs](https://github.com/syl20bnr/spacemacs)では最初からセットアップされており, このガイドにあるような設定は必要ないです. でも[Emacs Prelude](https://github.com/bbatsov/prelude)ではHelmがデフォルトで無効になってます. [ここ](https://github.com/bbatsov/prelude#helm)を見て有効にしてください. [Spacemacs](https://github.com/syl20bnr/spacemacs)はデフォルトで有効です. 

もしSpacemacsユーザーなら, もうなんにもしなくていいです. ふつうのEmacsユーザーなら`M-x list-packages`で**helm**を選んでインストールしましょう. インストールが終わったら以下の設定を追加してhelmを有効にしましょう. 

最小設定:

```emacs-lisp
(require 'helm-config)
(helm-mode 1)
```

拡張設定:

```elisp
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)
```

## つかいかた
Helmを使うと「ふつうのEmacsと使用感ぜんぜん違う！」と思うでしょう. だけど一度Helmに慣れるともう離れられなくなります. もしも, Helmがあんまり好きじゃなかったらIdoを使いましょう. Idoについては後ほど紹介します. さて, helmの使い方を見ていきましょう. 

Helmによる補完はふつうのEmacsの補完とはすいぶん異なります:

* Helmでは補完に**TAB**を使いません.

* まず探している子の名前の一部を入力します. スペース区切りでand検索もできます. この「名前の一部」を**patterns**と呼びます. Patternsは正規表現でもいいです.

* Helmはこの**patterns**に一致する子たちを探し, 一致する順にソートしてバッファに表示してくれます. **RET**で選択できます. 

* Helmのバッファ内では**C-p**と**C-n**でカーソルを動かしたり, **C-v**と**M-v**でページを移動したりできます. 

* **C-SPC**で候補にmarkすることができます. 複数の候補に同じ操作をしたい時などに便利です. **M-a**で全選択(mark)できます. 

* マークした候補を**C-c C-i**でカレントバッファに挿入できます. 

* **C-t**でHelmバッファを縦分割にできます. もう一度**C-t**を入力すると横分割に戻ります. 

**C-x b**で`helm-mini`が起動するので, それで練習してみましょう. 2つ以上のバッファをmarkして**RET**すると, markしたものすべてを開きます. 

**だいじなこと:** *決して補完に**TAB**を使わないことを覚えておいてください. ふつうのEmacsやIdoとは違います. Helmではpatternsを打ちこめば**TAB**を押さなくても候補が自動的に絞られます. これはHelmの特徴であり, 欠点ではありません. **TAB**で候補を絞る習慣は忘れましょう！Helmバッファでpatternsの素早い補完をしたければ, `hippie-expand`が従来の**TAB**の役割を担ってくれます. Helmを使いはじめるとこの**TAB**まわりの挙動が大きな混乱を招きますが, 慣れたら絶対に好きになるって！*

Helmコマンドを実行したいときはHelmセッションに入りましょう.

> Helmセッションとは, 例えば`helm-mini`とか`helm-find-files`とかのことを指します.

HelmセッションにいるときはHelmバッファが常に開いており, Helmセッションから出るとそのバッファは閉じられます. Helmセッションにいるときは以下の3つのコマンドを覚えておかなきゃいけません:

* **TAB**action menuを表示する**helm-select-action**です. action menuを開くとカレントHelmセッションから出て, markした候補に対するactionのリストが表示されます. `Find File`とか`Find Dired`とか`Grep File`とかで使えます. 

* **C-z**は**helm-excute-persistent-action**です. persistent actionを実行してもHelmセッションから出ることはありません. 

> persistent actionとは, つまり従来の**TAB**補完に相当するものです. 

* `helm-find-files`や`helm-mini`とかのいくつかのHelmセッションでは, `grep`とか`open`とかのアクションを複数の候補に対して実行できます. 

...なんですが, **TAB**と**C-z**を入れ替えちゃいましょう. **helm-select-action**よりも**helm-excute-persistent-action**の方がよく使うので, このほうが**TAB**を便利に使えるでしょう:

```emacs-lisp
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
```
Helmセッションでは**C-c ?**がヘルプにあたり, マニュアルを参照できます. 

## Helmのいいところ

* ***シンプルで一貫性のあるインターフェイス***: どのHelmセッションもシンプルで共通のインターフェイスを持っています: patternsを入力するミニバッファと, 候補を表示するHelmバッファです. インターフェイスがシンプルなので入門も容易ですね.

* ***対話型***: Helmはとっても対話的な子です: ユーザーがタイプすればすぐにHelmバッファの候補が更新されます. この特徴のおかげで, Helmは他には無いようなユニークなコマンドを持っています. 例えば`helm-ff-run-grep`などがあり, この子はgrepの結果をタイプするごとに更新してくれます. 

* ***まず探しているものを. 何をするかはあとで***: Helmではお目当ての物を探しあてるまで, 何を実行するかは考える必要がないのです. たとえば, とあるファイルをカレントウィンドウに開くか, それとも別のウィンドウに開くかは, ふつう**ファイルを開く前に**決めなければいけませんが, Helmではまずファイルを探すことに集中し, **見つけたあとに**カレントウィンドウに開くなり別ウィンドウに開くなりを決めればよいのです. これは大きなアドバンテージでして, 何を実行するかの目測を誤ってもコマンドを中止する必要がないのです. たとえばふつう, **C-x C-f**を実行したあとに「別ウィンドウに開いたほうがいいな」と思ったら, **C-g**でキャンセルして**C-x 4 C-f**を実行しなおさなければならないのです.

* ***マッチング***: これはHelmの他にはないとっても強力な特徴です. そう, 正規表現です. regexpのようにsearch patternを入力できるのです. 

> <font color="HotPink">...以下省略...</font>
<!---あたらしいプロジェクトを探しているようなケースでは, Helmを使っていると, プロジェクトの構造をインタラクティブに"学ぶ"ことができるのです. Linuxのカーネルソースに初めて触った私は, `x86`アーキテクチャにある`main.c`を探してさまよっています. `x86`ディレクトリがソースツリーのどこかにあることと, その中に`main.c`があることはわかっています. そのファイル名は`main.c`か`x86-main.c`であるはずです. これらの断片的な情報でもって, -->

>なぜ省略しているかというと, 自分は正規表現について語れることが何一つ無いからです. 誤訳が恐ろしくて省略しました. 是非原文を読んでください. デモ動画もあります. 

* ***パフォーマンス***: Helmは候補のリストが30000個程度なら問題なく動きます.

## カーソル付近のテキスト

既にHelmセッションにいるなら, 以下のコマンドによってカレントバッファの入力を引っ張ってくることができます:

* **C-w**で, カレントバッファのカーソル位置の後ろにある単語をHelmミニバッファにヤンクできます. 

* **M-n**でカーソル位置にあるシンボルをヤンクできます. 

`helm-mode`が有効なら, ヘルプコマンドでも同様にカーソル位置のシンボルを引っ張ってくることができます:

* **C-h f**は`describe-function`で, カーソル位置のシンボルを検索してくれます. 

* **C-h v**は`describe-variable`で, 右に同じ. 

* **C-h w**は`where-is`で, 右に同じ. 

これらのコマンドは自動的にHelmを利用します.

## Autoresize
`helm-autoresize-mode`が有効なら, Helmバッファのサイズを候補の数に応じて自動的にリサイズしてくれます. 

```emacs-lisp
(helm-autoresize-mode t)
```
バッファの大きさの最大値と最小値をカスタマイズできます. いじる変数は

* `helm-autoresize-max-height`

* `helm-autoresize-min-height`

デフォルトでは`helm-autoresize-max-height`は40に設定されており, これはHelmバッファサイズの最大値がフレーム全体の40％であることを意味しています. `helm-autoresize-min-height`についても同様です.

もしHelmウィンドウを勝手にリサイズされたくなかったら, `helm-autoresize-max-height`と`helm-autoresize-min-height`を同じ値に設定すればよいです. 

もし[golden-ratio](https://github.com/roman/golden-ratio.el)を使ってたら, Helmウィンドウについては無効にしてください(Spacemacsを使ってたら以下の設定はいらないです):

```emacs-lisp
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
```

## コマンド: `helm-M-x`
**キーバインド**:

デフォルトではバインドされていません. 以下で追加しましょう:

```emacs-lisp
(global-set-key (kbd "M-x") 'helm-M-x)
```

**解説**: 

`M-x`の機能と`helm-M-x`との違いを見てみましょう. `M-x`でEmacsにおけるコマンドのリストを見ることができます. そのリストをみて「コマンド多すぎでしょうが！」と思うかもしれません. でも`M-x`のことは嫌いにならないであげてください. 少し下まで読めばありがたみがわかるんで！

では`li pa`とタイプしてみましょう. すると`list-packages`が候補のいちばん上に躍り出ました！ `pa ^li`も同様にタイプしてみましょう. 

`helm-M-x`はデフォルトの`M-x`よりも優秀な子です. なぜなら, 候補の横にそのコマンドのキーバインドも表示してくれるし, **TAB**でコマンドのドキュメントも表示してくれます. 

バージョン1.6.5では`helm-M-x`はあいまい一致検索(fuzzy matching)が可能ですが, デフォルトでは無効になっています. 有効にするには以下のようにします:

```emacs-lisp
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
```

**注釈**: `helm-M-x`を**M-x**に手動で設定する必要があります. 設定しなくともデフォルトの`M-x`でもHelmの補完機能は使えますが, キーバインドの表示はしてくれませんし, **TAB**でドキュメントを参照することもできません. もうひとつの重要なことは, prefix argumentは`helm-M-x`を実行したあとに入力することです. `helm-M-x`を実行する前にprefix argumentを入力してもなにも起こりません. 

> prefix argumentとは`C-u 3...`みたいな子です. この子は「あとに続くコマンドを3回呼び出しなさい」という意味です. 

![](./images/figure21.png)

## コマンド: `helm-show-kill-ring`
**キーバインド**:

デフォルトではバインドされていません. 以下で追加しましょう:

```emacs-lisp
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
```

**解説**: 

`C-y`のあとに`M-y`を押すことでキルリングをぐるぐる回れることを覚えていますか？ ただこの機能はデフォルトではとっても使いづらいです. キルリングに登録されたものをいちいち覚えていられないですからね. キルリングを見るためには**C-h v**のあとに`kill-ring`とタイプしなければなりません. あまりよい方法ではないでしょう. 

`helm-show-kill-ring`はこの問題を解決してくれます. Helmはキルリングを見やすい形式で表示してくれて, かつ**patterns**で絞り込みができます. これでデフォルトの`M-y`の負担から解放されますね. 

上記の設定で`M-y`は`helm-show-kill-ring`にバインドされます. 是非とも使ってみてください. デフォルトよりも随分と楽になりますよ. 

![](./images/figure22.png)

## コマンド: `helm-mini`
**キーバインド**:

デフォルトではバインドされていません. 以下で追加しましょう:

```emacs-lisp
(global-set-key (kbd "C-x b") 'helm-mini)
```
あいまい一致を有効にするには以下を追加しましょう:

```emacs-lisp
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
```

**解説**: 

`helm-mini`はいろいろなソースを包括的に扱えます:

* 現在開いているバッファを`Buffers`に表示します

* 最近開いたファイルと`Recentf`に表示します

* `Create Buffer`を**RET**することで新しいバッファを作ります

**<left>**と**<right>**で異なるグループ間を行き来できます. scroll down/upとか**C-v**や**M-v**も普通に使えます. 

patternsとして`*<major-mode>`のように入力すると, そのメジャーモードに該当する候補を表示します. 例えば`*dired`と入力すると, Diredバッファに絞り込んでくれます. `*!dired`と入力すると, Dired modeじゃないバッファを表示します. 

patternsとして`/directory`のように入力するとディレクトリ単位の検索もできます. 例えば`/.emacs.d/`と入力すれば, `.emacs.d`内のファイルに絞り込みます. `!/.emacs.d/`とすると, `.emacs.d`にないバッファを表示します. 

![](./images/figure23.png)

`helm-mini`はpatternsに`@`を前置することで, バッファ名ではなくバッファの内容で絞り込むこともできます. 正規表現もOKです. 例えば, `test`という文字を含んだバッファを絞り込みたければpatternsを`@test`とすればよいのです. さらにそのバッファ内の何行目に`test`という文字が含まれているのかが知りたければ, **C-s**と入力します. すると, `helm-mini`セッションは`helm-moccur`にスイッチされ, 候補が表示されます. 

![](./images/figure24.png)

バッファの文字色とprefixの意味は以下の通りです:

* リモートバッファには`@`がついています. 

* <font color="Red">Red</font> => 外部プロセスによって書き換えられたバッファ

* <font color="#EE6363">Indianred2</font> => バッファは存在してるけどファイルは削除されてしまったもの

* <font color="Orange">Orange</font> => バッファの内容が変更されたが保存されていないもの

* *Italic* => ファイルではないバッファ

Emacsのテーマによって色は変わります. 自分のカラーテーマではどのように対応しているのかを確認しておきましょう. 

例:

* `*lisp ^helm @moc`と入力すると「lisp-modeで, ファイル名が"helm"からはじまり, バッファの内容に"moc"を含むもの」に絞り込まれます. 

* 絞り込むメジャーモードを追加したければ, `,`で区切ります. 例えば`*!lisp,!sh,!fun`と入力すると「lisp-modeでもsh-modeでもfundamental-modeでもないバッファ」に絞り込まれます. 

* `*lisp ^helm moc`は`moc`の前に`@`がついていないので「lisp-modeで, ファイル名が"helm"からはじまり, さらに"moc"を含むもの」に絞り込みます. 

* `*!lisp !helm`は「lisp-modeでなく, ファイル名に"helm"を含まないもの」に絞り込みます. 

* `/helm/ w3`は「"helm"ディレクトリ内の, ファイル名に"w3"を含むもの」に絞り込みます. 

`helm-mini`は`ibuffer`の対話型バージョンのようなものです. 


*類似のコマンド*:

* `helm-multi-files`: BuffersとRecentfとFiles from Current Directoryのリストを表示します. `helm-mini`ならファイルが見つからなかったときは一番上にCreate bufferが来て, 名前がpatternのバッファを作ることができますが, `helm-multi-file`はブランクバッファが表示されます. 

![](./images/figure25.png)

* `helm-locate`はシステム内すべてのファイルを検索できます.

* `helm-buffers-list`: `helm-mini`と似ていますが, 最近開いたファイルを`recentf`ではなく`ido-virtual-buffers`で参照します. これは`ido`が提供する, 最近開いたファイルを管理する子です. virtual bufferはパスを含んでいません. 設定のしかたによっては`helm-mini`の代わりにもなります. `helm-buffers-fuzzy-matching`を有効にしていると, `ido-virtual-buffers`でもあいまい一致が有効になります. 

## コマンド: `helm-find-files`

**キーバインド**:

デフォルトでも**\<helm-prefix\> C-x C-f**でバインドされていますが, 長いので以下のようにするといいでしょう:

```emacs-lisp
(global-set-key (kbd "C-x C-f") 'helm-mini)
```

**解説**: 

`helm-find-files`は強化されたファイルナビゲーターです. 

* `helm-find-files`はカレントディレクトリにおけるあいまい一致をサポートします. たとえば"fob"や"fbr"のようなミスタイプでも"foobar"を補完できます.

* **C-z**でファイル名補完ができます...が, この記事の最初のほうでやった設定では**TAB**に設定し直しています. up/downや**M-<next>, M-<prior>**でスクロールできます. 

* **C-j**でハイライトした候補に絞り込み, もう一度**C-j**を入力するとバッファの中身が覗けます. **C-l**で戻れます. 

* **C-l**でひとつ上のディレクトリに移動できます. **C-r**で戻れます. 

* ディレクトリを新しく作るには重複しない名前をpatternに入力した後, 最後に`/`を追加して**RET**. その後は作成したディレクトリに入ります. 

* ファイルを新しく作るには, ファイル名を入力してHelmバッファの一番上に表示される`[?]`が先頭についた候補を**RET**します. 

* **C-s**で`grep`っぽいことができます. **C-u C-s**でrecursive grepです. 

* patternの最後に`~/`を入力するとホームディレクトリにすぐ戻れます.

* patternの最後に`/`を入力するとルートディレクトリにすぐ戻れます.

* patternの最後に`./`を入力するとカレントディレクトリにすぐ戻れます.

`helm-select-action`を使えばもっといろんなことができます. デフォルトでは**TAB**, この記事の設定では**C-z**にバインドされています. action menuにあるactionについてのガイドは[Exploring large projects with Projectile and Helm Projectile](http://tuhdo.github.io/helm-projectile.html)に書いています. なんでここに書かないのかというと, 結局のところ[Projectile](https://github.com/bbatsov/projectile)を使ったほうがもっと効率的にファイルのナビゲーションが行えるからです.

![](./images/figure26.png)


Find file at point:

`ffap`って知ってます？`helm-find-files`でもできます. ファイルパスに相当する文字列にカーソルを合わせて`M-x ffap`とすると, そのファイルパスがミニバッファに入力された状態になります. しかし, もう`ffap`を覚える必要はありません. カーソルを合わせて`C-x C-f`でOKです.


履歴:

`helm-find-files`のセッションで**C-c h**を入力すると, 過去に訪れたファイルを表示してくれます.

## コマンド: `helm-semantic-or-imenu`
**キーバインド**:

**\<prefix\> i** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 
Imenuはいろいろな定義を探す方法を提供します. たとえば関数定義とか変数定義とか. `imenu`も`helm-semantic-or-imenu`と独立に実行できます. 

Semanticを有効にすると, 訪れたファイルの内容を自動的にパースしてくれます. このパーサはC/C++ではきっちり動いてくれます. `(semantic-mode 1)`で有効にできます. 

> 文法解釈を自動的にやってくれるんですね. その結果, 変数なり関数なりの定義を追えるようになるのかな？

HelmではSemanticとImenuを一緒にしたインターフェイスを提供してくれます. `semantic-mode`がカレントバッファで有効なら, 自動的にタグ付けしてくれて`imenu`に落とし込んでくれます. カーソルが適当なシンボルの上にあれば, `helm-semantic-or-imenu`のバッファは最初からそのシンボルを指してくれます.

`helm-semantic-or-imenu`はC/C++以外でもだいたい同様に動作してくれます. たとえばJava, Python, Ruby, Emacs Lisp and Lisp in general, shell script, Org-mode等々.

`Semantic`と`Imenu`のあいまい一致を有効にしたいなら以下のようにしましょう:

```emacs-lisp
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
```

**つかいかた**

* **C-c h i**で実行

* **C-p/C-n**でカーソル移動, **C-<down>/C-<up>**でカーソルの指すタグにカレントバッファが移動してくれます.

* カーソルがタグに登録されたシンボルの上, もしくは内部にあれば, `helm-semantic-or-imenu`のバッファは最初からそのシンボルを指してくれます.

Helmでは`beginning-of-defun`(**C-M-a**)と`end-of-defun`(**C-M-e**)を使えますが, `helm-semantic-or-imenu`はこれと同様かそれ以上のの動作をしてくれます. `C-g`でキャンセルしたら, `helm-semantic-or-imenu`を実行する前のカーソル位置に戻してくれます. この挙動は`helm-semantic-or-imenu`のみ対応しており, `imenu`では使えません. 

`helm-semantic-or-imenu`は以下の種類のタグをサポートしています:

* `Dependencies`: C/C++だとヘッダーファイルのincludeなど. 

* `Variables`: カレントバッファ内の変数

* `Functions`: カレントバッファ内の関数

* `Privides`: Emacs-Lispだと`(provide ...)`とか

タグの種類ごとにフィルタリングしたいときは`^`を前置してください. 

![](./images/figure27.png)

## コマンド: `helm-man-woman`
**キーバインド**:

**\<prefix\> m** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 

`helm-man-woman`を使うとマニュアルページに素早く飛べます. ミニバッファにpatternを入力することも, カーソルの指すシンボルをそのまま検索することもできます. 後者を有効にするには以下の設定を追加しましょう:

```emacs-lisp
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
```
![](./images/figure28.png)

## コマンド: `helm-find`

**キーバインド**:

**\<prefix\> /** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 

Unixの`find`コマンドに対応するもの. patternはいつもどおりスペース区切りでいけますが, Unixの`find`を内部で回しているので, ワイルドカード`*`も使えます. 

デフォルトではカレントディレクトリのみ検索しますが, `C-u`を前置すればどのディレクトリ内を検索するかを尋ねてくれます. また, `helm-find`は`helm-find-files`のセッション内で**C-c /**をタイプすることで実行できます. 

`helm-find`は大きなディレクトリ内で検索をかけると動作がとっても遅くなります. そのときは, **C-!**で, 候補を活かしたままサスペンドすることができます. レジュームするにはもう一度**C-!**です. 

![](./images/figure29.png)

## コマンド: `helm-locate`

**キーバインド**:

**\<prefix\> l** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 

`helm-find`に似ていますが, こちらは内部でUnixの`locate`コマンドを回しています. ローカルのデータベースを利用するためには`helm-locate`の前に`C-u`を前置してください. こちらも動きが遅くなることがありますが, `helm-find`同様**C-!**が使えます. 

`helm-locate`であいまい一致を有効にするには以下を追加しましょう:

```emacs-lisp
(setq helm-locate-fuzzy-match t)
```
`locate`はLinuxのコマンドなので, 他のプラットフォームだとちゃんと動かないかもしれません. 

![](./images/figure30.png)

## コマンド: `helm-occur`

**キーバインド**:

**\<prefix\> M-s o** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)ですが, コレだと長いので以下のバインドがいいでしょう:

```emacs-lisp
(global-set-key (kbd "C-c h o") 'helm-occur)
```

**解説**: 

`occur`に似ていますが, Helmのインターフェイスを使っています. カレントバッファ内でpatternと一致する行を列挙してくれます. patternとマッチする行を行き来するのに便利です. **TAB**で`helm-occur`を出ずに, カーソルが指す行に飛んでくれます. **C-g**で`helm-occur`を終了し, 元の位置に戻ります. 

![](./images/figure31.png)

## コマンド `helm-apropos`

**キーバインド**:

**\<prefix\> a** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 

Helmのコマンド・関数・変数などなどEmacsのありとあらゆるオブジェクトのマニュアルを参照するコマンドです. `apropos-command`に似ていますが, こちらはインタラクティブです. `helm-apropos`は以下の5つの子たちを統合しています:

* コマンド

* 関数

* クラス

* ジェネリック関数

* 変数

* Faces

* Helm attributes

あいまい一致も有効にできます:

```emacs-lisp
(setq helm-apropos-fuzzy-match t)
```

## コマンド: `helm-info-*`

**キーバインド**:

**\<prefix\> h** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

|Key              | Binding                      |
|-----------------|------------------------------|
| **\<prefix\> h g**| Command: `helm-info-gnus`    |
| **\<prefix\> h i**| Command: `helm-info-at-point`|
| **\<prefix\> h r**| Command: `helm-info-emacs`   |

**解説**: 

いろいろな種類のヘルプを開くことができます. 上のようなバインドもありますが, `M-x helm info`と入力した後に様々なものをくっつけられます. たとえば`helm-info-as`とか`helm-info-gdb`とか...

## コマンド; `helm-lisp-completion-at-point`

**キーバインド**:

**\<prefix\> \<tab\>** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 

Emacs Lispを書いている時にEmacsで呼び出せる関数を提供してくれます. たとえば適当に何か書いたあとにこれを実行すると, 補完候補を並べてくれます. これもあいまい一致を有効にできます:

```emacs-lisp
(setq helm-lisp-fuzzy-completion t)
```

# あとがき
自分はもう目から鱗状態でした. Helmってすごいんだなあと感心しきりです. 
息切れしてしまったので, とりあえずここまで. 後編に続きます. 
