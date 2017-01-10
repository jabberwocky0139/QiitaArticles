<!---
	初心者〜初級者のためのHelm事始め : 後編
-->

# 対象
Emacs及びHelmの初心者〜初級者を対象にしています. これを書いているのは初級者です.

# あらまし
[初心者〜初級者のためのHelm事始め : 前編](http://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c)

の続きです. 
# A Package in a league of its own: `Helm`

スルーしたコマンドがいくつかあります. 別のコマンドで代用可能だと感じたものが主です:

* 正規表現はそんなに喋れることないなあ...
* registerはkill-ringで代用可？
* eshellは重いしなあ...あんまり使わないや

## コマンド: `helm-resume`

**キーバインド**:

**\<prefix\> b** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**: 

これは直前まで開いていたhelmセッションとミニバッファの履歴をレジュームするコマンドです. これは複雑なコマンドを入力するときに便利です. ちょっとコマンドを間違えちゃったりしてもコマンド全部を入力し直す必要はありません. 

## コマンド: `helm-all-mark-rings`

**キーバインド**:

**\<prefix\> C-c SPC** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)ですが, コレだと長いので以下のバインドがいいでしょう:

```emacs-lisp
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
```

**解説**:

`mark-ring`と`global-mark-ring`をセットしたところにジャンプすることができます. もちろんインターフェイスはhelmで. マークは**C-SPC**でつけられます.

## コマンド: `helm-top`

**キーバインド**:

**\<prefix\> t** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**:

UNIXの`top`コマンドをhelmインターフェイスで扱えるようにするものです. 以下のコマンドでプロセスをキルできます:

|Key      | Binding             |
|---------|---------------------|
| **[f1]**| Kill(SIGTERM)       |
| **[f2]**| Kill(SIGKILL)       |
| **[f3]**| Kill(SIGINT)        |
| **[f4]**| Kill(Choose signal) |

さらに以下のような`helm-top`専用のコマンドもあります:

|Key      | Binding             |
|---------|---------------------|
| **C-c C-u**| `helm-top`をリフレッシュ       |
| **M-C**| **コマンド名**でソート       |
| **M-P**| **CPU使用率**でソート        |
| **M-U**| **ユーザー**でソート |
| **M-M**| **メモリ占有率**でソート |

## コマンド: `helm-surfraw`

**キーバインド**:

**\<prefix\> s** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

**解説**:

`surfraw`はコマンドラインからウェブの検索(GoogleとかGoogleとか...)ができるUNIXのコマンドです. `helm-surfraw`はそれをhelmインターフェイスで扱えます. 

> EmacsからのWeb検索というと`eww`ですが, テキストベースなのでちょっと不便です. この`helm-surfraw`を使うとフルブラウザの検索をEmacsからオペレートできます. ただし, Emacsの標準ブラウザがewwになっているのでChrome(FirefoxとかOperaとか...)に変更しましょう:
> ```emacs-lisp
> (setq helm-surfraw-default-browser-function 'browse-url-generic
>      browse-url-generic-program "google-chrome")
> ```

## コマンド: `helm-google-suggest`

**キーバインド**:

**\<prefix\> C-c g** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)ですが, コレだと長いので以下のバインドがいいでしょう:

```emacs-lisp
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
```

**解説**:

入力したパターンについてインタラクティブにGoogleのサジェストを候補に出してくれます. また, そのパターンについてGoogle・Wikipedia・Youtube・Google Map...などで検索をかけることができます. 候補が出てくるまでにすこし時間がかかるかもしれません. 

> 検索をかけるブラウザはEmacsで設定されているブラウザになります. `helm-surfraw`の節で設定をしていればChromeで開くことになります. 

## コマンド: `helm-color`

**キーバインド**:

**\<prefix\> c** (デフォルトでは**C-x c**. この記事の設定では**C-c h**)

カラーマップを`helm-color`で検索できます. 色の名前とそのカラーコードをkill-ringに追加したり, バッファに挿入したりできます. 


# あとがき

今回はかなり端折ってしまいました. Helmのメインコマンドは前半に固まっていたので, 後半は随分とライトな感じで...画像も載せていません. 

後編のMVPは`helm-surfraw`と`helm-google-suggest`でしょうか. 最近リリースされたEmacs25.1ではWebKitがサポートされ, **Emacs上でフルブラウザが動く**という衝撃的な機能が追加されましたが, いかんせん使い勝手はまだまだです. `helm-surfraw`はEmacs内で完結はしないものの, フルブラウザによるWeb検索をEmacs上でオペレートすることができるという点で便利だなーと思いました. 

おそらくこの記事に載せたもの以外にも便利なコマンドがたくさんあることでしょう. 
