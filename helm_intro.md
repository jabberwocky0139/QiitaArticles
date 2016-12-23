<!---
	Helm事始め(helm-intro 和訳)
-->

# あらまし
訳は「ノリ >> 正確性」です. 原文と異なる表現にしているところもたくさんあります. 

# A Package in a league of its own: `helm`

<u>作者さんたち:</u>

* Tamas Patrovic (-2007). 最初の開発者さん. このときは`Anything`という名前でした. 

* るびきち (2008-2011). このときもまだ`Anything`.

* Thierry Volpiatto (2011-). 現在のメンテナさん. このときに`Anything`は`Helm`に生まれ変わりました.

<u>ホームページ:</u> [GitHub](https://github.com/emacs-helm/helm)

<u>`Helm`の特徴</u>

<font color="HotPink"> `Helm`はEmacsでインクリメンタルに補完や検索をするためのフレームワークです. </font>Emacsで何か(Bufferやファイルなど)を探しているときに, 役に立ってくれます.

Helmは`anything.el`(Tamas Patrovicさん作)のフォークで, 言わば`anything.el`のご子息です. `anything.el`のレガシーコードをお掃除することに始まり, <font color="HotPink">いろんなツールを用意してくれています.</font> しかもそれらは下位互換に縛られるようなことはありません.

<u>インストール:</u>

[Emacs Prelude](https://github.com/bbatsov/prelude)か[Spacemacs](https://github.com/syl20bnr/spacemacs)では最初からセットアップされており, このガイドにあるような設定は必要ないです. でも[Emacs Prelude](https://github.com/bbatsov/prelude)ではHelmがデフォルトで無効になってます. [ここ](https://github.com/bbatsov/prelude#helm)を見て有効にしてください. [Spacemacs](https://github.com/syl20bnr/spacemacs)はデフォルトで有効です. 

もしSpacemacsユーザーなら, もうなんにもしなくていいです. ふつうのEmacsユーザーなら`M-x list-packages`で**helm**を選んでインストールしましょう. インストールが終わったら以下の設定を追加してhelmを有効にしましょう. 

最小設定:

```elisp
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
