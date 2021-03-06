<!---
	Pythonでカオス・フラクタルを見る
-->

# あらまし
カオスとかフラクタルとかって見てるだけで楽しいですよね. 簡単な数式や, 単純な物理から突拍子もないグラフが出てくるのはとても不思議でもあります. その世界を少しだけ覗いてみましょう. 

当方カオス理論が専門ではないので, 深い話には立ち入りません. 眺めて楽しければよいのです. 

## カオスとは？
決定論的であるがしかし, 非常にランダムな振る舞いをする非線形力学系をカオスと呼びます. 一番の特徴が「初期値鋭敏性」というもので, 初期値をほんのちょっと変えただけで全く異なる軌道を描きます.  **二重振り子**などが有名です. 

## フラクタルとは？
自己相似性を持つ図形のこと. フラクタル次元のはなしは面倒なのでスルー. **コッホ曲線**などが有名です. 

## めざすところ
カオスやフラクタルのすごいところは, **単純なモデルから複雑怪奇なものが出現することです.** ということで, シンプルなコードから意味不明な出力を得ることを目指します.

# Mandelbrot集合
いわゆる, フラクタルのはしりです. パラメータ$(x, y)$を用いて定義される漸化式
```math
	a_{n+1} = a_n^2 - b_n^2 + x,\hspace{1cm} b_{n+1} = 2a_nb_n + y,\hspace{1cm} a_0 = b_0 = 0
```


1. `numpy`のユニバーサル関数は`ndarray`を引数にとって`ndarray`を返します. `math`関数は`int`か`float`しか取ることができません. 

2. `math`関数は`complex`も取れません. `complex`を渡したい場合は`cmath`を使います. `numpy`は`complex`も取ることができます. 

3. `numpy`の関数は定義域から外れた, もしくは際どい値を与えるとRuntimeWarningを返すものの, Exceptionを返すことはありません. `math`関数は例外を返してきます:

```py3
import numpy as np
import math as m

m.log(0) # ValueError: math domain error
mp.log(0) # RuntimeWarning: divide by zero encountered in log
>>> -inf

mp.log(-1) # RuntimeWarning: invalid value encountered in log
>>> nan
```
`np.log(0)`が`-inf`を返してくれるのは嬉しいですね. 

4. このように`numpy`のユニバーサル関数は万能ですが, 実行速度では`math`の方が上のようです. `ndarray`を渡す用途でなければ`math`関数を使うと良いでしょう.



[^1]: これはとある後藤さんという方がつくった「Goto BLAS」のフォークです. 当の後藤さんは今はMKLの開発をしているそうで, Goto BLASの開発は止まってしまっているようです. 

[^2]: 科学技術系モジュールをいい感じに詰め込んだパッケージです. 

[^3]: Intelが開発する様々な数学ルーチンに関するライブラリ. BLAS, LAPACKなどの非常に高速な実装を含んでいます. 
