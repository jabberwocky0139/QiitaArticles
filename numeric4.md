<!---
	NumPy・SciPyを用いた数値計算の高速化 : 落ち穂拾い
-->

# 対象
Python及びNumPy初心者に向けて書いています. 「C言語は使えるけど最近Pythonを始めた」とか「Pythonらしい書き方がよくわからない」に該当する物理系の数値計算を目的とした方には特に有用かもしれません.

また, 自分の不勉強のために間違った記述があるかもしれません. ご容赦ください. 

# あらまし
[NumPyを用いた数値計算の高速化 : 基礎](http://qiita.com/jabberwocky0139/items/c3620fb2f011f20a633b)

[NumPy・SciPyを用いた数値計算の高速化 : 応用その1](http://qiita.com/jabberwocky0139/items/a9751d11caa64bc19226)

[NumPy・SciPyを用いた数値計算の高速化 : 応用その2](http://qiita.com/jabberwocky0139/items/26451d7942777d0001f1)

のおまけになります. 細々とした話が中心になります. 

# numpy関数とmath関数

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

# リストによる`ndarray`へのアクセス

以前にさらっと触れたのですが, `ndarray`へのアクセスをリストで行うことができます:

```py3
a = [1, 3, 5]
b = [7, 5, 4, 6, 2, 3, 1, 9, 8]
b[a] # TypeError: list indices must be integers or slices, not list

b_np = np.array(b)
b_np[a]
>>> array([5, 6, 3])
```
知っていると便利な場面もあるでしょう. 

# よりよいBLASへ

`numpy`の基本的な行列演算はBLAS(Basic Linear Algebra Subprograms)に依存しているので, 利用するBLASによって速度が大きく変わります. 例えばUbuntuのシステムに標準で入っている「reference BLAS」は結構遅い実装になっており, `numpy`自体を高速にするにはそれ相応のBLASを使ってあげなければいけません. 

その第一候補として「Open BLAS[^1]」が挙げられます. その導入方法や効果は[こちら](http://verifiedby.me/adiary/058)が詳しいです. 自分も以前はこれにお世話になっていました. 

しかしながら実は, Anaconda[^2]にはMLK(Math Kernel Library)[^3]が標準搭載されており, この上でnumpyを利用する限りは非常に高速です. NumPy・SciPyを使う方はAnacondaを使わない理由はもう無さそうですね.

ただAnacondaをインストールするとシステムの`python`がAnacondaの対応したPythonのバージョンに上書きされてしまい, システム周りの不具合が生じる場合があります. なのでpyenvなどを通じてインストールするのが良いです. AnacondaによるPython環境構築については[こちら](http://qiita.com/y__sama/items/5b62d31cb7e6ed50f02c)が詳しいです.

# Python高速化のサブセットについての感想

Pythonを高速化する枠組みはいくつも存在し, かつそのうちのいくつかを自分は使ってみました. 

## Cython

PythonのC拡張をより使いやすくするための新しい言語と言えばよいでしょうか. 基本文法はPythonなのですが, 実行の際にコードはCにコンパイルされます. ちゃんと高速化するには変数の型を教えてあげたりリストのサイズを教えてあげたりする必要があります. ちゃんと書けば実行速度はホントにCです. 

コードのボトルネックが明らかなとき, その部分を関数化してCythonで書き直すような運用がよいのでしょうか. しかし, 変数に型があったりリストのサイズを固定したりと, 最早Pythonではないです. 書いている最中に「コレ書くんなら最初からCで書けば...」と思ったことも何度かあります. Pythonicなコードにはならないと思います. 

## Boost/Python

PythonからC++のクラスや関数がそのままimportできるBoostのライブラリです. Cythonは高速化したい部分も含めてPythonで記述できるが, ビルドがめんどくさいです. Boost/Pythonは高速化したい部分だけC/C++にお任せすることができます.

しかしやはり多言語とのジョイントは型の問題が辛いですね. C++側に`ndarray`を渡せないようです. `ndarray`を渡せるBoost/NumPyやPyUblasという枠組みもあるのですが, Python3でのビルドが叶わず. 

## Numba

JITコンパイラを使って高速化するモジュール. デコレータ一発で動き, 上２つのようにモジュールが2つ以上にまたがることもない. すごく未来を感じる一方, ノウハウが熟しきっていない気がします. うまく速くなることもあれば失敗することもあり...

## 結論

科学計算ならNumPy・SciPyで十分であることがわかりました. 


# おわりに

今後も何か思いつく度に追加していくかもしれません. 

ありがとうございました. 


[^1]: これはとある後藤さんという方がつくった「Goto BLAS」のフォークです. 当の後藤さんは今はMKLの開発をしているそうで, Goto BLASの開発は止まってしまっているようです. 

[^2]: 科学技術系モジュールをいい感じに詰め込んだパッケージです. 

[^3]: Intelが開発する様々な数学ルーチンに関するライブラリ. BLAS, LAPACKなどの非常に高速な実装を含んでいます. 
