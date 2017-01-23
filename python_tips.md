<!---
	Python3 ｡oO(標準ライブラリ使ってる？)
-->
# 日陰者の関数たち

Pythonはバッテリー同梱と言われるだけあってbuilt-in関数やら標準ライブラリやらがとっても充実しています. 一方で充実しすぎて日の目を見ない関数たちも多いように思います.

これを書いたそもそもの動機はprint関数です. もっと評価されるべき.

# print
この子自体は超有名ですが, その引数たちは日陰者かも？. 

Python2ではprint文だったのが, Python3からprint関数になりました. 下位互換性をぶった切る以外にどんな意味があるのかと思っていましたが, よくよく見るとすごい便利な関数に進化していました:

```py3
print(value, ..., sep=' ', end='\n', file=sys.stdout, flush=False)
```

**- sep -**

区切り文字を設定できます. `join`と同じようなことができます:

```py3
print('a', 'b', 'c', 'd', sep='_')
# '_'.join(['a', 'b', 'c', 'd'])
>>> a_b_c_d
```

**- end -**

出力の最後につけるものを指定できます. デフォルトはもちろん`\n`:

```py3
print('Hello ', end=''); print('World!')
>>> Hello World!
```

**- file -**

これが最も感動したポイント. **ファイル出力が`print`で可能です**:

```py3
f = open('output.txt', 'w')
print('Hello World!', file=f)
```
ググると`write()`だったり`writeline()`を使う方法がよく紹介されていますが, これからは`print`でいけます.

**- flush -**

バッファのフラッシュが可能です. 例えばファイル書き込みをするのは`print(..., file=f)`を実行したタイミングではなく, バッファが満杯になったタイミングです. おそらくパフォーマンスのためなのですが, 実行中に書き込みファイルを覗きたい場合はとっとと書き込みをしてほしいわけです. そんなときは`flush`をTrueにします. 

**- 合わせ技 -**

たとえばなんかしらの処理をするときにループの回数をコンソールに出力したいとします:

```py3
for i in range(100):
	print('rep = {0}'.format{i})
	# 以下なんかしらの処理...

>>> rep = 0
	rep = 1
	rep = 2
	rep = 3
	...
```
でもこれだと出力が縦にずらずらと並んでコンソールの領域がもったいないです. **出力を上書きするようにしましょう:**

```py3
for i in range(100):
	print('rep = {0}'.format{i}, '\r', end='', flush=True)
	# 以下なんかしらの処理...
```
`\r`はカーソル位置を行頭に戻すエスケープシーケンスです. これで出力が上書きされて1行に収まります. 便利ですよ. 

# pprint
**pretty-print, 略して`pprint`**. その名の通り出力をかわいくしてくれます. おそらくここで言う「かわいい」の定義は「コンソールの幅に合わない出力が複数行に分けて出力されている状態」のことだと思われます:

```py3
from pprint import pprint

pprint('There should be one-- and preferably only one --obvious way to do it.'.split())

>>> ['There',
	'should',
	'be',
	'one--',
	'and',
	'preferably',
	'only',
	'one',
	'--obvious',
	'way',
	'to',
	'do',
	'it.']
```
かわいいかどうかはわかりませんが, 便利なこともあるでしょう. なお, `print`関数のような万能感はありません.

# shelve
数値計算結果をtxt, csvファイルなどに保存するのはよくあることですが, これをまたPythonで読み込もうとするとstring型になるのでそれを`int`・`float`にキャスト...という手順を踏まなければなりません. **`shelve`はPython内のオブジェクトをそのまま保存・展開できます:**

```py3
import shelve

zen_string = 'There should be one-- and preferably only one --obvious way to do it.'
zen_list = zen_string.split()

# 新しいShelveオブジェクトを作る
obj = shelve.open('./test')
# dict型として代入
obj['string'], obj['list'] = zen_string, zen_list
# 保存(close)
obj.close()

# 展開
obj_restore = shelve.open('./test')
print(list(obj_restore.keys()))
>>> ['string', 'list']

print(obj_restore['string'])
>>> 'There should be one-- and preferably only one --obvious way to do it.'
```
Pythonに住むならこれは便利なはず！ ただ他の言語やツールからはアクセスできないので汎用性は？

# collections.deque
listは便利なコンテナですが, 末尾以外の要素の追加・削除はとても効率が悪いです. `collections.deque`は両端のappend, popが高速に実行可能です:

```py3
import numpy as np
import deque
a = np.linspace(0, 1, 100000)
b = deque(a)

# iPython
%timeit -n5 a.pop(0)
>>> 5 loops, best of 3: 35.6 µs per loop

%timeit -n5 b.popleft()
5 loops, best of 3: 147 ns per loop
```
<font color="HotPink">**200倍以上**</font>はやいです. 仮にプロコンでPythonを使うなら必須かと思います.

# おまけ : IPython
IPythonの便利さについてはいろいろなところで語られております. 

[IPythonの使い方](http://qiita.com/5t111111/items/7852e13ace6de288042f)

コマンドのうしろに`?`をつけるとヘルプを参照できたり, シェルのコマンドがそのまま使えたり, マジックコマンドとか便利です.

## IPython.embed
コードを実行している途中でに一旦実行を止めてIPythonに入ります. 

```py3
from IPython import embed

tmp = []
for i in range(100):
    tmp.append(i)
    if i == 50:
		# i == 50でIPythonで入る
        embed()

# IPython
In [1]: tmp
Out[1]:
[0,
 1,
 2,
 ...
 50]
# exit()とかCtrl-DとかでIPythonを出ると実行再開
```
`embed()`で止めて, それまでに生成されたオブジェクトにアクセスすることができます. printデバッグは卒業したいけどデバッガは敷居が高い...という方に是非.

# さいごに
自分がよく使うものであんまり知られて無さそうなものを挙げてみました. いいものあったら足していきます. いいものあったら教えてください.
