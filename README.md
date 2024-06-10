# East Asian Ambiguous Width問題を解決するための修正ロケール

## East Asian Ambiguous Width問題とは

East Asian Ambiguous文字とは、Unicodeで文字幅が曖昧(文脈により異なる文字幅で表示する)と定義されている文字のことで、例えば、○(U+25CB)や×(U+00D7)や△(U+25B3)などの文字です。

East Asian Ambiguous 文字の一覧は[こちら](https://raw.githubusercontent.com/hamano/locale-eaw/master/test/eaw.txt)。

East Asian Ambiguous Width 問題とは、これらの文字をコンソールで表示する際libcの`wcwidth(3)`、シェル、ターミナルエミュレータ、テキストエディタなどのTUIアプリがそれぞれ異なる文字幅で認識する問題です。

例えばテキストエディタのカーソルの表示位置と内部状態が食い違ってしまうため、表示が壊れてしまいます。

## 解決方法その1: 曖昧な文字を全角で統一する

この問題のてっとり早い解決方法は曖昧な文字の文字幅を全角で統一する事です。

対応方法はアプリケーションによって様々ですが、xterm, mlterm, vimなどを使っている人は設定やオプションを指定するだけで文字幅を変更することができます。

また、libc localeを修正することでrxvt-unicodeのようなアプリの文字幅を変更できます。

詳細は[README-EAW-FULLWIDTH.md](README-EAW-FULLWIDTH.md)を参照してください。

## 解決方法その2: 文字毎に最適な文字幅を設定する

曖昧な文字幅を全角や半角で統一しても快適なターミナル生活を送ることはできません。

絵文字はNeutralと定義されつつも、多くのフォントでは全角で描かれているという問題があります。
たとえば以下の範囲の文字。

- U+2600 - U+27FF (☀☁☂☃)
- U+1F000 - U+1FFFF (🀀🌶🐿🕿)

また、曖昧な文字を全角に統一すると以下の問題があります。
- 罫線が全角となってしまう。(TUIアプリは罫線を半角と期待していることが多い)
- ブロック要素が全角となってしまう。(TUIアプリは半角を期待していることが多い)
- 半角表示で十分なギリシャ文字、キリル文字が全角となってしまう。
- U+2010(‐)ハイフンが全角となってしまう

より良い解決方法は各文字毎に好みの文字幅を定義し、利用しているアプリケーションで文字幅を統一することです。
これが可能なアプリケーションは、
- libcの`wcwidth(3)`を利用するアプリケーション
  - rxvt-unicode
  - bash, zshなどのシェル
- mlterm
- emacs
- 最近のvim

などです。
各文字毎に文字幅を設定するのは大変ですが、このレポジトリではこれを実現するために役立つツールとおすすめのロケール`UTF-8-EAW-CONSOLE`を提供します。

このロケールは曖昧な文字を全角にしつつも罫線やブロック要素は半角にしたい、というような人におすすめです。

`UTF-8-EAW-FULLWIDTH`や`UTF-8-EAW-CONSOLE`というロケールのフレーバーは[config.ini](config.ini)に定義しておりこれらが気に入らない場合、以下のようにしてカスタマイズできます。

```
[EAW-CUSTOM]
eaw = 2
...
```

# どうしてこうなったシリーズ

## 大文字と小文字の違いのはずなのに
- [Ō] U+014C LATIN CAPITAL LETTER O WITH MACRON: N (neutral)
- [ō] U+014D LATIN SMALL LETTER O WITH MACRON: A (ambiguous)

## ₀だけNeutral
- [₀] U+2080 SUBSCRIPT ZERO: N (neutral)
- [₁] U+2081 SUBSCRIPT ONE - U+2084 SUBSCRIPT FOUR: A (ambiguous)

## ♦だけNeutral(カードゲームが出来ない…)
- [♠] U+2660 BLACK SPADE SUIT: A (ambiguous)
- [♥] U+2665 BLACK HEART SUIT: A (ambiguous)
- [♣] U+2663 BLACK CLUB SUIT: A (ambiguous)
- [♦] U+2666 BLACK DIAMOND SUIT: N (neutral)

## 小文字ローマ数字の11と12だけNeutral
- [ⅺ] U+217A SMALL ROMAN NUMERAL ELEVEN: N (neutral)
- [ⅻ] U+217B SMALL ROMAN NUMERAL TWELVE: N (neutral)

