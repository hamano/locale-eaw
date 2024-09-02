# East Asian Ambiguous Width問題を解決するための修正ロケール

## East Asian Ambiguous Width問題とは

East Asian Ambiguous文字とは、Unicodeで文字幅が曖昧(文脈により異なる文字幅で表示する)と定義されている文字のことで、例えば、○(U+25CB)や×(U+00D7)や△(U+25B3)※(U+203B)などの文字です。

East Asian Ambiguous 文字の一覧は[こちら](https://raw.githubusercontent.com/hamano/locale-eaw/master/test/amb.txt)。

East Asian Ambiguous Width 問題とは、これらの文字をコンソールで表示する際libcの`wcwidth(3)`、シェル、ターミナルエミュレータ、テキストエディタなどのTUIアプリがそれぞれ異なる文字幅で認識する問題です。

例えばテキストエディタのカーソルの表示位置と内部状態が食い違ってしまうため、表示が壊れてしまいます。

## 解決方法その1: 曖昧な文字を全角で統一する EAW-FULLWIDTH

この問題のてっとり早い解決方法は曖昧な文字の文字幅を全角で統一する事です。

対応方法はアプリケーションによって様々ですが、xterm, mlterm, vimなどを使っている人は設定やオプションを指定するだけで文字幅を変更することができます。

また、libc localeを修正することでrxvt-unicodeのようなアプリの文字幅を変更できます。

詳細は[README-EAW-FULLWIDTH.md](README-EAW-FULLWIDTH.md)を参照してください。

## 解決方法その2: 文字毎に最適な文字幅を設定する EAW-CONSOLE

曖昧な文字を全角に統一しても以下のような問題があります。

- 罫線が全角となってしまう。
  - ncursesやtmuxなどのTUI表示が壊れる
- ブロック要素が全角となってしまう。
  - プログレスバー表示などが壊れる
- 半角表示で十分なギリシャ文字、キリル文字が全角となってしまう。
- ①が全角なのに⓪が半角になる
- ローマ数字のⅰ〜ⅹが全角なのにⅺとⅻが半角となる

より良い解決方法は各文字毎に好みの文字幅を定義し、利用しているアプリケーションで文字幅を統一することです。
これが可能なアプリケーションは、
- libcの`wcwidth(3)`を利用するアプリケーション
  - xterm
  - rxvt-unicode
  - bash, zshなどのシェル
- mlterm
- emacs
- 最近のvim

などがあります。
各文字毎に文字幅を設定するのは大変ですが、このレポジトリではこれを実現するために役立つツールとおすすめのロケール`UTF-8-EAW-CONSOLE`を提供します。

このロケールは曖昧な文字を半角にしつつ絵文字は全角にしたい、というような人におすすめです。

## さらにカスタマイズ

`UTF-8-EAW-FULLWIDTH`や`UTF-8-EAW-CONSOLE`などのロケールのフレーバーは[config.ini](config.ini)に定義しておりこれらが気に入らない場合、以下のように追記して`./gen.py`を実行します。

```
[EAW-CUSTOM]
eaw = 1
...
```

# 絵文字について
絵文字はNeutralと定義されつつも、多くのフォントでは全角で描かれているという問題があります。
たとえば以下の範囲の文字。

- U+2600 - U+27FF (☀☁☂☃)
- U+1F000 - U+1FFFF (🀀🌶🐿🕿)

# どうしてこうなったシリーズ

## 大文字と小文字の違いのはずなのに
- [Ō] U+014C LATIN CAPITAL LETTER O WITH MACRON: N (neutral)
- [ō] U+014D LATIN SMALL LETTER O WITH MACRON: A (ambiguous)

## ₀だけNeutral
- [₀] U+2080 SUBSCRIPT ZERO: N (neutral)
- [₁] U+2081 SUBSCRIPT ONE - U+2084 SUBSCRIPT FOUR: A (ambiguous)

## ♦だけNeutral
- [♠] U+2660 BLACK SPADE SUIT: A (ambiguous)
- [♥] U+2665 BLACK HEART SUIT: A (ambiguous)
- [♣] U+2663 BLACK CLUB SUIT: A (ambiguous)
- [♦] U+2666 BLACK DIAMOND SUIT: N (neutral)

## 小文字ローマ数字の11と12だけNeutral
- [ⅰ] U+2170 SMALL ROMAN NUMERAL ONE: A (ambiguous)
- [ⅺ] U+217A SMALL ROMAN NUMERAL ELEVEN: N (neutral)
- [ⅻ] U+217B SMALL ROMAN NUMERAL TWELVE: N (neutral)

## Neutralだが半角描画が厳しい文字たち
- [‱] U+2031 PER TEN THOUSAND SIGN: N (neutral)
- [℻] U+213B FACSIMILE SIGN
- [⅏] U+214F SYMBOL FOR SAMARITAN SOURCE

## ⑳と㉑
- [⑳] U+2473 CIRCLED NUMBER TWENTY: A (ambiguous)
- [㉑] U+3251 CIRCLED NUMBER TWENTY ONE: W (wide)

## ㉈
- [㉈] U+3248 CIRCLED NUMBER TEN ON BLACK SQUARE: A (ambiguous)
この文字はambiguousなのにデフォルトのglibcロケールで全角となっている。ゆえに壊れやすい。

https://sourceware.org/bugzilla/show_bug.cgi?id=24658

# 参考文献
- https://gitlab.freedesktop.org/terminal-wg/specifications/-/issues/9
