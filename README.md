# East Asian Ambiguous Width問題のための修正ロケール

## East Asian Ambiguous Width問題とは

East Asian Ambiguous文字とは、Unicodeで文字幅が曖昧(文脈により異なる文字幅)と定義されている文字のことで、例えば、※(U+203B)や○(U+25CB)や×(U+00D7)や△(U+25B3)などの文字です。

East Asian Ambiguous 文字の一覧は[こちら](https://raw.githubusercontent.com/hamano/locale-eaw/master/test/amb.txt)。

East Asian Ambiguous Width問題とは、これらの文字をコンソールで表示する際libcの`wcwidth(3)`、シェル、ターミナルエミュレータ、テキストエディタなどのプログラムがそれぞれ異なる文字幅で認識する問題です。

例えばテキストエディタのカーソルの表示位置と内部状態が食い違ってしまうため、表示が壊れてしまいます。

また、絵文字やNerdFontの文字も曖昧となっているため、ターミナルで絵文字を利用する際にも問題となります。

## 解決方法その1: 曖昧な文字を全角で統一する EAW-FULLWIDTH

この問題の古典的な解決方法は曖昧な文字幅を全角で統一する事です。

対応方法はアプリケーションによって様々ですが、xterm, mlterm, vimなどを使っている人は設定やオプションを指定するだけで文字幅を変更できます。

rxvt-unicodeのようなアプリはlibc localeを修正することで文字幅を変更できます。

各アプリケーションの設定方法は[README-EAW-FULLWIDTH.md](README-EAW-FULLWIDTH.md)を参照してください。

## 解決方法その2: 文字幅を個別に調整する EAW-CONSOLE

曖昧な文字を全角に変更すると以下のような問題が起こります。

- 罫線が全角となってしまう。
  - ncursesやtmuxなどのTUI画面が壊れる
- ブロック要素が全角となってしまう。
  - プログレスバー表示などが壊れる
- latin1のSOFT HYPHENなども全角となってしまう
- 半角表示で十分なギリシャ文字、キリル文字が全角となってしまう。
- ①が全角なのに⓪が半角になる
- ローマ数字のⅰ〜ⅹが全角なのにⅺとⅻが半角となる
- 全角表示が自然な絵文字がNeutralとして定義されていて半角になる

より良い解決方法は文字幅を個別に調整し、利用しているすべてのアプリケーションで文字幅を統一することです。
これが可能なアプリケーションは、
- libcの`wcwidth(3)`を利用するアプリケーション
  - xterm
  - rxvt-unicode
  - bash, zshなどのシェル
- mlterm
- emacs
- vim
- neovim
- wezterm

などがあります。
すべてのアプリケーションで文字幅を統一するのは大変ですが、このレポジトリではこれを実現するためのツールとおすすめのロケール`UTF-8-EAW-CONSOLE`を提供します。

各アプリケーションの設定方法は[README-EAW-CONSOLE.md](README-EAW-CONSOLE.md)を参照してください。

## もっとカスタマイズ

`UTF-8-EAW-FULLWIDTH`や`UTF-8-EAW-CONSOLE`などのロケールのフレーバーは[config.ini](config.ini)に定義しておりこれらが気に入らない場合、以下のように追記して`./gen.py`を実行します。

```
[EAW-CUSTOM]
# 曖昧な文字幅を全角にする
amb = 2
...
```

# フォント

この修正ロケールに最適化した合成フォントをつくりました!
[font-eaw](https://github.com/hamano/font-eaw)

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

## ℡がAmbiguousで℻がNeutral
- [℡] U+2121 TELEPHONE SIGN
- [℻] U+213B FACSIMILE SIGN

## Neutralで半角描画が困難な文字たち
- [‱] U+2031 PER TEN THOUSAND SIGN: N (neutral)
- [⅏] U+214F SYMBOL FOR SAMARITAN SOURCE
- [☃] U+2603 SNOWMAN

## ⓪と⑳と㉑
- [⓪] U+24EA CIRCLED DIGIT ZERO: N (neutral)
- [⑳] U+2473 CIRCLED NUMBER TWENTY: A (ambiguous)
- [㉑] U+3251 CIRCLED NUMBER TWENTY ONE: W (wide)

## ㉈
- [㉈] U+3248 CIRCLED NUMBER TEN ON BLACK SQUARE: A (ambiguous)
この文字はambiguousなのにglibc標準のUTF-8ロケールで全角となっている。ゆえに壊れやすい。

https://sourceware.org/bugzilla/show_bug.cgi?id=24658

## 麻雀牌
- [🀀] MAHJONG TILE EAST WIND: N (neutral)
- [🀄] MAHJONG TILE RED DRAGON: W (wide)

## トランプのジョーカーだけWide
- [🃏] PLAYING CARD BLACK JOKER: W (wide)
- [🂡] PLAYING CARD ACE OF SPADES: N (neutral)

# 参考文献
- [UAX#11: East Asian Width](https://www.unicode.org/reports/tr11/)
- [UTS#51: UNICODE EMOJI](https://www.unicode.org/reports/tr51/)
- https://gitlab.freedesktop.org/terminal-wg/specifications/-/issues/9
