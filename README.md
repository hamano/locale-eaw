# East Asian Ambiguous Widthと絵文字の為の修正ロケール

## East Asian Ambiguous Width問題とは

East Asian Ambiguous文字とは、Unicodeで文字幅が曖昧(文脈により異なる文字幅で表示する)と定義されている文字のことで、例えば、○(U+25CB)や×(U+00D7)や△(U+25B3)などの文字です。

East Asian Ambiguous 文字の一覧は[こちら](https://raw.githubusercontent.com/hamano/locale-eaw/master/test/eaw.txt)。

East Asian Ambiguous Width 問題とは、これらの文字をコンソールで表示する際libcの`wcwidth(3)`、シェル、ターミナルエミュレータ、テキストエディタなどのTUIアプリがそれぞれ異なる文字幅で認識する問題です。

例えばテキストエディタのカーソルの表示位置と内部状態が食い違ってしまうため、表示が壊れてしまいます。

## 解決方法その1

この問題のてっとり早い解決方法のひとつは曖昧な文字の文字幅を全角で統一する事です。

対応方法はアプリケーションによって様々ですが、xterm, mlterm, vimなどを使っている人は設定やオプションを指定するだけで文字幅を変更することができます。

## 解決方法その2

曖昧な文字幅を全角や半角で統一しても快適なターミナル生活を送ることはできません。

絵文字はNeutralと定義されつつも、多くのフォントでは全角で描かれているという問題があります。
たとえば以下の範囲の文字。

- U+2600 - U+27FF (☀☁☂☃)
- U+1F000 - U+1FFFF (🀀🌶🐿🕿)

また、曖昧な文字を全角に統一すると以下の問題があります。
- 罫線が全角となってしまう。(TUIアプリは罫線を半角と期待していることが多い)
- 半角表示で十分なギリシャ文字、キリル文字が全角となってしまう。
- U+2010(‐)ハイフンが全角となってしまう
- プログレスバーなどで使われるブロック要素が全角となってしまう。(TUIアプリは半角を期待していることが多い)

より良い解決方法は各文字毎に好みの文字幅を定義し、利用しているアプリケーションで文字幅を統一することです。
これが可能なアプリケーションは、
- libcの`wcwidth(3)`を利用するアプリケーション
  - rxvt-unicode
- emacs
- 最近のvim
- mlterm

などです。
各文字毎に文字幅を設定するのは大変ですが、このレポジトリではこれを実現するために役立つツールを提供します。

## libcのロケールの曖昧な文字幅を全角にする

[こちら](http://vdr.jp/d/20070322.html)で配布されている UTF-8-EAW-FULLWIDTH.gz がちょっと古くなっていたので同じ方法で生成してメンテ出来るようにしました。

1. [UTF-8-EAW-FULLWIDTH.gz](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/UTF-8-EAW-FULLWIDTH.gz) を /usr/share/i18n/charmaps/ に配置

2. /etc/locale.gen を以下のように変更
~~~
#ja_JP.UTF-8 UTF-8
ja_JP.UTF-8 UTF-8-EAW-FULLWIDTH
~~~

3. locale-gen を実行

## rxvt-unicodeで曖昧な文字幅を全角にする

libcのロケールを修正する事で、曖昧な文字幅を統一できます。

## emacsで曖昧な文字幅を全角にする

emacs21 と emacs22 と emacs23以降で対応方法が異なります。
様々なemacsのバージョンで動作するelispを用意していますのでこれを使ってください。

1. [eaw.el](https://raw.githubusercontent.com/hamano/locale-eaw/master/eaw.el) を ~/.emacs.d/site-lisp/ に配置

2. .emacs に以下を設定する

~~~
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw)
(eaw-fullwidth)
~~~

## vimで曖昧な文字幅を全角にする

.vimrc に、
~~~
if exists('&ambw')
    set ambw=double
endif
~~~
と設定する。

## xtermで曖昧な文字幅を全角にする

.Xresources などに、
~~~
xterm*utf8: 1
xterm*locale: true
xterm*cjkWidth: true
~~~
と設定する。

## mltermで曖昧な文字幅を全角にする

実行オプションに `-a 2` を付けるか、
.mlterm/main に、
~~~
col_size_of_width_a = 2
~~~
と設定する。

絵文字も全角にするには

~~~
unicode_full_width_areas = U+2600-27FF,U+1F000-1FFFF
~~~

## GNU screenで曖昧な文字幅を全角にする
~/.screenrc に
~~~
cjkwidth on
~~~
と設定する。

## w3mで曖昧な文字幅を全角にする

~/.w3m/config に
~~~
east_asian_width 1
~~~
と設定する。

## tmux 2.2以降

* libcのロケールを修正 -> tmuxのウィンドウ内の表示はA=2に統一できます
* ウィンドウ分割時の罫線は改修が必要です

## tmux 2.7以降

libcのロケールをA=2に修正した上で、以下の修正が必要でした

~~~
diff --git a/tmux.c b/tmux.c
index 5b73079..0377ddd 100644
--- a/tmux.c
+++ b/tmux.c
@@ -202,6 +202,7 @@ main(int argc, char **argv)
                        errx(1, "need UTF-8 locale (LC_CTYPE) but have %s", s);
        }

+       setlocale(LC_CTYPE, "");
        setlocale(LC_TIME, "");
        tzset();
~~~


ターミナルがACSをサポートしている場合、罫線の描画にACSを使うよう設定します。

~/.tmux.conf:

~~~
set -ag terminal-overrides ',*:U8=0'
~~~

* [tmuxの罫線素片をACSに強制する](https://qiita.com/yanma/items/2644e6db6f3bcf249690)

## weztermで曖昧な文字幅を全角にする
<https://wezfurlong.org/wezterm/config/lua/config/treat_east_asian_ambiguous_width_as_wide.html>

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

