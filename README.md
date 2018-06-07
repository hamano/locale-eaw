# East Asian Ambiguous Widthと絵文字の為の修正ロケール

## East Asian Ambiguous Width 問題とは

East Asian Ambiguous 文字とは、Unicodeで文字幅が曖昧と定義されている文字のことで、例えば、○(U+25CB)や×(U+00D7)や△(U+25B3)などの文字です。

East Asian Ambiguous 文字の一覧は[こちら](https://raw.githubusercontent.com/hamano/locale-eaw/master/test.txt)。

East Asian Ambiguous Width 問題とはこれらの文字をコンソールで表示する際に、libcのlocale、ターミナル、エディタなどがそれぞれ異なる文字幅(半角、全角)で文字を扱う為に表示がズレてしまう問題です。

この問題のてっとり早い解決方法のひとつは曖昧な文字の文字幅を全角で統一する事です。

対応方法はアプリケーションによって様々ですが、xterm, mlterm, vimなどを使っている人は簡単に解決できます。

しかし、rxvt-unicodeやemacsを使っている人は残念でした、これらのアプリケーションは曖昧な文字幅を統一するオプションが用意されていません。

このレポジトリでは主にrxvt-unicodeとemacs向けに曖昧な文字幅問題を解決するファイルをメンテしています。

## 絵文字について
絵文字はNarrowと定義されているにも関わらず、多くのフォントでは全角で描かれているという問題があります。
たとえば🀀(U+1F000)、🕿(U+1F57F)。
この問題に対処するため、U+1F000-U+1FFFFの範囲のNarrow文字を全角にするという対応を行いました。
この範囲以外の絵文字についてはよく分からないのでそのままにしてあります。

## libcのロケールの曖昧な文字幅を全角にする

[こちら](http://vdr.jp/d/20070322.html)で配布されている UTF-8-EAW-FULLWIDTH.gz がちょっと古くなっていたので同じ方法で生成してメンテ出来るようにしました。

1. [UTF-8-EAW-FULLWIDTH.gz](https://raw.githubusercontent.com/hamano/locale-eaw/master/UTF-8-EAW-FULLWIDTH.gz) を /usr/share/i18n/charmaps/ に配置

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
unicode_full_width_areas = U+1F000-1FFFF
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

libcのロケールを修正する事で、曖昧な文字幅を統一できます。

## 最近のtmux

`setlocale(LC_CTYPE, "en_US.UTF-8")` がハードコードされているので以下のように修正する。

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


