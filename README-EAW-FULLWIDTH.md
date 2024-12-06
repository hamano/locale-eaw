# 曖昧な文字を全角で統一する修正ロケール(UTF-8-EAW-FULLWIDTH)の利用方法

## libcのロケールの曖昧な文字幅を全角にする

1. [UTF-8-EAW-FULLWIDTH.gz](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/UTF-8-EAW-FULLWIDTH.gz) を /usr/share/i18n/charmaps/ に配置

2. /etc/locale.gen を以下のように変更
~~~
#ja_JP.UTF-8 UTF-8
ja_JP.UTF-8 UTF-8-EAW-FULLWIDTH
~~~

3. `sudo locale-gen` を実行

## rxvt-unicodeで曖昧な文字幅を全角にする

libcのロケールを修正する事で、曖昧な文字幅を統一できます。

## xtermで曖昧な文字幅を全角にする

.Xresources に以下を設定
~~~
xterm*cjkWidth: true
~~~

## emacsで曖昧な文字幅を全角にする

emacs21 と emacs22 と emacs23以降で対応方法が異なります。
様々なemacsのバージョンで動作するelispを用意していますのでこれを使ってください。

1. [eaw-fullwidth.el](https://raw.githubusercontent.com/hamano/locale-eaw/dist/eaw-fullwidth.el) を ~/.emacs.d/site-lisp/ に配置

2. .emacs に以下を設定

~~~
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw-fullwidth)
~~~

## vimで曖昧な文字幅を全角にする

.vimrc に以下を設定

~~~
if exists('&ambw')
    set ambw=double
endif
~~~

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

## tmuxで曖昧な文字幅を全角にする

[fixtmux](./fixtmux) を利用してください。

`UTF-8-EAW-FULLWIDTH`を利用する場合罫線の修正も必要です。

## weztermで曖昧な文字幅を全角にする
<https://wezfurlong.org/wezterm/config/lua/config/treat_east_asian_ambiguous_width_as_wide.html>
