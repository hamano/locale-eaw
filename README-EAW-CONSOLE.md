# UTF-8EAW-CONSOLE 修正ロケール

East Asian Ambiguous文字を一律に全角とすると、様々な問題が起こります。
このような問題を避けるため、曖昧な文字幅の文字を基本的に半角とし、
欧文であまり利用されず、全角で表示することが自然な文字のみを全角とすることで、
TUIアプリケーションができるだけ壊れないように配慮したロケールです。

具体的には、

罫線、ブロック要素、ラテン文字、キリル文字などを半角とし、

※、♨、①、ローマ数字、絵文字、nerdfont文字などを全角として扱います。

## libcのロケールの設定

1. [UTF-8-EAW-CONSOLE.gz](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/UTF-8-EAW-FULLWIDTH.gz) を /usr/share/i18n/charmaps/ に配置

2. /etc/locale.gen を以下のように変更
~~~
#ja_JP.UTF-8 UTF-8
ja_JP.UTF-8 UTF-8-EAW-CONSOLE
~~~

3. `sudo locale-gen` を実行

## rxvt-unicodeの設定

libcのロケールを修正する事で、曖昧な文字幅が良い感じになります

## xtermで曖昧な文字幅を設定する

.Xresources に以下を設定

~~~
XTerm*mkSampleSize: 0
XTerm*cjkWidth: false
~~~

## emacsの設定

emacs21 と emacs22 と emacs23以降で対応方法が異なります。
様々なemacsのバージョンで動作するelispを用意していますのでこれを使ってください。

1. [eaw.el](https://raw.githubusercontent.com/hamano/locale-eaw/master/eaw-console.el) を ~/.emacs.d/site-lisp/ に配置

2. .emacs に以下を設定する

~~~
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw-console)
~~~

## vimの設定

1. [eaw-console.vim](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/eaw-console.vim) を`~/.vim/`に配置

2. .vimrc に以下を設定
~~~
source ~/.vim/eaw-console.vim
~~~

## mltermの設定

.mlterm/main に設定

~~~
unicode_full_width_areas = ...
~~~

# 裁定

## ※
- 和文では全角として扱われてきた歴史がある
- 欧文であまり使われていないような印象

全角とする

## 丸数字
- 和文フォントでは全角が一般的
- ⑳を半角で表示するのは窮屈
- ⑳まではambiguousだが㉑からWideとなっている。⑳を半角にすると一貫性を保てない

全角とする

## ローマ数字
- 和文では全角として扱われてきた歴史がある
- 欧文ではi,v,xの組み合わせで表現されることがありあまり利用されていないような印象
- ⅷを半角で表示するのは窮屈

全角とする

# 絵文字

絵文字はNeutralと定義されつつも、多くのフォントでは全角で描かれているという現状があります。
たとえば以下の範囲の文字。

- U+2600 - U+27FF (☀☁☂☃)
- U+1F000 - U+1FFFF (🀀🌶 🐿 🕿)

半角で描画するのが難しい文字はNeutralであっても全角とする。

