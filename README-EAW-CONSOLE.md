# 修正ロケール(UTF-8-EAW-CONSOLE)の利用方法

East Asian Ambiguous文字を一律に全角とすると、様々な問題が起こります。
これをを避けるため、曖昧な文字幅の文字を基本的に半角とし、
欧文であまり利用されず、半角で表示することが困難な文字のみを全角とすることで、
できるだけTUIアプリケーションが壊れないよう調整したロケールです。

例を挙げると、

罫線、ブロック要素、ラテン文字、キリル文字などを半角とし、

※、♨、①、絵文字、nerdfontのプライベート領域などを全角としてます。

詳細は[文字幅の裁定](#文字幅の裁定)を参照

## libcのロケールの設定

1. [UTF-8-EAW-CONSOLE.gz](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/UTF-8-EAW-CONSOLE.gz) を /usr/share/i18n/charmaps/ に配置

2. /etc/locale.gen を以下のように変更
~~~
#ja_JP.UTF-8 UTF-8
ja_JP.UTF-8 UTF-8-EAW-CONSOLE
~~~

3. `sudo locale-gen` を実行

## rxvt-unicodeの設定

rxvt-unicodeは標準でglibcのロケールを参照するため、設定は不要です。

## xtermの設定

xtermは文字幅に関して独自のテーブルを持っていますが、
.Xresources に以下を設定するとglibcロケールの文字幅を参照します。

~~~
XTerm*mkSampleSize: 0
XTerm*cjkWidth: false
~~~

## emacsの設定

emacs21 と emacs22 と emacs23以降で対応方法が異なります。
様々なemacsのバージョンで動作するelispを用意していますのでこれを使ってください。

1. [eaw-console.el](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/eaw-console.el) を ~/.emacs.d/site-lisp/ に配置

2. .emacs に以下を設定する

~~~
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'eaw-console)
~~~

## vimの設定

1. [eaw-console.vim](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/eaw-console.vim) を`~/.vim/`に配置

```
mkdir -p ~/.vim/
curl -o ~/.vim/eaw-console.vim https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/eaw-console.vim
```

2. .vimrc に以下を設定
~~~
source ~/.vim/eaw-console.vim
~~~

## neovimの設定

1. [eaw-console.vim](https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/eaw-console.vim) を`~/.config/nvim/`に配置
```
mkdir -p ~/.config/nvim/
curl -o ~/.config/nvim/eaw-console.vim https://raw.githubusercontent.com/hamano/locale-eaw/master/dist/eaw-console.vim
```

2. `~/.config/nvim/init.vim` に以下を設定
~~~
source ~/.config/nvim/eaw-console.vim
~~~

## mltermの設定

`dist/eaw-console.mlterm`の内容を`~/.mlterm/main`に設定

~~~
$ cat dist/eaw-console.mlterm >> ~/.mlterm/main
~~~

# 文字幅の裁定

## `※♨`
- 和文では全角として扱われてきた歴史がある
- 欧文であまり使われていないような印象

全角とする

## 丸数字(①〜⑳、⓪)
- 和文フォントでは全角が一般的
- ⑳を半角で表示するのは窮屈
- ⑳まではambiguousだが㉑からWideとなっている。⑳を半角にすると一貫性がなくなる
- ⓪はNeutralだがこれのみ半角とする訳にはいかない

全角とする

## ローマ数字
- 和文では全角で扱われてきた歴史がある
- 欧文ではi,v,xの組み合わせで表現されることがありあまり利用されていないような印象
- ⅷを半角で表示するのは窮屈で視認性が悪い
- ⅺとⅻがNeutralでそれ以外Ambiguous

全角とする

## 絵文字

絵文字はNeutralと定義されつつも、多くのフォントでは全角で描かれているという現状があります。
たとえば以下の範囲の文字。

- U+2600 - U+27FF (`☀☁☂☃`)
- U+1F000 - U+1FFFF (`🀀🌶🐿🕿`)

半角で描画するのが難しい絵文字はNeutralであっても全角とする。

## 罫線

TUIが壊れるため半角とする

## ブロック要素

プログレスバーが壊れるため半角とする

## `♠♥♣♦`

- 和文では全角で扱われてきた歴史がある
- 欧文では半角で扱われてきた歴史がある
- `♦`だけNeutral
- 半角で表示可能

半角とする

## `♩♪♫♬♭♮`

- 和文では全角で扱われてきた歴史がある
- 欧文では半角で扱われてきた歴史がある
- `♮`と`♫`はNeutral
- 半角で表示可能

半角とする

## U+2014 EM DASH
- JIS X 0208の全角ダッシュがU+2014 EM DASHにマッピングされている
- 文字名が表すようにem幅のダッシュは半角で描画することも可能
- 欧文では半角で扱われている

ひとまず半角とする

## U+2026 HORIZONTAL ELLIPSIS いわゆる三点リーダー

- 異なる字形を一つのコードポイントに割り当ててしまった過ちはもうどうにもならない。
  - 欧文では下部に点3つ、和文では中央に点3つで全角
- 欧文では半角前提で多用されている。

半角とするしかない。
和文で三点リーダーを使いたいときはEAW=Neutralだが`⋯`(U+22EF)を使うことができる

## 矢印(U+2190..U+21FF)
- `←↑→↓`だけであれば半角でも描画可能
- しかし`⇴⇼↹`などを半角で描画するのは視認性が悪い
- U+FFE9`￩￪￫￬`以降に半角の矢印が存在する、しかし斜め矢印が無い

ひとまず全角とし、問題があれば再検討

## 性別記号と天体記号
- `♀`(U+2640)と`♂`(U+2642)は性別を表しAmbiguousであるが半角で描画可能
- しかし`♀`と`♂`は金星と火星を表す天体記号でもあり、その他の天体記号との一貫性を配慮する必要がある
- その他の天体記号の土星`♄`天王星`♅`冥王星⯓を半角で描画するのは視認性が悪い

性別記号と天体記号は全角とする

## サイコロ(`⚀⚁⚂⚃⚄⚅`)
- サイコロは正方形であってほしい。

全角とする

## 麻雀牌
- `🀀`がNeutralで`🀄`はWide

全角とする

## ドミノ牌
- 縦の配は半角に収まるが、横の牌は全角でないと難しい
- よく考えたらドミノ倒ししかやったことがなくゲームのルールを知らない

全角とする

## トランプのカード
- `🃏`のみWideであとのカードはNerutral

全角とする

## NerdFont

- NerdFontの絵文字はほぼ全てが全角で描画されている。
- いくつかの文字のみ半角で描画可能(``)
- NerdFontで提供されているSymbolsNerdFontMono-Regular.ttfではこれらの文字も全角なので全て全角で扱ったほうが無難。

すべて全角とする

