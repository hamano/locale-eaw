# East Asian Width を全角で表示する UTF-8ロケール

○や×や△が半角で表示されてしまう、いわゆるEast Asian Ambiguous問題に
ついて、

大抵のターミナルやエディタには独自の対処方法があるけれど、一部のターミ
ナル(rxvt-unicodeなど)で解決方法がない場合にAmbiguous文字を全角にする修
正UTF-8ロケールです。

[こちら](http://vdr.jp/d/20070322.html)で配布されている
UTF-8-EAW-FULLWIDTH.gz がちょっと古くなっていたのでメンテ出来るようにし
ました。

# インストール方法

1. [UTF-8-EAW-FULLWIDTH.gz](https://raw.githubusercontent.com/hamano/locale-eaw-fullwidth/master/UTF-8-EAW-FULLWIDTH.gz) を /usr/share/i18n/charmaps/ に配置

2. /etc/locale.gen を以下のように変更
~~~
#ja_JP.UTF-8 UTF-8
ja_JP.UTF-8 UTF-8-EAW-FULLWIDTH
~~~

3. locale-gen を実行
