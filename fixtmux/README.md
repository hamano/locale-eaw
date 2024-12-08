# tmuxの修正

tmuxはロケールの文字幅を利用しますがtmux 2.7以降、
修正したロケール `UTF-8-EAW-CONSOLE` や `UTF-8-EAW-FULLWIDTH`
を導入してもtmuxでは反映されません。

tmuxは起動時に

```
setlocale(LC_CTYPE, "C")
```

を実行するからです。
tmuxを以下のように修正すると解決します。

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

しかしtmuxのアップデートの度修正を行うのは大変なのでLD_PRELOADによる修正を行います。

# ビルド

```
$ make
gcc -shared -fPIC -ldl fixtmux.c -o fixtmux.so
```

# 利用方法

```
LD_PRELOAD=./fixtmux.so tmux
```

# ラッパースクリプト

`fixtmux.so`と`tmux.sh`を`/usr/local/bin`などに置いておくと便利です。

```
$ make install
install -m 755 fixtmux.so /usr/local/bin
install -m 755 tmux.sh /usr/local/bin/tmux
```

# 罫線の修正

`UTF-8-EAW-CONSOLE` を利用する場合、罫線は半角なので問題ありません。

`UTF-8-EAW-FULLWIDTH` を利用する場合、罫線が全角となるので表示が壊れます。

ターミナルがACSをサポートしている場合、罫線の描画にACSを使うよう設定すると良いでしょう。

~/.tmux.conf:

~~~
set -ag terminal-overrides ',*:U8=0'
~~~

* [tmuxの罫線素片をACSに強制する](https://qiita.com/yanma/items/2644e6db6f3bcf249690)

