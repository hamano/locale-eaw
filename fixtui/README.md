# fixtui

`fixtui` は、アプリケーションが出力する文字を動的に置換する TTY プロキシです。
East Asian Width (EAW) が「Ambiguous (A)」な文字を「Narrow (N)」な文字に置き換えることで、ターミナルの表示崩れを防ぎます。

## 概要

多くのターミナルアプリケーションでは、`●`、`○`、`▲`、`▼` などの文字（EAW=A）が使われます。これらの文字は、ターミナルの設定（`EAW=A` を 1 幅とするか 2 幅とするか）によって描画幅が変わります。

アプリケーション側が想定している幅とターミナル側の設定が一致しない場合、UI の枠線がズレたり、文字が重なったりして表示が崩れる原因となります。

`fixtui` はコマンドの出力をインターセプトし、これらの曖昧な幅の文字を、確実に 1 カラム幅で描画される文字（Narrow）に透過的に置換します。これにより、どのようなターミナル環境でも表示が壊れないようにします。

## 動作確認済み(動作確認中)アプリケーション

- lazygit
- opencode
- Claude CLI
- Github Copilot CLI
- Codex CLI
- Gemini CLI

## 特徴

- **PTY プロキシ**: ターゲットアプリケーションを擬似端末 (PTY) 上で実行するため、インタラクティブな TUI アプリでもそのまま動作します。
- **文字置換**: 出力バッファ内の EAW=A 文字を、等価な Narrow 文字に高速に置換します。
- **シグナル転送**: ウィンドウのリサイズ信号 (`SIGWINCH`) を正しく子プロセスに転送します。

## 置換テーブル

このテーブルはEAW-CONSOLEロケールを利用していて問題が起こる文字を置換しています。
EAW-FULLWIDTH+EAW=A文字を全角で表示するようターミナルを設定している場合、
BLACK CIRCLE -> BULLETの置換で問題があります。(BULLETのEAW=Aなので別の文字に調整してください。)

| 元の文字 (EAW=A) | 置換後 (Narrow) | 説明 |
| :--- | :--- | :--- |
| ● | • | BLACK CIRCLE -> BULLET |
| ○ | ◦ | WHITE CIRCLE -> WHITE BULLET |
| ◯ | ◦ | LARGE CIRCLE -> WHITE BULLET |
| ■ | ▪ | BLACK SQUARE -> BLACK SMALL SQUARE |
| □ | ▫ | WHITE SQUARE -> WHITE SMALL SQUARE |
| ▲ | ▴ | BLACK UP-POINTING TRIANGLE -> SMALL ... |
| ▼ | ▾ | BLACK DOWN-POINTING TRIANGLE -> SMALL ... |
| ← | ￩ | LEFTWARDS ARROW -> HALFWIDTH ... |
| ↑ | ￪ | UPWARDS ARROW -> HALFWIDTH ... |
| → | ￫ | RIGHTWARDS ARROW -> HALFWIDTH ... |
| ↓ | ￬ | DOWNWARDS ARROW -> HALFWIDTH ... |

## ビルドと使用方法

**ビルド:**
```bash
make
```

**インストール**
```bash
make install
```

`/usr/local/bin/fixtui`へ配置されます。

**使用方法:**
```bash
fixtui <command> [args...]
```

**lazygitの利用例**

```bash
fixtui lazygit
```

## ライセンス
MIT
