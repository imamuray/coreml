# coreml

大堀淳『コンパイラ 原理と構造』（共立出版, 2021年）を参考にして書いたコードです。

- [出版社サイト](https://www.kyoritsu-pub.co.jp/book/b10003349.html)
- [サポートページ](https://atsushiohori.github.io/ja/texts/compiler/)
- [サポートリポジトリ](https://github.com/AtsushiOhori/compiler-text)

## 動作環境

実行には[SML#](https://smlsharp.github.io/ja/)が必要です。

このリポジトリは以下で動作を確認しています:

- WSL2 (Ubuntu 22.04 LTS)
- SML# 4.2.0

## 実行方法

各章の `main` ディレクトリにある Makefile でコンパイルします。

例:

```bash
$ cd chapter3/main
$ make
$ ./Main Main.smi
_require
"basis.smi"
_require
"./Top.smi"
```

各章の実行例はテキストを参照してください。

`chapter6,7,8`はテスト用の`test.sml`で動作確認ができます。

例:

```bash
$ cd chapter6/main
$ make
$ ./Main test.sml
Parse result:
val num = 1
Inferred Typing:
val num : int
Evaluated to:
val num = 1
...
```
