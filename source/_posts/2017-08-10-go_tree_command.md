title: Goでtreeコマンド
date: 2017-08-10 00:00
tags: Go

---

最近、Goでtreeコマンドを作ってみた。
自分はディレクトリとファイルのツリー構造をREADMEなど仕様上の資料に含めることが多いのですが、
その際に必要とするディレクトリとファイルを`mkdir`、`touch`してから、ツリー構造の先頭に行き
treeコマンドを実行して用意するのが手間だったので作りました。

https://github.com/fujimisakari/go-tree

使い方は、yamlでtree構造を定義するだけです。
`go-tree`以降からyamlの配列で定義していき、ディレクトリには末尾にセミコロン付けます。
また、`root-dir`でrootディレクトリ名を定義することができます(defaultでは`.`になります)

- sample.yaml
```yaml
root-dir: "Sample"
go-tree:
  - fizz
  - buzz
  - dir1:
    - comp1-1
    - comp1-2
    - comp1-3
    - comp1-4
  - dir2:
    - comp2-1
    - comp2-2
    - comp2-3
    - comp2-4
    - dir3:
      - comp3-1
      - comp3-2
      - dir4:
        - comp4-1
        - comp4-2
        - comp4-3
      - comp3-3
      - comp3-4
    - comp2-5
    - comp2-6
  - dir5:
    - comp4-1
    - comp4-2
    - comp4-3
    - comp4-4
  - foo
  - bar
```

あとは、go-treeコマンドのパラメータにyamlファイルのpathに渡して実行。
```
$ go-tree ./sample.yaml
Sample
├── fizz
├── buzz
├── dir1
│   ├── comp1-1
│   ├── comp1-2
│   ├── comp1-3
│   └── comp1-4
├── dir2
│   ├── comp2-1
│   ├── comp2-2
│   ├── comp2-3
│   ├── comp2-4
│   ├── dir3
│   │   ├── comp3-1
│   │   ├── comp3-2
│   │   ├── dir4
│   │   │   ├── comp4-1
│   │   │   ├── comp4-2
│   │   │   └── comp4-3
│   │   ├── comp3-3
│   │   └── comp3-4
│   ├── comp2-5
│   └── comp2-6
├── dir5
│   ├── comp4-1
│   ├── comp4-2
│   ├── comp4-3
│   └── comp4-4
├── foo
└── bar
```
