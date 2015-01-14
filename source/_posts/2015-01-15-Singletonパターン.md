title: Singletonパターン
date: 2015-01-15 00:28:46
tags: DesignPatterns
---

## パターンについて
ただ1つだけのオブジェクトが作成されていることを保証します。

>HeadFirstデザインパターンでの定義
>
>1つのクラスがただ1つのインスタンスを持つことを保証し
>インスタンスにアクセスするグローバルポイントを提供する。


## パターン構造
![singleton_pattern](/image/DesignPattern/singleton.jpg)
**構成要素**
uniqueInstance :　getInstanceで生成されたインスタンスを保持しておく静的な変数。
Singleton(コンストラクタ) :　スコープをprivateに定義し、getInstanceからのみ生成できるようにする。
getInstance :　唯一のインスタンスを取得できる静的メソッド。呼び出し時にインスタンスが存在しなれば生成する。


## サンプル
Singletonクラスを生成する場合(マルチスレッドの場合は別途対策が必要)

・Singletonクラス(Singleton)
``` java
public class Singleton {
	private static Singleton uniqueInstance;
 
	private Singleton() {}
 
	public static Singleton getInstance() {
		if (uniqueInstance == null) {
			uniqueInstance = new Singleton();
		}
		return uniqueInstance;
	}
 
	// other useful methods here
	public String getDescription() {
		return "I'm a classic Singleton!";
	}
}
```

・実行コード
``` java
public class Client {
	public static void main(String[] args) {
		Singleton singleton = Singleton.getInstance();
        System.out.println(singleton.getDescription());
	}
}
```
