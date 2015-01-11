title: Decoratorパターン
date: 2015-01-11 14:47:45
tags: DesignPatterns
---

## パターンについて
新しい振舞いを提供するために、オブジェクトをラップします。

>HeadFirstデザインパターンでの定義
>
>オブジェクトに付加的な責務を動的に付与します。
>デコレータは、サブクラス化の代替となる柔軟な機能拡張手段を提供します。


## 使い所
オブジェクト毎に機能を追加したいときや機能を動的に付け外ししたいとき、
同じインターフェースを持つオブジェクトで被せるようにすることで、既存クラスに手を加えずに機能追加ができます。


## パターン構造
![decorator_pattern](/image/DesignPattern/decorator.jpg)
**構成要素**
Component :　拡張される機能を定義した抽象クラス(abstractかinterfaceを利用する)
ConcreteComponent :　Componentクラスで定義した機能を基本実装する、装飾される具象クラス。
Decorator :　Componentを継承して装飾される具象クラスと型の一致を実現するためクラス。振舞いを取得するために継承してるわけではない
ConcreteDecoratorA, B :　具象クラスに機能の拡張(飾り付け)を行う具象クラス。


## サンプル
コーヒーにトッピングを入れて注文した場合

・飲み物
``` java
public abstract class Beverage {
	String description = "不明な飲み物";

	String getDescription() {
		return description;
	}

	public abstract double cost();
}
```

・コーヒー_ダークロースト
``` java
public class DarkRoast extends Beverage {

	public DarkRoast() {
		description = "ダークロースト";
	}

	public double cost() {
		return 340;
    }
}
```

・デコレータ
``` java
public abstract class CondimentDecorator extends Beverage {
	// Beverage.getDescriptionがabstractになってないので再定義
	abstract String getDescription();
}
```

・トッピング_ホイップ
``` java
public class Whip extends CondimentDecorator {

	Beverage beverage;

	public Whip(Beverage beverage) {
		this.beverage = beverage;
	}

	public double cost() {
		return beverage.cost() + 30;
	}

	public String getDescription() {
		return beverage.getDescription() + "、ホイップ入り";
    }
}
```

・トッピング_ミルク
``` java
public class Milk extends CondimentDecorator {

	Beverage beverage;

	public Milk(Beverage beverage) {
		this.beverage = beverage;
	}

	public double cost() {
		return beverage.cost() + 30;
	}

	public String getDescription() {
		return beverage.getDescription() + "、ミルク入り";
    }
}
```

・実行コード
``` java
public class Client {
	public static void main(String[] args) {
		Beverage darkRoast = new DarkRoast();
		darkRoast = new Milk(darkRoast);
		darkRoast = new Whip(darkRoast);
		System.out.println("商品名: " + darkRoast.getDescription());
		System.out.println("値段: " + darkRoast.cost());
    }
}

// 実行結果
// 商品名: ダークロースト、ホイップ入り、ミルク入り
// 値段: 400
```
