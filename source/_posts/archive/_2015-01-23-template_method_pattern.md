title: TemplateMethodパターン
date: 2015-01-23 23:05:59
tags: DesignPatterns
---

## パターンについて
アルゴリズム実現するためのテンプレート作成するパターンになります。
同じような処理が複数あるとき、基本操作(primitiveOperation)をサブクラスでオーバーライドすることで、
全体を共通化しながら、バリエーションによる影響を局所的に抑えられます

>HeadFirstデザインパターンでの定義
>
>メソッドにおけるアルゴリズムの骨組みを定義し、いくつかの手順をサブクラスに先送りします。
>TemplateMethodは、アルゴリズムの構造を変えることなく、アルゴリズムのある手順をサブクラスに再定義します


## パターン構造
![templatemethod_pattern](/image/DesignPattern/templatemethod.png)
**構成要素**
AbstractClass :　templateMethodは基本操作(primitiveOperation)を利用しますが実装がサブクラスに任せます
ConcreteClass :　抽象操作の実装します


## サンプル
カフェイン飲料クラステンプレートを利用いて、コーヒーと紅茶を生成する場合

・カフェイン飲料(AbstractClass)
``` java
public abstract class CaffeineBeverage {
  
	final void prepareRecipe() {
		boilWater();
		brew();
		pourInCup();
		addCondiments();
	}
 
	abstract void brew();
  
	abstract void addCondiments();
 
	void boilWater() {
		System.out.println("Boiling water");
	}
  
	void pourInCup() {
		System.out.println("Pouring into cup");
	}
}
```

・コーヒー(ConcreteClass)
``` java
public class Coffee extends CaffeineBeverage {
	public void brew() {
		System.out.println("Dripping Coffee through filter");
	}
	public void addCondiments() {
		System.out.println("Adding Sugar and Milk");
	}
}
```

・紅茶(ConcreteClass)
``` java
public class Tea extends CaffeineBeverage {
	public void brew() {
		System.out.println("Steeping the tea");
	}
	public void addCondiments() {
		System.out.println("Adding Lemon");
	}
}
```

・実行コード(Client)
``` java
public class Client {
	public static void main(String[] args) {
 
		Tea tea = new Tea();
		Coffee coffee = new Coffee();
 
		System.out.println("\nMaking tea...");
		tea.prepareRecipe();
 
		System.out.println("\nMaking coffee...");
		coffee.prepareRecipe();
	}
}
```
