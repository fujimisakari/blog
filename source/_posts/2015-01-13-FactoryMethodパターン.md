title: FactoryMethodパターン
date: 2015-01-13 00:10:16
tags: DesignPatterns
---

## パターンについて
サブクラスが作成する具象クラスを決定します。
インスタンス化したいオブジェクト(製品)を実行時の条件によって決めたい場合に利用します。
ただ、FactoryMethodパターンはオブジェクト(製品)を生成する側と利用する側を分けて定義する必要があります。
分けておくことで、将来システムに起こり得る変更をあらかじめ分離でき保守性を保つことができます。

>HeadFirstデザインパターンでの定義
>
>オブジェクト作成のためのインタフェースを定義するが
>どのクラスをインスタンス化するかについてはサブクラスに決定させる。
>Factory Methodにより、クラスはサブクラスにインスタンス化を先送りできる。


## パターン構造
![factorymethod_pattern](/image/DesignPattern/abstractfactory.png)
**構成要素**
Product :　生成されるオブジェクト（製品）のAPIを定義する抽象クラス。製品の具象クラスが抽象クラスで使用できるようにする
ConcreteProduct :　生成される具象製品クラス
Creator :　ファクトリメソッドを除く製品操作するAPI抽象クラスを定義する抽象クラス。
ConcreteCreator	:　ファクトリメソッドを実装し実際に製品を作成するクラス。


## サンプル
ピザ店でチーズピザと野菜ピザを作る場合

・抽象ピザ店クラス(Creator)
``` java
public abstract class PizzaStore {
 
	abstract Pizza createPizza(String item);
 
	public Pizza orderPizza(String type) {
		Pizza pizza = createPizza(type);
		System.out.println("--- Making a " + pizza.getName() + " ---");
		pizza.prepare();
		pizza.bake();
		pizza.cut();
		pizza.box();
		return pizza;
	}
}
```

・具象ピザ店クラス(ConcreteCreator)
``` java
public class NYPizzaStore extends PizzaStore {

	Pizza createPizza(String item) {
		if (item.equals("cheese")) {
			return new NYStyleCheesePizza();
		} else if (item.equals("veggie")) {
			return new NYStyleVeggiePizza();
		} else return null;
	}
}
```

・抽象ピザクラス(Product)
``` java
public abstract class Pizza {
	String name;
	String dough;
	String sauce;
	ArrayList<String> toppings = new ArrayList<String>();
 
	void prepare() {
		System.out.println("Prepare " + name);
		System.out.println("Tossing dough...");
		System.out.println("Adding sauce...");
		System.out.println("Adding toppings: ");
		for (String topping : toppings) {
			System.out.println("   " + topping);
		}
	}
  
	void bake() {
		System.out.println("Bake for 25 minutes at 350");
	}
 
	void cut() {
		System.out.println("Cut the pizza into diagonal slices");
	}
  
	void box() {
		System.out.println("Place pizza in official PizzaStore box");
	}
 
	public String getName() {
		return name;
	}

	public String toString() {
		StringBuffer display = new StringBuffer();
		display.append("---- " + name + " ----\n");
		display.append(dough + "\n");
		display.append(sauce + "\n");
		for (String topping : toppings) {
			display.append(topping + "\n");
		}
		return display.toString();
	}
}
```

・具象ピザクラス1(ConcreteProductA)
``` java
public class NYStyleCheesePizza extends Pizza {

	public NYStyleCheesePizza() { 
		name = "NY Style Sauce and Cheese Pizza";
		dough = "Thin Crust Dough";
		sauce = "Marinara Sauce";
 
		toppings.add("Grated Reggiano Cheese");
	}
}
```

・具象ピザクラス2(ConcreteProductB)
``` java
public class NYStyleVeggiePizza extends Pizza {

	public NYStyleVeggiePizza() {
		name = "NY Style Veggie Pizza";
		dough = "Thin Crust Dough";
		sauce = "Marinara Sauce";
 
		toppings.add("Grated Reggiano Cheese");
		toppings.add("Garlic");
		toppings.add("Onion");
		toppings.add("Mushrooms");
		toppings.add("Red Pepper");
	}
}
```

・実行コード
``` java
public class Client {
 
	public static void main(String[] args) {
		PizzaStore nyStore = new NYPizzaStore();
 
		Pizza pizza = nyStore.orderPizza("cheese");
		System.out.println("Ethan ordered a " + pizza.getName() + "\n");
 
		pizza = nyStore.orderPizza("veggie");
		System.out.println("Ethan ordered a " + pizza.getName() + "\n");
	}
}
```
