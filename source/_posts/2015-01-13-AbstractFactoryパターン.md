title: AbstractFactoryパターン
date: 2015-01-13 03:33:25
tags: DesignPatterns
---

## パターンについて
クライアントが具象クラス特定することなく一連のオブジェクト作成できるようにします。
そのため、具体的なクラスをクライアントから隠蔽でき容易に切り換えることができます。

>HeadFirstデザインパターンでの定義
>
>具象クラスを指定することなく、一連の関連するオブジェクトや
>依存オブジェクトを作成するためのインタフェースを提供する


## パターン構造
![abstractfactory_pattern](/image/DesignPattern/abstractfactory.jpg)
**構成要素**
AbstractFactory :　製品を生成するための抽象メソッドを定義するクラスです。
ConcreteFactory :　生成メソッドを実装するクラス。ここには、具体的な製品であるConcreteProductクラスを返す
AbstractProduct :　生成される製品のAPIを定義する抽象クラス。製品の具象クラスが抽象クラスで使用できるようにする
ConcreteProduct :　生成される具象製品クラス
Client :　AbstractFactoryクラスとAbstractProductクラスを利用する。


## サンプル
チーズピザとペペロンチーノピザのオブジェクトを生成する場合

・抽象ピザクラス
``` java
public abstract class Pizza {
	String name;

	Dough dough;
	Sauce sauce;
	Cheese cheese;
	Pepperoni pepperoni;

	public abstract void prepare();

	public void bake() {
		System.out.println("Bake for 25 minutes at 350");
	}

	public void cut() {
		System.out.println("Cutting the pizza into diagonal slices");
	}

	public void box() {
		System.out.println("Place pizza in official PizzaStore box");
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public String toString() {
		StringBuffer result = new StringBuffer();
		result.append("---- " + name + " ----\n");
		if (dough != null) {
			result.append(dough);
			result.append("\n");
		}
		if (sauce != null) {
			result.append(sauce);
			result.append("\n");
		}
		if (cheese != null) {
			result.append(cheese);
			result.append("\n");
		}
		if (pepperoni != null) {
			result.append(pepperoni);
			result.append("\n");
		}
		return result.toString();
	}
}
```

・具象ピザクラス1(Client)
``` java
public class CheesePizza extends Pizza {
	PizzaIngredientFactory ingredientFactory;
 
	public CheesePizza(PizzaIngredientFactory ingredientFactory) {
		this.ingredientFactory = ingredientFactory;
	}
 
	public void prepare() {
		System.out.println("Preparing " + name);
		dough = ingredientFactory.createDough();
		sauce = ingredientFactory.createSauce();
		cheese = ingredientFactory.createCheese();
	}
}
```

・具象ピザクラス2(Client)
``` java
public class PepperoniPizza extends Pizza {
	PizzaIngredientFactory ingredientFactory;
 
	public PepperoniPizza(PizzaIngredientFactory ingredientFactory) {
		this.ingredientFactory = ingredientFactory;
	}
 
	public void prepare() {
		System.out.println("Preparing " + name);
		dough = ingredientFactory.createDough();
		sauce = ingredientFactory.createSauce();
		cheese = ingredientFactory.createCheese();
		pepperoni = ingredientFactory.createPepperoni();
	}
}
```

・抽象素材クラス(AbstractFactory)
``` java
public interface PizzaIngredientFactory {
 
	public Dough createDough();
	public Sauce createSauce();
	public Cheese createCheese();
	public Pepperoni createPepperoni();
}
```

・具象素材クラス(ConcreteFactory)
``` java
public class NYPizzaIngredientFactory implements PizzaIngredientFactory {
 
	public Dough createDough() {
		return new ThinCrustDough();
	}
 
	public Sauce createSauce() {
		return new MarinaraSauce();
	}
 
	public Cheese createCheese() {
		return new ReggianoCheese();
	}

	public Pepperoni createPepperoni() {
		return new SlicedPepperoni();
	}
}
```

・パン生地クラス(抽象(AbstratProductA), 具象(ProductA))
``` java
public interface Dough {
	public String toString();
}

public class ThickCrustDough implements Dough {
	public String toString() {
		return "ThickCrust style extra thick crust dough";
	}
}
```

・チーズクラス(抽象(AbstratProductB), 具象(ProductB))
``` java
public interface Cheese {
	public String toString();
}

public class ReggianoCheese implements Cheese {
	public String toString() {
		return "Reggiano Cheese";
	}
}
```

・ソースクラス(抽象(AbstratProductC), 具象(ProductC))
``` java
public interface Sauce {
	public String toString();
}

public class MarinaraSauce implements Sauce {
	public String toString() {
		return "Marinara Sauce";
	}
}
```

・ペペロンチーノクラス(抽象(AbstratProductD), 具象(ProductD))
``` java
public interface Pepperoni {
	public String toString();
}

public class SlicedPepperoni implements Pepperoni {
	public String toString() {
		return "Sliced Pepperoni";
	}
}
```

・実行コード
``` java
public class Client {
 
	public static void main(String[] args) {
		Pizza pizza = null;
		PizzaIngredientFactory ingredientFactory = new NYPizzaIngredientFactory();
  
		pizza = new CheesePizza(ingredientFactory);
		pizza.setName("New York Style Cheese Pizza");
		System.out.println("Ethan ordered a " + pizza + "\n");
        
		pizza = new PepperoniPizza(ingredientFactory);
		pizza.setName("New York Style Pepperoni Pizza");
		System.out.println("Ethan ordered a " + pizza + "\n");
	}
}
```
