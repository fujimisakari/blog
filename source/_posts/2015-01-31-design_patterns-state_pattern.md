title: Stateパターン
date: 2015-01-31 03:36:01
tags: DesignPatterns
---

## パターンについて
状態が変化が多数ある場合に条件分岐を使うことなく状態変化を実現できます
状態毎のオブジェクトを作っておいて、状態を変えたいときは
コンテキスト内の状態オブジェクトを変更するだけで振舞いを容易に変更できます

>HeadFirstデザインパターンでの定義
>
>オブジェクトの内部状態が変化した際にオブジェクトがその振舞いを変更できます。
>オブジェクトはそのクラスを変更したように見えます。


## パターン構造
![state_pattern](/image/DesignPattern/state.png)
**構成要素**
Context :　多数の内部状態を持つことができるクラス
State :　すべての具象状態クラス用の共通インタフェース
ConcreteState :　各ConcreteStateは、Contextからの要求に対する独自の振舞いを実装する


## サンプル
ガムマシーンでお金を入れてガムが出てくるまでの状態を遷移をStateパターンで表現した場合

・ガムマシーン(Context)
``` java
public class GumballMachine {
 
	State soldOutState;
	State noQuarterState;
	State hasQuarterState;
	State soldState;
	State winnerState;
 
	State state = soldOutState;
	int count = 0;
 
	public GumballMachine(int numberGumballs) {
		soldOutState = new SoldOutState(this);
		noQuarterState = new NoQuarterState(this);
		hasQuarterState = new HasQuarterState(this);
		soldState = new SoldState(this);
		winnerState = new WinnerState(this);

		this.count = numberGumballs;
 		if (numberGumballs > 0) {
			state = noQuarterState;
		} 
	}
 
	public void insertQuarter() {
		state.insertQuarter();
	}
 
	public void ejectQuarter() {
		state.ejectQuarter();
	}
 
	public void turnCrank() {
		state.turnCrank();
		state.dispense();
	}

	void setState(State state) {
		this.state = state;
	}
 
	void releaseBall() {
		System.out.println("ガムボールがスロットから転がり出てきてます");
		if (count != 0) {
			count = count - 1;
		}
	}
 
	int getCount() {
		return count;
	}
 
    public State getState() {
        return state;
    }

    public State getSoldOutState() {
        return soldOutState;
    }

    public State getNoQuarterState() {
        return noQuarterState;
    }

    public State getHasQuarterState() {
        return hasQuarterState;
    }

    public State getSoldState() {
        return soldState;
    }

    public State getWinnerState() {
        return winnerState;
    }
}
```

・状態クラスのインタフェース(State)
``` java
public interface State {
 
	public void insertQuarter();
	public void ejectQuarter();
	public void turnCrank();
	public void dispense();
}
```

・まだお金を投入してない状態のクラス(ConcreteState)
``` java
public class NoQuarterState implements State {
    GumballMachine gumballMachine;
 
    public NoQuarterState(GumballMachine gumballMachine) {
        this.gumballMachine = gumballMachine;
    }
 
	public void insertQuarter() {
		System.out.println("25セントを投入しました");
		gumballMachine.setState(gumballMachine.getHasQuarterState());
	}
 
	public void ejectQuarter() {
		System.out.println("25セントを投入していません");
	}
 
	public void turnCrank() {
		System.out.println("クランクを回しましたが、25セントを投入していません");
	 }
 
	public void dispense() {
		System.out.println("まず支払いをする必要があります");
	}
}
```

・お金を投入した状態のクラス(ConcreteState)
``` java
public class HasQuarterState implements State {
	Random randomWinner = new Random(System.currentTimeMillis());
	GumballMachine gumballMachine;
 
	public HasQuarterState(GumballMachine gumballMachine) {
		this.gumballMachine = gumballMachine;
	}
  
	public void insertQuarter() {
		System.out.println("もう一度25セントを投入することはできません");
	}
 
	public void ejectQuarter() {
		System.out.println("25セントを返却しました");
		gumballMachine.setState(gumballMachine.getNoQuarterState());
	}
 
	public void turnCrank() {
		System.out.println("クランクを回しました...");
		int winner = randomWinner.nextInt(10);
		if ((winner == 0) && (gumballMachine.getCount() > 1)) {
			gumballMachine.setState(gumballMachine.getWinnerState());
		} else {
			gumballMachine.setState(gumballMachine.getSoldState());
		}
	}

    public void dispense() {
        System.out.println("販売するガムボールはありません");
    }
}
```

・ガムマシーンのクランクを回した状態のクラス(ConcreteState)
``` java
public class SoldState implements State {
    GumballMachine gumballMachine;
 
    public SoldState(GumballMachine gumballMachine) {
        this.gumballMachine = gumballMachine;
    }
       
	public void insertQuarter() {
		System.out.println("お待ちください。すでにガムボールを出しています");
	}
 
	public void ejectQuarter() {
		System.out.println("申し分けありせん。すでにクランクを回しています");
	}
 
	public void turnCrank() {
		System.out.println("2回まわしてもガムボールをもう1つ手に入れることはできません");
	}
 
	public void dispense() {
		gumballMachine.releaseBall();
		if (gumballMachine.getCount() > 0) {
			gumballMachine.setState(gumballMachine.getNoQuarterState());
		} else {
			System.out.println("おっと、ガムボールがなくなりました!");
			gumballMachine.setState(gumballMachine.getSoldOutState());
		}
	}
}
```

・当りがあたった状態のクラス(ConcreteState)
``` java
public class WinnerState implements State {
    GumballMachine gumballMachine;
 
    public WinnerState(GumballMachine gumballMachine) {
        this.gumballMachine = gumballMachine;
    }
 
	public void insertQuarter() {
		System.out.println("お待ちください。すでにガムボールを出しています");
	}
 
	public void ejectQuarter() {
		System.out.println("申し分けありせん。すでにクランクを回しています");
	}
 
	public void turnCrank() {
		System.out.println("2回まわしてもガムボールをもう1つ手に入れることはできません");
	}
 
	public void dispense() {
		gumballMachine.releaseBall();
		if (gumballMachine.getCount() == 0) {
			gumballMachine.setState(gumballMachine.getSoldOutState());
		} else {
			gumballMachine.releaseBall();
			System.out.println("当たりです!25セントで2つのガムボールがもらえます");
			if (gumballMachine.getCount() > 0) {
				gumballMachine.setState(gumballMachine.getNoQuarterState());
			} else {
            	System.out.println("おっとガムボールがなくなりました!");
				gumballMachine.setState(gumballMachine.getSoldOutState());
			}
		}
	}
}
```

・売り切れた状態のクラス(ConcreteState)
``` java
public class SoldOutState implements State {
    GumballMachine gumballMachine;
 
    public SoldOutState(GumballMachine gumballMachine) {
        this.gumballMachine = gumballMachine;
    }
 
	public void insertQuarter() {
		System.out.println("25セントを投入することはできません。このマシンは売り切れです。");
	}
 
	public void ejectQuarter() {
		System.out.println("返金できません。まだ25セントを投入していません");
	}
 
	public void turnCrank() {
		System.out.println("クランクを回しましたがガムボールはありません");
	}
 
	public void dispense() {
		System.out.println("販売するガムボールはありません");
	}
}
```

・実行コード(Client)
``` java
public class Client {

	public static void main(String[] args) {
		GumballMachine gumballMachine = new GumballMachine(5);

		System.out.println(gumballMachine);

		gumballMachine.insertQuarter();
		gumballMachine.turnCrank();
		gumballMachine.insertQuarter();
		gumballMachine.turnCrank();

		System.out.println(gumballMachine);

		gumballMachine.insertQuarter();
		gumballMachine.turnCrank();
		gumballMachine.insertQuarter();
		gumballMachine.turnCrank();

		System.out.println(gumballMachine);

		gumballMachine.insertQuarter();
		gumballMachine.turnCrank();
		gumballMachine.insertQuarter();
		gumballMachine.turnCrank();
	}
}
```
