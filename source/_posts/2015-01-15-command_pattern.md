title: Commandパターン
date: 2015-01-15 02:27:11
tags: DesignPatterns
---

## パターンについて
Commandパターンは、リクエストを行うオブジェクトと、その実行方法を
知っているオブジェクトを分離させます。
そして、どのリクエストにも対応できるよう共通のAPIを持ち合せており
異る種類のリクエストでも同様の呼び出しでアクションを実行させることできる

>HeadFirstデザインパターンでの定義
>
>リクエストをオブジェクトとしてカプセル化し、
>その結果、他のオブジェクトを異るリクエスト、キューまたはログリクエストで
>パラメータ化でき、アンドゥ可能な操作もサポートします

## パターン構造
![command_pattern](/image/DesignPattern/command.png)
**構成要素**
Invoker :　コマンドを保持しており、コマンドにリクエストを実行するよう依頼するクラス
Command :　リクエストを実行するためのインタフェースを定義
ConcreteCommand :　リクエストの実行に必要なReceiverのactionを定義してます
Receiver :　リクエストに対処するために処理の実行方法を知っている。どのクラスでもいい。


## サンプル
リモコンに照明ON/OFF、ガレージ開閉の機能を付与する場合

・コマンド(Command)
``` java
public interface Command {
	public void execute();

	public void undo();
}
```

・照明クラス(Client, Receiver)
``` java
public class Light {

	public Light() {
	}

	public void on() {
		System.out.println("照明をつけます");
	}

	public void off() {
		System.out.println("照明を消します");
	}
}
```

・照明ONコマンド(ConcreteCommand)
``` java
public class LightOnCommand implements Command {
	Light light;

	public LightOnCommand(Light light) {
		this.light = light;
	}

	public void execute() {
		light.on();
	}

	public void undo() {
		light.off();
	}
}
```

・照明OFFコマンド(ConcreteCommand)
``` java
public class LightOffCommand implements Command {
	Light light;

	public LightOffCommand(Light light) {
		this.light = light;
	}

	public void execute() {
		light.off();
	}

	public void undo() {
		light.on();
	}
}
```

・ガレージ(Client, Receiver)
``` java
public class GarageDoor {

	public void up() {
		System.out.println("ゲージを開くよ");
	}

	public void down() {
		System.out.println("ゲージを閉じるよ");
	}

	public void stop() {
		System.out.println("ゲージを止めるよ");
	}

	public void lightUp() {
		System.out.println("ゲージの照明をつけるよ");
	}

	public void lightDown() {
		System.out.println("ゲージの照明を落すよ");
    }
}
```

・ガレージONコマンド(ConcreteCommand)
``` java
public class GarageDoorOpenCommand implements Command {

	GarageDoor garageDoor;

	public GarageDoorOpenCommand(GarageDoor garageDoor) {
		this.garageDoor = garageDoor;
	}

	public void execute() {
		garageDoor.lightUp();
		garageDoor.up();
	}

	public void undo() {
		garageDoor.lightDown();
		garageDoor.down();
	}
}
```

・ガレージOFFコマンド(ConcreteCommand)
``` java
public class GarageDoorCloseCommand implements Command {

	GarageDoor garageDoor;

	public GarageDoorCloseCommand(GarageDoor garageDoor) {
		this.garageDoor = garageDoor;
	}

	public void execute() {
		garageDoor.lightDown();
		garageDoor.down();
	}

	public void undo() {
		garageDoor.lightUp();
		garageDoor.up();
	}
}
```

・空のコマンド(ConcreteCommand)
``` java
public class NoCommand implements Command {
	public void execute() {
	}

	public void undo() {
	}
}
```

・リモコン(Invoker)
``` java
public class RemoteControl {
	Command[] onCommands;
	Command[] offCommands;
	Command undoCommand;

	public RemoteControl() {
		onCommands = new Command[7];
		offCommands = new Command[7];

		Command noCommand = new NoCommand();
		for (int i = 0; i < 7; i++) {
			onCommands[i] = noCommand;
			offCommands[i] = noCommand;
		}
		undoCommand = noCommand;
	}

	public void setCommand(int slot, Command onCommand, Command offCommand) {
		onCommands[slot] = onCommand;
		offCommands[slot] = offCommand;
	}

	public void onButtonWasPressed(int slot) {
		onCommands[slot].execute();
		undoCommand = onCommands[slot];
	}

	public void offButtonWasPressed(int slot) {
		offCommands[slot].execute();
		undoCommand = offCommands[slot];
	}

	public void undoButtonWasPressed() {
		undoCommand.undo();
	}
}
```

・実行コード
``` java
public class Client {

	public static void main(String[] args) {
		RemoteControl remote = new RemoteControl();

		Light light = new Light();
		GarageDoor garageDoor = new GarageDoor();

		Command lightOn = new LightOnCommand(light);
		Command lightOff = new LightOffCommand(light);
		Command garageDoorOpen = new GarageDoorOpenCommand(garageDoor);
		Command garageDoorClose = new GarageDoorCloseCommand(garageDoor);

		remote.setCommand(0, lightOn, lightOff);
		remote.setCommand(1, garageDoorOpen, garageDoorClose);

		remote.onButtonWasPressed(0);
		remote.offButtonWasPressed(0);
		remote.undoButtonWasPressed();
		remote.onButtonWasPressed(1);
		remote.undoButtonWasPressed();
		remote.offButtonWasPressed(1);

	}
```
