title: Facadeパターン
date: 2015-01-21 23:31:54
tags: DesignPatterns
---

## パターンについて
Facadeパターンは、何らかのサブシステムに属する一連の複雑なクラスを
簡素化して統合するFacadeクラスを作成します。
クライアントはサブシステムのこと何も意識せずにFacadeクラスのみに依存した
最小構成のシステムを実現できます。

>HeadFirstデザインパターンでの定義
>
>サブシステムの一連のインタフェースに対する統合されたインタフェースを提供する
>ファサードは、サブシステムをより使いやすくする高水準インタフェースを定義する


## パターン構造
![facade_pattern](/image/DesignPattern/facade.png)
**構成要素**
Client :　Facadeインタフェースを利用するクライアント
Facade :　統合されたインタフェース
Class1〜5 :　サブシステムのクラス


## サンプル
ホームシアターをFacadeで定義した場合

・プロジェクター
``` java
public class Projector {
	String description;
	DvdPlayer dvdPlayer;
	
	public Projector(String description, DvdPlayer dvdPlayer) {
		this.description = description;
		this.dvdPlayer = dvdPlayer;
	}
 
	public void on() {
		System.out.println(description + " on");
	}
 
	public void off() {
		System.out.println(description + " off");
	}

	public void wideScreenMode() {
		System.out.println(description + " in widescreen mode (16x9 aspect ratio)");
	}

	public void tvMode() {
		System.out.println(description + " in tv mode (4x3 aspect ratio)");
	}
  
	public String toString() {
			return description;
	}
}
```

・スクリーン
``` java
public class Screen {
	String description;

	public Screen(String description) {
		this.description = description;
	}

	public void up() {
		System.out.println(description + " going up");
	}

	public void down() {
		System.out.println(description + " going down");
	}

	public String toString() {
		return description;
	}
}
```

・チューナー
``` java
public class Tuner {
	String description;
	Amplifier amplifier;
	double frequency;

	public Tuner(String description, Amplifier amplifier) {
		this.description = description;
	}

	public void on() {
		System.out.println(description + " on");
	}

	public void off() {
		System.out.println(description + " off");
	}

	public void setFrequency(double frequency) {
		System.out.println(description + " setting frequency to " + frequency);
		this.frequency = frequency;
	}

	public String toString() {
		return description;
	}
}
```

・DVDプレイヤー
``` java
public class DvdPlayer {
	String description;
	int currentTrack;
	Amplifier amplifier;
	String movie;
	
	public DvdPlayer(String description, Amplifier amplifier) {
		this.description = description;
		this.amplifier = amplifier;
	}

	public void on() {
		System.out.println(description + " on");
	}
 
	public void off() {
		System.out.println(description + " off");
	}

	public void eject() {
		movie = null;
																							    System.out.println(description + " eject");
				}
 
	public void play(String movie) {
		this.movie = movie;
		currentTrack = 0;
		System.out.println(description + " playing \"" + movie + "\"");
	}

	public void stop() {
		currentTrack = 0;
		System.out.println(description + " stopped \"" + movie + "\"");
	}
 
	public String toString() {
		return description;
	}
}
```

・アンプ
``` java
public class Amplifier {
	String description;
	Tuner tuner;
	DvdPlayer dvd;
	
	public Amplifier(String description) {
		this.description = description;
	}
 
	public void on() {
		System.out.println(description + " on");
	}
 
	public void off() {
		System.out.println(description + " off");
	}
 
	public void setStereoSound() {
		System.out.println(description + " stereo mode on");
	}
 
	public void setSurroundSound() {
		System.out.println(description + " surround sound on (5 speakers, 1 subwoofer)");
	}
 
	public void setVolume(int level) {
		System.out.println(description + " setting volume to " + level);
	}

	public void setTuner(Tuner tuner) {
		System.out.println(description + " setting tuner to " + dvd);
		this.tuner = tuner;
	}
  
	public void setDvd(DvdPlayer dvd) {
		System.out.println(description + " setting DVD player to " + dvd);
		this.dvd = dvd;
	}
 
	public String toString() {
		return description;
	}
}
```

・Facadeクラス
``` java
public class HomeTheaterFacade {
	Amplifier amp;
	Tuner tuner;
	DvdPlayer dvd;
	Projector projector;
	Screen screen;
 
	public HomeTheaterFacade(Amplifier amp, 
				 Tuner tuner, 
				 DvdPlayer dvd, 
				 Projector projector, 
				 Screen screen) {
 
		this.amp = amp;
		this.tuner = tuner;
		this.dvd = dvd;
		this.projector = projector;
		this.screen = screen;
	}
 
	public void watchMovie(String movie) {
		System.out.println("Get ready to watch a movie...");
		screen.down();
		projector.on();
		projector.wideScreenMode();
		amp.on();
		amp.setDvd(dvd);
		amp.setSurroundSound();
		amp.setVolume(5);
		dvd.on();
		dvd.play(movie);
	}
 
 
	public void endMovie() {
		System.out.println("Shutting movie theater down...");
		screen.up();
		projector.off();
		amp.off();
		dvd.stop();
		dvd.eject();
		dvd.off();
	}

	public void listenToRadio(double frequency) {
		System.out.println("Tuning in the airwaves...");
		tuner.on();
		tuner.setFrequency(frequency);
		amp.on();
		amp.setVolume(5);
		amp.setTuner(tuner);
	}

	public void endRadio() {
		System.out.println("Shutting down the tuner...");
		tuner.off();
		amp.off();
	}
}
```

・実行コード(Client)
``` java
public class Client {
	public static void main(String[] args) {
		Amplifier amp = new Amplifier("Top-O-Line Amplifier");
		Tuner tuner = new Tuner("Top-O-Line AM/FM Tuner", amp);
		DvdPlayer dvd = new DvdPlayer("Top-O-Line DVD Player", amp);
		Projector projector = new Projector("Top-O-Line Projector", dvd);
		Screen screen = new Screen("Theater Screen");

		HomeTheaterFacade homeTheater = new HomeTheaterFacade(amp, tuner, dvd,projector, screen);
		homeTheater.watchMovie("Raiders of the Lost Ark");
		homeTheater.endMovie();
	}
}
```
