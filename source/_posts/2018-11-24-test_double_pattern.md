title: Test Double パターン
date: 2018-11-24 00:00
tags:
- Go
- Test

---

ユニットテストでよく mock や stub, fake など見かけるが理解度が低かったので、テストでは有名な [xUnit Test Patterns](http://xunitpatterns.com/Test%20Double.html) を漁ってみると
これらの用語は Test Double パターン がそれに該当するようだったので調べてみた


## Test Double パターン について

対象コードをテストするために依存してるコンポーネントを別で用意するパターンの総称となる。
もともと由来は映画のスタントマンから来てるようで、現場では俳優の代わりに「スタント・ダブル」を雇います。
彼らは単独で行動こそできませんが、どのように大きな高さから落ちるか、車を墜落させるか、シーンが求めているものを知っています。
コードも同様でテスト用に任意の振る舞いを持たせた依存コンポーネントを用意してテストに入ります。

Test Doubleでは、どのように/なぜ使用するかに基づいて次の分類されます

![testdoubles](http://xunitpatterns.com/Types%20Of%20Test%20Doubles.gif)
(http://xunitpatterns.com/Test%20Double.html より参照)

今は、「Test Stub」「Mock Oject」「Fake Object」 のパターンについて調べた


## 各テストパターン

- 用語
  - SUT: system under test の略でテスト対象のこと
  - 間接入力: テスト対象ロジックが外部コンポーネントに依存している場合、外部コンポーネントからの返り値のこと
  - 間接出力: テスト対象ロジック中の出力を依存してる外部コンポーネントに引数などから渡したりすること
  
### Test Stub

Test StubはSUTが依存するコンポーネントを置き換えてテストするパターン

特徴としては
- テストケースにSUTの間接入力用の制御ポイントを用意する
- 間接出力を検証する必要ない

**SUT**
```go
package teststub

type PostCode struct {
	code    string
	address string
}

type PostCodeRepo interface {
	GetByCode(code string) *PostCode
}

type AddressService struct {
	repo PostCodeRepo
}

func (s *AddressService) GetAddress(code string) string {
	pc := s.repo.GetByCode(code)
	return pc.address
}
```

**Test**
```go
package teststub

import "testing"

type testRepo struct {
	address string
}

func (r *testRepo) GetByCode(code string) *PostCode {
	return &PostCode{address: r.address}
}

func TestGetAddress(t *testing.T) {
	wantAddress := "AAA県BBB市CCC町"
	repo := &testRepo{address: wantAddress}

	as := AddressService{repo: repo}
	address := as.GetAddress("123-4567")
	if got, want := address, wantAddress; got != want {
		t.Fatalf("got %v, want %v", got, want)
	}
}
```

### Mock Object

Mock Objectは、SUTの動作検証のほかに間接的出力の副作用も検証に含ませて実施するパターン

特徴としては
- SUTが依存するobjectと同じインタフェースを実装するMock Objectを定義されてること
- SUTを実行する前にメソッド呼び出しの大部分またはすべての引数の値を予測できること
- SUTから期待するメソッド呼び出し（期待される引数を含む）とSUTに応答する必要がある戻り値を使用して、Mock Objectを構成してること
- 関節出力を検証できること。Mock ObjecがSUTの実行中に呼び出されると受け取った実際の引数を期待される引数と比較し、一致しなければテストに失敗する

**SUT**

```go
package mockobject

type User struct{}

type UserRepo interface {
	GetByID(id int) (*User, error)
}

func GetUser(id int, repo UserRepo) (*User, error) {
	return repo.GetByID(id)
}
```

**Test**

```go
package mockobject

import (
	"errors"
	"testing"

	"github.com/golang/mock/gomock"

	mock "./mock_mockobject"
)

func TestGetUser(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

	userErr := errors.New("user error")
	mockUserRepo := mock.NewMockUserRepo(ctrl)
	mockUserRepo.EXPECT().GetByID(10).Return(&User{}, nil)
	mockUserRepo.EXPECT().GetByID(20).Return(nil, userErr)

	// valid test
	u, err := GetUser(10, mockUserRepo)
	if u == nil {
		t.Fatal("expect User, but nil")
	}
	if err != nil {
		t.Fatal(err)
	}

	// invalid test
	_, err = GetUser(20, mockUserRepo)
	if err != userErr {
		t.Fatal(err)
	}
}
```

### Fake Object

Fake Objectは、本物の依存コンポーネントと同じI/Fを持つ疑似コンポーネントを作成しSUTに注入するパターンで
SUTの間接的な入力と出力の検証以外の場合に利用する。

Stubと似てる印象を受けましたが、Stubはテスト時に依存したコンポーネントを差し替える用途で利用するのに対して
Fakeは本物の疑似コンポーネントなのでテスト以外にもローカル開発時の外部サービスと疑似接続の用途として利用したりと用途が広い。

特徴
- Fake Objectは、Test Stubの一種で代替可能な依存関係をSUTに注入する
- SUTとFake Object間で発生しうる処理を提供するだけ
- 間接出力を検証する必要ない
- Fake Objectからの戻り値はハードコードされているか、テストによって設定される
- DBや外部サービス、テストが困難または遅くなるような他のコンポーネントに依存する場合に利用する

**SUT**
```go
package fakeobject

type User struct{}

func GetUser(id int, repo UserRepo) (*User, error) {
	return repo.GetByID(id)
}

type UserRepo interface {
	GetByID(id int) (*User, error)
}

type UserRepoImpl struct{}

func (r *UserRepoImpl) GetByID(id int) (*User, error) {
	return &User{}, nil
}

type FakeUserRepoImpl struct {
	GetByIDFunc func(id int) (*User, error)
}

func (r *FakeUserRepoImpl) GetByID(id int) (*User, error) {
	if r.GetByIDFunc != nil {
		return r.GetByIDFunc(id)
	}
	return &User{}, nil
}
```

**Test**
```go
package fakeobject

import (
	"errors"
	"testing"
)

func TestGetUser(t *testing.T) {
	repo := &FakeUserRepoImple{}

	// valid test
	u, err := GetUser(10, repo)
	if u == nil {
		t.Fatal("expect User, but nil")
	}
	if err != nil {
		t.Fatal(err)
	}

	// invalid test
	userErr := errors.New("user error")
	repo.GetByIDFunc = func(id int) (*User, error) {
		return nil, userErr
	}
	_, err = GetUser(10, repo)
	if err != userErr {
		t.Fatal(err)
	}
}
```

## 参考

http://goyoki.hatenablog.com/entry/20120301/1330608789
