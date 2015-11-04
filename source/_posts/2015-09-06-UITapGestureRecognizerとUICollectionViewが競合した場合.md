title: UITapGestureRecognizerとUICollectionViewが競合した場合
date: 2015-09-06 14:20:29
tags:
- Objective-C

---

背景Viewにキーボードを隠すジェスチャーアクションを設定したら
UICollectionViewCellのタップアクションが効かなくなる問題が起きた。

現在のコード

```objective-c
- (void)viewDidLoad {
    [super viewDidLoad];

    // 背景をクリックしたら、キーボードを隠す
    UITapGestureRecognizer *gestureRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(closeSoftKeyboard)];
    [self.view addGestureRecognizer:gestureRecognizer];
}

- (void)closeSoftKeyboard {
    // キーボードを隠す処理
}

- (void)collectionView:(UICollectionView *)collectionView didSelectItemAtIndexPath:(NSIndexPath *)indexPath {
    // cellをタップした場合
}
```

原因は、タップアクションが呼ばれる前にジェスチャーアクションが先に呼ばれるためだった。
解決するには、ジェスチャーアクションを先に呼ぶ設定のcancelsTouchesInViewをNOにすれば解決できた

cancelsTouchesInViewを追加したコード

```objective-c
- (void)viewDidLoad {
    [super viewDidLoad];

    // 背景をクリックしたら、キーボードを隠す
    UITapGestureRecognizer *gestureRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(closeSoftKeyboard)];
    gestureRecognizer.cancelsTouchesInView = NO;
    [self.view addGestureRecognizer:gestureRecognizer];
}

- (void)closeSoftKeyboard {
    // キーボードを隠す処理
}

- (void)collectionView:(UICollectionView *)collectionView didSelectItemAtIndexPath:(NSIndexPath *)indexPath {
    // cellをタップした場合
}
```
