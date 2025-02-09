title: 公開鍵暗号
date: 2020-11-01 00:00
tags:
- Go
- Security

---

暗号化技術の勉強で前回は、[共通鍵](https://blog.fujimisakari.com/aes-key/) をやったので公開鍵暗号をやりました。

## 公開鍵暗号

暗号化と復号に別々の鍵（公開鍵、秘密鍵）で行う暗号方式です。暗号化を公開鍵で行い、復号を秘密鍵で行なわれます。暗号化アルゴリズムにはRSAや楕円曲線（Elliptic Curve）暗号などがあります。

RSAの鍵長サイズは1,024から4,096ビッドで、サイズは256の倍数でなければなりません。
鍵長が大きくなるほど暗号強度が向上しますが、データ処理に時間と負荷がかかるので安全性と処理負荷を考慮したうえで、鍵長を決めることが大切です。現在鍵長は2048ビット以上が推奨されています。

共通鍵暗号と公開鍵暗号のデータ処理時間に特徴があり、共通鍵暗号は処理が早く、公開鍵暗号は処理が遅いです。
そのため、データ通信なのでは共通鍵暗号でデータを暗号化し、公開鍵で共通鍵を暗号化するハイブリット方式により安全性と高速化を実現しています。

```go
privateKey, _ := rsa.GenerateKey(rand.Reader, 2048)
publicKey := &privateKey.PublicKey

// Encrypt
plainText := []byte("fizzbuzzfizzbuzz...")
cipherText, err := rsa.EncryptPKCS1v15(rand.Reader, publicKey, plainText)
if err != nil {
	fmt.Printf("Err: %s\n", err)
	return
}
fmt.Printf("Cipher text: %x\n", cipherText)

// Decrypt
decryptedText, err := rsa.DecryptPKCS1v15(rand.Reader, privateKey, cipherText)
if err != nil {
	fmt.Printf("Err: %s\n", err)
	return
}
fmt.Printf("Decrypted text: %s\n", decryptedText)
```

## 参考URL

- https://ja.wikipedia.org/wiki/RSA%E6%9A%97%E5%8F%B7
- https://deeeet.com/writing/2015/11/10/go-crypto/
