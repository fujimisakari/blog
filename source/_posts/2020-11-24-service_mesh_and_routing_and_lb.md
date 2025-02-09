title: k8sのServiceMeshとルーティング、LB
date: 2020-11-24 00:00
tags:
- Kubernetes

---

最近よく聞くk8sのServiceMesh。業務では使ってるけどいまいち理解が追いついてないのでルーティング方法とLBについて調べてみました。

## ServiceMeshとは

マイクロサービス間に張り巡らされたメッシュ状の通信やその経路を制御する考え方です。
OSSしてはIstio / Conduit / Linkerdなどが有名で、今回はIstioについて調べました。


## Istio

<img src="/image/k8s/istio.svg" width="220" />

https://istio.io/

Google / IBM / Lyftが中心となって開発しているOSSで、ServiceMeshのフレームワークになります。
MS間のトラフィック管理機能などを持つEnvoyと組み合わせてServiceMesh機能を実現しています。

いろいろ機能ありますがよく使われそうなものとしては次のようなものがありました。
- 負荷分散（v1へ90％、v2へ10%とか）
- 動的な通信制御（ルールベースでのルーティング）
- サービス間のアクセス制御
- Timeout制御
- Retry制御
- Rate Limit
- Circuit Breaker
- Fault Injection
- 通信メトリクス収集

### 仕組み

各PodにSidecarとしてEnvoyプロキシ(コンテナ)が埋め込み、トラフィックが流れていく際には同じPodに内包されているEnvoyが一度トラフィックを受けてからトラフィックを転送します。
API → Serviceと連動している場合だと次のようなフローとなります

<img src="/image/k8s/sidecar_envoy.png" width="720" height="420" />

### IstioとEnvoyの役割

IstioはControlPlane, EnvoyはDataPlaneで役割を持っています。
ControlPlaneはトラフィックコントロールの定義（Pilot）, メトリクスの収集先（Mixer）, MS間の認証設定（mTLS）。
DataPlaneが実際の実務処理を行っています。定義に基づいたトラフィックコントロール、メトリクス送信、MS間認証。

## ルーティングについて

Istioのルーティング（Virtual Service）について調べだすと、Envoy Proxyでルーティングやロードバランシングなどいろいろ登場してきて混乱するので整理しました。

### IstioのVirtualService
VirtualServiceはService(L4)へルールベースでルーティングするアプローチです。
このルールベースでルーティングはEnvoyを利用して実現されています。L7まで受けてからルーティングしてるので、HTTP Headerなどもルールとして利用できます。

<img src="/image/k8s/istio_virtual_service.png" width="720" height="420" />


```yaml
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: microservice-B
spec:
  hosts:
  - microservice-B.default.svc.cluster.local
  http:
  - match:
    name: dev1
    - headers:
        microservice-B-router:
          exact: dev1
    route:
    - destination:
        host: microservice-B-dev1.default.svc.cluster.local
  - match:
    name: dev2
    - headers:
        microservice-B-router:
          exact: dev2
    route:
    - destination:
        host: microservice-B-dev2.default.svc.cluster.local
  - match:
    name: dev3
    - headers:
        microservice-B-router:
          exact: dev3
    route:
    - destination:
        host: microservice-B-dev3.default.svc.cluster.local
  - name: default
    route:
    - destination:
        host: microservice-B.default.svc.cluster.local
```


### EnvoyのFront Proxy
EnvoyのFront proxyは次のように、Istioとは関係のないEnvoyのみでのProxy機能を利用するケースになります。
定義方法が異なりますが、機能的にはEnvoyを内包しているIstioのVirtualServiceと同様のものとなります。
使いどことしては、リクエストを受ける最前段Pod（GWなど）が考えられると思います。IstioのVirtualServiceは、upstreemにIstioが導入されている必要があり、リクエストを受ける最前段PodではVirtualServiceを利用することができないので。

<img src="/image/k8s/envoy_proxy.png" width="720" height="420" />

### EnvoyはLBもできる

gRPC通信（http2）などコネクションが貼られっぱなしになるプロトコルでは、常に同じPodに対してリクエスト送るのでLBが難しくなりますが、Envoyを中継させることで、upstreemにいるpodへのLBが容易になり相性が良いです。

### Headless ServiceでのLB方法
VirtualService(EnvoyのProxy)を使わずに、k8sのHeadless Service（内部DNS経由で各PodのIPが取得できる）+ gRPC Client Side LBを利用して、LBすることもできます。Headless Serviceを利用してUpStreemにいるすべてのPodとコネクションを貼り、gRPC Client Sideからリクエストに利用するコネクションをRound RobinしてLBします。

<img src="/image/k8s/headless_service_lb.png" width="720" height="420" />

## 参考書籍、URL

- [Kubernetes完全ガイド](https://www.amazon.co.jp/Kubernetes%E5%AE%8C%E5%85%A8%E3%82%AC%E3%82%A4%E3%83%89-%E7%AC%AC2%E7%89%88-Top-Gear-%E9%9D%92%E5%B1%B1/dp/4295009792)
- [CNDK2019 1年間のシステム運用を通して分かったIstioの嬉しさと活用における注意点](https://speakerdeck.com/ido_kara_deru/benefits-and-usage-notes-of-istio-based-on-our-system-operation-experience)
- [Kubernetes上でgRPCサービスを動かす](https://deeeet.com/writing/2018/03/30/kubernetes-grpc/)
- [Envoy Proxyに入門した](https://i-beam.org/2019/01/22/hello-envoy/)
