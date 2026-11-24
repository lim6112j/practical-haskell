# practical-haskell

Functor / Applicative / Monad / Arrow 이후의 어려운 주제를 연습하기 위한 저장소.

## 연습 파일 (구현은 비어 있음)

각 파일에는 **목표와 힌트만** 적혀 있고, 구현은 `undefined` 로 비어 있다.
직접 채워 넣으면서 공부하는 용도. `main` 의 `print` 옆 주석이 기대값이다.

예외: Ex12 만 `src/Ex12UrlShortener.hs` 가 완성 구현이고,
`src/Ex12UrlShortenerAnswer.hs` 가 `undefined` 로 비어 있는 연습 파일이다.

| 명령 | 파일 | 주제 |
| --- | --- | --- |
| `stack run -- ex01-foldable-traversable` | `src/Ex01FoldableTraversable.hs` | Foldable / Traversable 인스턴스, Monoid, Alternative |
| `stack run -- ex02-monad-transformers` | `src/Ex02MonadTransformers.hs` | StateT / ExceptT / ReaderT 스택, `lift`, mtl 스타일 |
| `stack run -- ex03-gadt` | `src/Ex03Gadt.hs` | GADTs, 타입 안전 표현식, 존재 양화(existential) |
| `stack run -- ex04-laziness` | `src/Ex04Laziness.hs` | 게으름, thunk, 공간 누수, BangPatterns |
| `stack run -- ex05-st` | `src/Ex05ST.hs` | ST 모나드와 국소적 가변성, IO 와의 비교 |
| `stack run -- ex06-comonad` | `src/Ex06Comonad.hs` | Comonad (Monad의 쌍대): Stream/Zipper/Store, 셀룰러 오토마타 |
| `stack run -- ex07-free-monad` | `src/Ex07FreeMonad.hs` | Free 모나드: 효과를 데이터로, 인터프리터 분리 |
| `stack run -- ex08-lens` | `src/Ex08Lens.hs` | Profunctor, van Laarhoven Lens/Traversal, Iso |
| `stack run -- ex09-recursion-schemes` | `src/Ex09RecursionSchemes.hs` | Fix, cata/ana/hylo/para, 재귀의 분해와 재조합 |
| `stack run -- ex10-type-families` | `src/Ex10TypeFamilies.hs` | DataKinds, 닫힌 타입 가족, TypeLits, 길이를 아는 Vec |
| `stack run -- ex11-generics` | `src/Ex11Generics.hs` | GHC.Generics 범용 인코더, DefaultSignatures, DerivingVia |
| `stack run -- ex12-url-shortener` | `src/Ex12UrlShortener.hs` | 실전: URL 단축 + 클릭 통계 서비스 (servant/warp/sqlite/aeson) |

```bash
stack build                 # 전체 빌드 (스캐폀드 상태 그대로도 컴파일된다)
stack run -- ex03-gadt       # 연습 3 실행
stack ghci                   # 대화형 실험
```

## 권장 순서

1. Ex01 — Foldable/Traversable (구조를 "한 줄로 펼치는" 일반화)
2. Ex02 — Monad Transformers (효과를 쌓고 벗기기)
3. Ex03 — GADTs (타입 안전한 DSL)
4. Ex04 — Laziness / strictness (디버깅 능력)
5. Ex05 — ST (순수 인터페이스 뒤의 가변성)
6. Ex06 — Comonad (Monad의 쌍대)
7. Ex07 — Free Monad (효과를 값으로)
8. Ex08 — Lens / Profunctor (광학의 원리)
9. Ex09 — Recursion Schemes (모든 재귀의 분해)
10. Ex10 — Type Families / DataKinds (타입 수준 프로그래밍)
11. Ex11 — Generics + DerivingVia (보일러플레이트 제거)
12. Ex12 — 실전 프로그램: 지금까지의 개념을 실제 프로그램에 쓴다 (입력 검증, sqlite, aeson, servant/warp)

Ex12 는 서버를 띄워 쓰는 프로그램이다. `src/Ex12UrlShortener.hs` 는 완성 구현이고,
`src/Ex12UrlShortenerAnswer.hs` 의 TODO 1~17 이 `undefined` 로 비어 있다. Answer 파일을
채워 보면서 `stack test` 로 순수 코어를 확인하고, TODO 10~17 을 채워 서버를 올려보자.

```bash
stack run -- ex12-url-shortener            # 기본 8080 포트 / ex12.db
stack run -- ex12-url-shortener -- --port 9000 --db my.db
# 다른 터미널에서
curl -s -X POST localhost:8080/api/links \
  -H 'content-type: application/json' -d '{"url":"haskell.org"}'
curl -si localhost:8080/r/<code>
curl -s localhost:8080/api/links/<code>
```

각 파일 상단의 TODO 번호 순서대로 채우면 된다.

## 기존 모듈

`src/` 의 다른 파일(`Functors.hs`, `Foldables.hs`, `TreeTutorial.hs` 등)은
이전에 작성한 예제/튜토리얼 모듈이다.
