# 연습 12 — 실전 프로그램: URL 단축 + 클릭 통계 서비스 (설계)

- 날짜: 2026-11-22
- 상태: **검증 완료** (`stack test` 8개 통과, HTTP 통합 시나리오 통과)
- 사용자 결정사항: 후보 A(URL 단축 + 통계 서비스)로 진행, 의존성 추가 자유, 만들기 전에 설계를 먼저 보여줄 것

## 1. 의도와 이유

Ex01~Ex11은 개념을 하나씩 연습하는 파일이다. 이번에는 **지금까지의 개념을 모두 쓰는, 실제로 돌아가는 프로그램**을
같은 방식(곳곳에 `undefined`)으로 만든다. 목적은 두 가지다.

1. 개념이 아니라 **프로그램**으로 연습한다: 파서 없는 세상에서 입력 검증, 영속성, 직렬화, 서버, 에러 전파가
   실제로 어떻게 맞물리는지 손으로 겪는다.
2. 프로그램이 **실제로 쓸 만해야** 한다: 로컬에서 띄워 두고 긴 URL을 짧게 만들고, 리다이렉트 횟수를 브라우저로 본다.

## 2. 목표 / 비목표 / 안티골

**목표**

- `src/Ex12UrlShortener.hs` (연습용, TODO 곳곳에 `undefined`) + `src/Ex12UrlShortenerAnswer.hs` (정답) 한 쌍.
- TODO 번호·난이도 티어·힌트·`main` 옆 기대값 주석 — Ex01~Ex11과 같은 문서 톤.
- `stack run -- ex12-url-shortener` 로 서버가 뜨고, curl로 단축/리다이렉트/통계가 동작한다.
- 순수 코어는 `test/Spec.hs` 의 hspec으로 검증한다.
- README 표와 권장 순서에 12번으로 등록된다.

**비목표**

- 사용자 인증/회원가입, 커스텀 별칭(내가 코드를 지정), 링크 만료·삭제, rate limiting.
- 프론트엔드 프레임워크, 마이그레이션 도구, Docker, 성능 튜닝, 동시성 제어(락/STM).
- HTTP 레벨 통합 테스트(네트워크가 섞이면 흔들린다).

**안티골**

- DB나 HTTP를 건드리는 테스트(순수 코어만 hspec).
- `undefined` 를 "나중에 채울 빈 파일" 수준으로 두지 않는다 — 각 TODO는 목표·힌트·난이도가 구체적이어야 한다.
- 새 의존성을 닥치는 대로 늘리지 않는다 (아래 5개 + time 으로 고정).

## 3. 제약

- macOS, stack, resolver **lts-22.44**. 검증 완료: `servant 0.20.2`, `servant-server 0.20.2`,
  `warp 3.3.31`, `sqlite-simple 0.4.19.0`, `aeson 2.1.2.1` (전부 스냅숏 안에 있음).
- sqlite-simple 은 시스템 sqlite3 C 라이브러리를 쓴다. macOS 기본 탑재라 추가 설치 불필요.
- 기존 빌드 옵션(`-Wall` 외 다수)과 Ex 파일 컨벤션을 그대로 쓴다. `undefined` 본문은 경고만 낼 뿐 빌드는 성공한다.
- servant + warp 첫 빌드는 수 분 걸린다(한 번만).

## 4. 대안 비교 (무엇을 검토했고 무엇을 골랐는지)

초기 후보는 A(URL 단축 + 통계 서비스) / B(깨진 링크 검사기 CLI) / C(스프레드시트 엔진)였다.
사용자가 A를 골랐다. A 안에서의 선택:

| 결정 | 대안 | 고른 것 | 이유 |
| --- | --- | --- | --- |
| 웹 프레임워크 | scotty, raw warp | **servant-server + warp** | 타입 수준 API 설계가 Ex10(타입 가족)의 자연스러운 확장이고, 핸들러 스택이 Ex02(모나드 변환기)와 직결된다. scotty는 쉽지만 타입 안전 이득이 없다 |
| 파일 구성 | 모듈 분리(`Api.hs`, `Store.hs`, ...) | **단일 파일 `src/Ex12UrlShortener.hs`** | Ex 시리즈는 "한 파일을 위에서부터 채운다"는 경험이다. Ex07이 이미 같은 형태로 200줄을 넘는다 |
| 저장소 | 메모리 Map, sqlite-simple | **sqlite-simple** | "실제로 쓸 만하다"의 핵심은 영속성. 메모리 버전은 선택 TODO(18)에서 Free 인터프리터로 다시 등장 |
| 짧은 코드 생성 | UUID, 카운터 | **base62 + 순수 LCG** | 순수 함수로 연습 거리가 크고, 코드가 짧으며, 왕복 검증(인코드→디코드)이 `main`의 기대값으로 바로 확인된다 |
| 시간 표현 | aeson의 시간 인스턴스 | **epoch `Int` 로 저장·직렬화** | aeson의 시간 타입 인스턴스 유무에 의존하지 않고, 변환 자체가 TODO(연습 거리)가 된다 |
| 직렬화 | 수동 `toJSON` | **DeriveAnyClass + `genericToJSON`/옵션** | Ex11(Generics/DerivingVia)을 그대로 재활용한다 |

## 5. 프로그램 개요

로컬에서 도는 URL 단축기. 동작:

- `POST /api/links` `{"url": "haskell.org"}` → `{code, url, shortUrl, createdAt}`
  - URL을 정규화(`https://` 보정, 호스트 소문자, http/https만 허용)하고, 이미 있는 URL이면 기존 코드를 돌려준다.
  - 없으면 시드에서 만든 8자리 base62 코드로 저장한다.
- `GET /r/:code` → 원래 URL로 **302** 리다이렉트. 코드가 없으면 404(JSON 에러). 리다이렉트할 때마다 클릭 1회 기록.
- `GET /api/links/:code` → `{code, url, createdAt, totalClicks, byDay, recentClicks}`.
- `GET /` → PlainText 사용법 페이지(어떤 curl을 치면 되는지).

DB는 `ex12.db`(기본값, `--db` 로 변경). 포트 기본 8080(`--port`).

**DB 스키마**

```sql
CREATE TABLE IF NOT EXISTS links (
  code       TEXT PRIMARY KEY,
  url        TEXT NOT NULL,
  created_at INTEGER NOT NULL          -- epoch 초
);
CREATE TABLE IF NOT EXISTS clicks (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  code       TEXT NOT NULL,
  clicked_at INTEGER NOT NULL           -- epoch 초
);
```

**API 타입 (연습 파일에 그대로 주어진다 — 타입은 `undefined` 로 비울 수 없으므로)**

```haskell
type ShortenerAPI =
       "api" :> "links" :> ReqBody '[JSON] NewLink    :> Post '[JSON] Link
  :<|> "api" :> "links" :> Capture "code" Text       :> Get  '[JSON] LinkStats
  :<|> "r"   :> Capture "code" Text                  :> Get  '[JSON] NoContent
  :<|> Get '[PlainText] Text
```

**타입**

```haskell
data NewLink  = NewLink  { newLinkUrl :: Text }              deriving (Generic)   -- FromJSON
data LinkRow  = LinkRow  { rowCode :: Text, rowUrl :: Text, rowCreatedAt :: Int }  -- DB 행
data Link     = Link     { linkCode :: Text, linkUrl :: Text, linkShortUrl :: Text, linkCreatedAt :: Int }
data LinkStats = LinkStats
  { statsCode :: Text, statsUrl :: Text, statsCreatedAt :: Int
  , statsTotalClicks :: Int, statsByDay :: [(Text, Int)], statsRecentClicks :: [Int] }
```

`byDay` 의 키는 `"YYYY-MM-DD"` 문자열 — ISO 형식이라 사전순 정렬이 곧 시간순이다(힌트로 쓴다).

## 6. 파일 구성과 컨벤션

- `src/Ex12UrlShortener.hs` — 연습 파일. 섹션 배너 + `TODO N.` + 목표/힌트 + `main`(기대값 주석).
  모듈 내보내기는 `main` **그리고 순수 코어 함수들**(TODO 1~6) — hspec이 임포트해야 하므로. (기존 Ex 파일이
  `main`만 내보내는 컨벤션에서 의도적으로 벗어나는 지점이다. 이유: DB/서버는 테스트하지 않고 순수 코어만 테스트한다.)
- `src/Ex12UrlShortenerAnswer.hs` — 같은 파일에 구현이 채워진 정답. 선택 TODO(18)는 정답에서도 비워 둔다.
- `package.yaml` — 실행파일 추가:

  ```yaml
  ex12-url-shortener:
    main:                Ex12UrlShortener.hs
    source-dirs:         src
    other-modules:       []
    dependencies:
    - aeson
    - servant
    - servant-server
    - sqlite-simple
    - text
    - time
    - warp
    ghc-options: [ -threaded, -rtsopts, -with-rtsopts=-N, -main-is, Ex12UrlShortener ]
  ```

  이 의존성들은 **전역 `dependencies` 목록에도 추가**한다(`text` 포함 — `Data.Text` 를 직접 쓰므로).
  hpack은 import를 스캔하지 않기 때문에, `src/` 직하부 파일은 라이브러리가 자동 노출하는데, 그 모듈이
  servant를 import하면 라이브러리의 build-depends 에도 servant가 필요하다. 전역에 넣으면 다른 실행파일도
  "사용 가능" 목록에만 추가될 뿐 실제로 컴파일되지는 않는다(한 번 받아두는 비용만 든다).
- `README.md` — 표에 `ex12-url-shortener` 행 추가, 권장 순서에 12 추가("지금까지의 개념을 실제 프로그램에").
- `test/Spec.hs` — 순수 코어 테스트 추가(아래 9장).

연습 파일에서 구현된 채로 주어지는 것: `main`(인자 처리 + 순수 코어 점검 print + 서버 기동),
`httpError`(에러 응답 생성), `argValue`(인자 파싱). 나머지는 전부 TODO.

필요 언어 확장: `DataKinds`, `TypeOperators`, `OverloadedStrings`, `DeriveGeneric`, `DeriveAnyClass`,
`ScopedTypeVariables`(Proxy 주석용).

## 7. TODO 설계

난이도 티어는 Ex01 톤으로 표시한다. 시그니처는 확정, 힌트 문구는 빌더가 쓴다(10장).

### 티어 1 — 순수 코어 (Ex01 스타일, `main`의 기대값으로 검증)

| # | 대상 | 시그니처(요지) | 관련 Ex |
| --- | --- | --- | --- |
| 1 | base62 인코딩 | `base62Alphabet :: Text`, `base62Encode :: Word64 -> Text`, `base62Decode :: Text -> Maybe Word64` | — |
| 2 | URL 정규화/검증 | `normalizeUrl :: Text -> Either Text Text` (앞뒤 공백 제거, 스킴 없으면 `https://` 붙임, 호스트 소문자, http/https만, 빈 호스트 거부) | — |
| 3 | 시드 전진 | `lcgNext :: Word64 -> Word64` (상수 1664525 / 1013904223) | — |
| 4 | 코드 생성 | `genCode :: Word64 -> Text` (시드 → 8자리 base62, 짧으면 앞을 `0`으로 패딩) | 1, 3 |
| 5 | 일별 집계 | `countByDay :: [UTCTime] -> [(Text, Int)]` (ISO 날짜 키, 오름차순) | 01(Map/Foldable) |
| 6 | 사용법 페이지 | `renderUsage :: Int -> Text -> Text` (포트, 베이스 URL) | — |

### 티어 2 — 직렬화 (Ex11 재활용)

| # | 대상 | 시그니처(요지) | 관련 Ex |
| --- | --- | --- | --- |
| 7 | 요청 본문 | `instance FromJSON NewLink` (DeriveAnyClass) | 11 |
| 8 | 링크 응답 | `instance ToJSON Link` — `genericToJSON` + `fieldLabelModifier`(레코드 접두사 `link`/`stats` 를 떼고 camelCase 유지: `linkShortUrl` → `shortUrl`) | 11 |
| 9 | 통계 응답 | `instance ToJSON LinkStats` | 11 |

### 티어 3 — 저장소 (sqlite-simple)

| # | 대상 | 시그니처(요지) | 관련 Ex |
| --- | --- | --- | --- |
| 10 | 연결/스키마 | `openDb :: FilePath -> IO Connection` (`CREATE TABLE IF NOT EXISTS` 두 개) | — |
| 11 | 링크 읽기/쓰기 | `instance FromRow LinkRow`, `instance ToRow LinkRow`, `insertLink`, `findLinkByCode`, `findLinkByUrl` | — |
| 12 | 클릭 기록/조회 | `insertClick`, `clicksFor`, `toEpoch :: UTCTime -> Int`, `fromEpoch :: Int -> UTCTime` | — |
| 13 | 데모 데이터 | `seedSample :: Connection -> IO ()` (링크가 비어 있을 때만: 링크 2개 + 과거 클릭 몇 개) | — |

### 티어 4 — 서버 (servant + warp)

| # | 대상 | 시그니처(요지) | 관련 Ex |
| --- | --- | --- | --- |
| 14 | 단축 핸들러 | `linkHandler :: Connection -> Int -> NewLink -> Handler Link` (정규화 실패 400, 중복 URL 이면 기존 것 반환, 아니면 코드 생성 후 저장) | 02, 03 |
| 15 | 리다이렉트 핸들러 | `redirectHandler :: Connection -> Text -> Handler NoContent` (없으면 404 JSON, 있으면 클릭 기록 + `err302` + `Location` 헤더) | 02 |
| 16 | 통계 핸들러 | `statsHandler :: Connection -> Text -> Handler LinkStats` (`clicksFor` → `countByDay`) | 01, 02 |
| 17 | 조립 | `serverT`, `app` (`serve (Proxy :: Proxy ShortenerAPI)`). `main`·`httpError`·`argValue` 는 연습 파일에 구현된 채로 주어진다 | — |

### 선택 티어

| # | 대상 | 내용 | 관련 Ex |
| --- | --- | --- | --- |
| 18 | Free 모나드 코어 | 주석으로만 제시되는 도전 과제: `data StoreF a` + 인터프리터 두 개(sqlite / 순수 `Map`). 코드 스캐폴드는 없다 | 07 |

## 8. 검증 방법

```bash
stack build
stack test                                  # 순수 코어 hspec
stack run -- ex12-url-shortener              # 기본 8080 / ex12.db (서버는 블로킹)
```

다른 터미널에서:

```bash
curl -s localhost:8080/
curl -s -X POST localhost:8080/api/links -H 'content-type: application/json' -d '{"url":"haskell.org"}'
curl -si localhost:8080/r/<code>             # 302 + Location
curl -si localhost:8080/r/nope               # 404 JSON
curl -s localhost:8080/api/links/<code>      # 통계 (여러 번 리다이렉트 후 totalClicks 증가)
sqlite3 ex12.db 'select * from links;'       # DB 직접 확인
```

`main` 은 서버를 띄우기 전에 순수 코어 점검을 먼저 찍는다(Ex01 방식):

```haskell
print (base62Encode 0)                    -- "0"
print (base62Encode 62)                   -- "10"
print (base62Decode "10")                 -- Just 62
print (normalizeUrl "haskell.org")        -- Right "https://haskell.org"
print (normalizeUrl "ftp://x.com")        -- Left "..."
print (T.length (genCode 42))             -- 8
```

기대값은 주석으로만 적고, 값을 직접 확인해 보게 한다.

## 9. 테스트 범위

`test/Spec.hs` 에 추가하는 것:

- base62 인코딩/왕복 (`base62Decode (base62Encode n) == Just n`, 경계 0/61/62/2^48)
- `normalizeUrl` 케이스 (스킴 없음, 대문자 호스트, 거부할 스킴, 공백)
- `genCode` 가 항상 8자리인지 (`T.length` 사용)
- `countByDay` (같은 날 여러 번, 여러 날 섞임, 빈 리스트)

DB/서버/HTTP는 테스트하지 않는다(3장 제약, 2장 안티골).

주의: 테스트는 **연습 파일**의 순수 코어를 임포트한다. 따라서 TODO 1~6 을 채우기 전에는
`stack test` 가 빨간 불이다(스캐폴드는 컴파일만 된다). README 에 그 점을 적어 둔다.

## 11. 빌더 체크 결과와 확정된 결정

설계 문서를 신선한 서브에이전트가 빌더 입장에서 검토했다. 나온 세 질문은 아래처럼 닫었다(모두 빌더의 몫):

1. **코드 시드는 어디서 오나 / 충돌은?** → 요청마다 현재 시각을 시드로 쓴다(무상태, 서버 재시작과 무관).
   코드가 이미 쓰였으면 `lcgNext` 로 한 칸 전진해 재시도(`freshCode`, 10 회 안에 실패 시 에러).
   시드를 상태로 들고 다가면 핸들러 시그니처가 더러워지고, 이 연습의 초점이 아니기 때문.
2. **JSON 키가 섹션 5 와 다르지 않나?** → 섹션 5 대로 camelCase 키를 쓴다.
   `fieldLabelModifier` 가 레코드 접두사(`link`, `stats`)만 떼고 camelCase 는 유지한다
   (`linkShortUrl` → `shortUrl`). `byDay` 는 기본 인스턴스 그대로 배열의 배열로 나간다
   (객체로 하고 싶으면 `ToJSON` 을 손으로 쓰는 도전).
3. **`seedSample` 은 언제 도나?** → 링크가 비어 있을 때만(`linkCount`). 매 실행마다 쌓이면 안 된다.

그 외 지적("테스트가 연습 파일을 임포트하므로 스캐폴에서 `stack test` 가 빨간 불")은 의도된 설계로 남겼다.

## 10. 빌더에게 남은 것

- 각 TODO의 목표/힌트 문구(시그니처와 티어·관련 Ex는 7장으로 확정).
- 에러 응답 JSON 형태(`{"error": "..."}` 로 통일하는 정도).
- 시드 데이터 구체적 내용, 코드 길이 8자, 기본 포트 8080/DB 파일명 확정은 이미 위에서 내린 호출이다.
- README 표 행 문구.
- 선택 TODO 18의 구체적 명령 집합.
