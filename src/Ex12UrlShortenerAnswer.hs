{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Ex12UrlShortenerAnswer
Description : 연습 12 — 실전: URL 단축 + 클릭 통계 서비스 (연습용)

  이 파일은 '구현이 비어 있는' 연습 파일이다.
  아래 TODO가 표시된 함수/인스턴스의 `undefined`를 직접 채워 넣으면 된다.

  실행:
      stack run -- ex12-url-shortener

      # 다른 터미널에서
      curl -s localhost:8080/
      curl -s -X POST localhost:8080/api/links \
        -H 'content-type: application/json' -d '{"url":"haskell.org"}'
      curl -si localhost:8080/r/<code>
      curl -s localhost:8080/api/links/<code>

  이 연습의 특징:
    Ex01~Ex11 은 개념을 하나씩 연습했다. 이번에는 개념이 아니라 *프로그램*이다.
    입력 검증, 영속성(sqlite), 직렬화(aeson), 서버(servant/warp), 에러 전파가
    한 덩어리로 맞물리는 모습을 손으로 겪는 것이 목적이다.

  구조:
    TODO 1~6   순수 코어 — main 의 print 옆 기대값으로 바로 검증한다
    TODO 7~9   JSON 직렬화 (aeson, Ex11 재활용)
    TODO 10~13 저장소 (sqlite-simple)
    TODO 14~17 서버 (servant + warp)
    TODO 18    (선택) 효과를 데이터로 — Free 모나드 코어

  순수 코어는 test/Spec.hs 의 hspec으로도 검증한다 (TODO 1~6 을 채운 뒤):
      stack test
-}

module Ex12UrlShortenerAnswer
  ( main
    -- * 순수 코어 (TODO 1~6, hspec 이 임포트한다)
  , base62Alphabet
  , base62Encode
  , base62Decode
  , normalizeUrl
  , lcgNext
  , genCode
  , countByDay
  , renderUsage
    -- * 서버 유틸
  , httpError
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), encode, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection, FromRow (..), ToRow (..))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.API
  ( Capture, Get, JSON, NoContent, PlainText, Post, ReqBody, (:>), (:<|>) )
import Servant.Server (Application, Handler, ServerError (..), ServerT)
import System.Environment (getArgs)

-- ---------------------------------------------------------------------------
-- TODO 1. base62 인코딩 — "짧은 코드" 를 만드는 가장 단순한 방법
--
--   목표: 62진수 인코딩/디코딩.
--         n < 62 이면 알파벳에서 한 글자, 아니면 (n `div` 62) 을 다시 인코딩하고
--         (n `mod` 62) 자리를 뒤에 붙인다.
--         decode 는 encode 의 역함수 (왕복 검증용).
--
--   힌트:
--     * T.index base62Alphabet (fromIntegral n) 으로 글자를 얻는다.
--     * decode 는 왼쪽부터 acc * 62 + digit 를 누적한다.
--     * 알파벳에 없는 글자가 나오면 Nothing.
--     * 62 = 10 숫자 + 26 소문자 + 26 대문자. base64 와의 차이는 + / 를 뺀 것.
-- ---------------------------------------------------------------------------

base62Alphabet :: Text
base62Alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

base62Encode :: Word64 -> Text
base62Encode = undefined

base62Decode :: Text -> Maybe Word64
base62Decode = undefined

-- ---------------------------------------------------------------------------
-- TODO 2. URL 정규화와 검증
--
--   목표: 사용자가 뭘 치든 안전한 URL로 만든다.
--     * 앞뒤 공백 제거
--     * "://" 이 없으면 "https://" 를 붙인다
--     * 스킴은 http/https 만 허용 (나머지는 Left)
--     * 호스트 부분만 소문자로 (경로/쿼리는 그대로)
--     * 호스트가 비면 Left
--
--   힌트:
--     * T.breakOn "://" 으로 스킴을 떼어내고, T.stripPrefix 로 없음을 판단.
--     * Either Text Text — 실패 이유를 문자열로 돌려주면 핸들러에서 400 이 된다.
--     * "입력은 믿지 말고, 경계에서 한 번만 검증한다" 가 이 함수의 역할이다.
-- ---------------------------------------------------------------------------

normalizeUrl :: Text -> Either Text Text
normalizeUrl = undefined

noScheme :: Text -> Either Text Text
noScheme = undefined

lowerHost :: Text -> Text
lowerHost = undefined

-- ---------------------------------------------------------------------------
-- TODO 3. 순수 난수 전진 (LCG, Linear Congruential Generator)
--
--   목표: 시드 하나로 다음 시드를 만드는 순수 함수.
--
--   힌트:
--     * lcgNext n = 1664525 * n + 1013904223  (Numerical Recipes 의 상수)
--     * 같은 시드 → 같은 값 (재현 가능). 암호학적 난수가 아니다.
--     * 왜 순수 함수인가? 시드만 넘기면 결과가 정해지므로 테스트가 쉽고,
--       IO 없이도 "난수처럼 보이는" 값들을 만들 수 있다.
-- ---------------------------------------------------------------------------

lcgNext :: Word64 -> Word64
lcgNext = undefined

-- ---------------------------------------------------------------------------
-- TODO 4. 코드 생성
--
--   목표: 시드에서 8자리 짧은 코드를 만든다.
--
--   힌트:
--     * genCode = T.take 8 . T.justifyRight 8 '0' . base62Encode . lcgNext
--       (base62Encode 결과가 8자리보다 짧을 수 있으므로 왼쪽을 '0'으로 채운다)
--     * 62^8 ≒ 2×10^14 — 충분히 희소하다.
--     * 이 함수는 순수하다. "이미 쓰인 코드인가" 는 DB를 봐야 아는 일이므로
--       충돌 재시도는 핸들러(TODO 14)의 몫이다.
-- ---------------------------------------------------------------------------

genCode :: Word64 -> Text
genCode = undefined

-- ---------------------------------------------------------------------------
-- TODO 5. 일별 집계
--
--   목표: 클릭 시각 목록을 (날짜, 횟수) 쌍의 오름차순 목록으로 요약한다.
--
--   힌트:
--     * Map.fromListWith (+) 로 같은 날짜의 1 들을 합친다.
--     * Map.toAscList 로 정렬 — ISO "YYYY-MM-DD" 문자열은 사전순이 곧 시간순이다.
--     * 날짜 키: formatTime defaultTimeLocale "%Y-%m-%d" . utctDay
--       (Data.Time.Format, Data.Time.Clock)
--     * Ex01 의 Foldable/Monoid 감각: "구조를 접어서 요약한다".
-- ---------------------------------------------------------------------------

countByDay :: [UTCTime] -> [(Text, Int)]
countByDay = undefined

dayKey :: UTCTime -> Text
dayKey = undefined

-- ---------------------------------------------------------------------------
-- TODO 6. 사용법 페이지
--
--   목표: 루트 페이지에 보여줄 PlainText 를 만든다 (순수 함수).
--
--   힌트:
--     * T.intercalate "\n" [...] 한 줄로 끝난다.
--     * 어떤 curl 을 치면 되는지 그대로 적어라 — 사용자가 열었을 때
--       다음 행동이 보여야 한다.
-- ---------------------------------------------------------------------------

renderUsage :: Int -> Text -> Text
renderUsage = undefined

-- ---------------------------------------------------------------------------
-- TODO 7. 요청 본문 파싱 (aeson)
--
--   목표: NewLink 을 JSON 요청 본문으로 받을 수 있게 하라.
--
--   힌트:
--     * DeriveAnyClass 를 쓰면 deriving (Generic, FromJSON) 한 줄로 끝난다 (Ex11).
--     * 필드가 하나뿐이라 generic 파서가 그대로다.
-- ---------------------------------------------------------------------------

data NewLink = NewLink {newLinkUrl :: Text}
  deriving (Generic, Show)

instance FromJSON NewLink where
  parseJSON = undefined

-- ---------------------------------------------------------------------------
-- TODO 8. 링크 응답 직렬화 (aeson)
--
--   목표: Link 를 JSON 으로 내보내되, 필드명이 아니라 API 키로 나가게 하라.
--         linkCode → "code", linkShortUrl → "shortUrl" (섹션 5의 응답 형태)
--
--   힌트:
--     * genericToJSON defaultOptions { fieldLabelModifier = ... }
--     * fieldLabelModifier 는 Haskell 레코드 필드명을 JSON 키로 바꾸는 함수.
--     * 접두사("link")를 떼고 첫 글자를 소문자로: stripFieldPrefix / lowerFirst.
--     * 왜 레코드 필드에 접두사를 붙였나? Link 와 LinkStats 가 모두 code/url 을
--       가져야 하므로 같은 이름을 피하려고 (DuplicateRecordFields 를 안 쓰려고).
-- ---------------------------------------------------------------------------

data Link = Link
  { linkCode :: Text
  , linkUrl :: Text
  , linkShortUrl :: Text
  , linkCreatedAt :: Int
  }
  deriving (Generic, Show)

instance ToJSON Link where
  toJSON = undefined

-- ---------------------------------------------------------------------------
-- TODO 9. 통계 응답 직렬화 (aeson)
--
--   목표: LinkStats 를 JSON 으로 내보내라. statsCode → "code" 등.
--
--   힌트:
--     * TODO 8 과 같은 요령 (접두사 "stats").
--     * statsByDay :: [(Text, Int)] 은 기본 인스턴스로
--       [["2026-11-22",3], ...] 같은 "배열의 배열" 이 된다.
--       {"2026-11-22":3} 같은 객체로 하고 싶으면 ToJSON 을 손으로 써야 한다.
--     * 시간은 전부 epoch 초(Int)로 나간다 — aeson 의 시간 인스턴스에 기대지 않는다.
-- ---------------------------------------------------------------------------

data LinkStats = LinkStats
  { statsCode :: Text
  , statsUrl :: Text
  , statsCreatedAt :: Int
  , statsTotalClicks :: Int
  , statsByDay :: [(Text, Int)]
  , statsRecentClicks :: [Int]
  }
  deriving (Generic, Show)

instance ToJSON LinkStats where
  toJSON = undefined

-- ---------------------------------------------------------------------------
-- TODO 10. DB 연결과 스키마
--
--   목표: 데이터베이스 파일을 열고, 없으면 테이블을 만든다.
--
--   힌트:
--     * open path 으로 Connection 을 얻는다 (없으면 파일을 만든다).
--     * execute_ conn "CREATE TABLE IF NOT EXISTS ..." — idempotent 해야
--       매 실행마다 안전하게 호출할 수 있다.
--     * 스키마는 파일 상단 주석의 두 테이블.
--     * linkCount 는 seedSample(TODO 13)이 "이미 데이터가 있는가" 를 보는 데 쓴다.
-- ---------------------------------------------------------------------------

openDb :: FilePath -> IO Connection
openDb = undefined

linkCount :: Connection -> IO Int
linkCount = undefined

-- ---------------------------------------------------------------------------
-- TODO 11. 링크 읽기/쓰기
--
--   목표: DB 행 ↔ LinkRow 변환과 세 가지 질의.
--
--   힌트:
--     * FromRow / ToRow 인스턴스는 손으로 쓴다:
--         parseRow = LinkRow <$> field <*> field <*> field
--         toRow (LinkRow c u t) = toRow (c, u, t)   -- 튜플 인스턴스를 쓴다
--     * insert: execute conn "INSERT ... VALUES (?,?,?)" (code, url, now)
--     * 조회: query conn "SELECT ... WHERE code = ?" (Only code)
--       결과는 [LinkRow] 이므로 첫 행을 Maybe 로 — listToMaybe.
--     * 왜 LinkRow 과 Link 가 따로 있나? LinkRow 은 DB 모양(code/url/created_at)이고,
--       Link 는 API 모양(shortUrl 이 추가). 경계에서 한 번 변환한다(toLink).
-- ---------------------------------------------------------------------------

data LinkRow = LinkRow {rowCode :: Text, rowUrl :: Text, rowCreatedAt :: Int}
  deriving (Show)

instance FromRow LinkRow where
  fromRow = undefined

instance ToRow LinkRow where
  toRow = undefined

insertLink :: Connection -> Text -> Text -> Int -> IO ()
insertLink = undefined

findLinkByCode :: Connection -> Text -> IO (Maybe LinkRow)
findLinkByCode = undefined

findLinkByUrl :: Connection -> Text -> IO (Maybe LinkRow)
findLinkByUrl = undefined

-- ---------------------------------------------------------------------------
-- TODO 12. 클릭 기록/조회
--
--   목표: epoch 변환과 클릭 두 가지 질의.
--
--   힌트:
--     * toEpoch   = round . utcTimeToPOSIXSeconds
--     * fromEpoch = posixSecondsToUTCTime . fromInteger . toInteger
--     * 시간을 epoch Int 로 저장하는 이유: 타임존 애매함이 없고, aesan 에서
--       그대로 숫자로 나간다.
--     * clicksFor 는 최신 순(ORDER BY id DESC) — 통계의 "최근 클릭" 이 그 방향이다.
-- ---------------------------------------------------------------------------

toEpoch :: UTCTime -> Int
toEpoch = undefined

fromEpoch :: Int -> UTCTime
fromEpoch = undefined

insertClick :: Connection -> Text -> Int -> IO ()
insertClick = undefined

clicksFor :: Connection -> Text -> IO [UTCTime]
clicksFor = undefined

-- ---------------------------------------------------------------------------
-- TODO 13. 데모 데이터
--
--   목표: 첫 실행에 통계가 의미를 갖게 샘플 링크와 과거 클릭을 넣는다.
--
--   힌트:
--     * 링크가 이미 있으면 아무 것도 하지 마라 (linkCount) — 매 실행마다
--       쌓이면 안 된다.
--     * 과거 시각은 addUTCTime (-3600) now 처럼 만든다.
--     * 코드는 8자리 생성 코드와 겹치지 않게 "haskell", "stack" 같이 짧게.
-- ---------------------------------------------------------------------------

seedSample :: Connection -> IO ()
seedSample = undefined

-- ---------------------------------------------------------------------------
-- 서버: API 타입과 핸들러 (TODO 14~17)
--
--   API 타입은 연습 파일에 그대로 주어진다. 타입은 `undefined` 로 비울 수 없기
--   때문이다 — 채워 넣을 것은 핸들러와 조립이다.
-- ---------------------------------------------------------------------------

type ShortenerAPI =
  "api" :> "links" :> ReqBody '[JSON] NewLink :> Post '[JSON] Link
    :<|> "api" :> "links" :> Capture "code" Text :> Get '[JSON] LinkStats
    :<|> "r" :> Capture "code" Text :> Get '[JSON] NoContent
    :<|> Get '[PlainText] Text

-- ---------------------------------------------------------------------------
-- TODO 14. 단축 핸들러
--
--   목표: POST /api/links — URL을 받아 짧은 코드를 발급한다.
--
--   힌트:
--     * normalizeUrl 실패 → httpError 400.
--     * findLinkByUrl 로 이미 있는 URL이면 기존 코드를 그대로 돌려준다
--       (같은 URL 은 같은 코드 — 중복을 쌓지 않는다).
--     * 없으면: 현재 시각을 시드로 freshCode → insertLink → 방금 넣은 행을
--       다시 조회해서 Link 로 변환(toLink).
--     * freshCode: 코드가 이미 쓰였으면 lcgNext 로 한 칸 전진해 재시도.
--       10 번 안에 못 찾으면 실패(사실상 나오지 않는다).
--     * Handler 는 ExceptT ServerError IO 다. liftIO 로 DB 작업을 넣고,
--       throwError 로 HTTP 에러를 만든다 (Ex02).
-- ---------------------------------------------------------------------------

linkHandler :: Connection -> Int -> NewLink -> Handler Link
linkHandler = undefined

freshCode :: Connection -> Word64 -> IO Text
freshCode = undefined

toLink :: Int -> LinkRow -> Link
toLink = undefined

-- ---------------------------------------------------------------------------
-- TODO 15. 리다이렉트 핸들러
--
--   목표: GET /r/:code — 원래 URL로 302, 클릭을 1회 기록한다.
--
--   힌트:
--     * 코드가 없으면 httpError 404.
--     * 있으면 insertClick (현재 시각) 후:
--         throwError err302 { errHeaders = [("Location", url)] }
--       OverloadedStrings 덕에 "Location" 이 CI ByteString 로 해석된다.
--     * redirect 는 "성공" 이 아니라 "에러를 던져" 상태 코드를 만드는 것이다 —
--       throwError 가 상태 코드를 만드는 유일한 방법이다.
-- ---------------------------------------------------------------------------

redirectHandler :: Connection -> Text -> Handler NoContent
redirectHandler = undefined

-- ---------------------------------------------------------------------------
-- TODO 16. 통계 핸들러
--
--   목표: GET /api/links/:code — 클릭 통계를 돌려준다.
--
--   힌트:
--     * LinkRow 로 기본 정보, clicksFor 로 시각 목록.
--     * statsByDay = countByDay clicks (TODO 5), total = length clicks.
--     * recentClicks 는 최근 10 개 (take 10) — clicksFor 가 이미 최신 순이다.
--     * 없으면 httpError 404.
-- ---------------------------------------------------------------------------

statsHandler :: Connection -> Text -> Handler LinkStats
statsHandler = undefined

-- ---------------------------------------------------------------------------
-- TODO 17. 조립과 실행
--
--   목표: 핸들러를 API 타입에 꽂고, warp 로 띄운다.
--
--   힌트:
--     * serverT 는 :<|> 로 체인을 만든다. 순서는 API 타입의 순서와 같아야 한다.
--     * app = serve (Proxy :: Proxy ShortenerAPI) (serverT conn port)
--     * Proxy — "이 타입을 값으로" 들고 다니는 트릭. servant 의 타입 수준 DSL을
--       런타임에 넘기는 유일한 방법이다 (Ex10 과 연결).
--     * main 은 이미 구현되어 있다: 순수 코어 점검 print → DB 열기 →
--       seedSample → 사용법 출력 → run port.
-- ---------------------------------------------------------------------------

serverT :: Connection -> Int -> ServerT ShortenerAPI Handler
serverT = undefined

app :: Connection -> Int -> Application
app = undefined

-- ---------------------------------------------------------------------------
-- TODO 18 (선택). 효과를 데이터로 — Free 모나드 코어
--
--   지금은 핸들러가 sqlite 에 직접 붙어 있어서, DB 없이 핸들러 로직을 테스트할 수
--   없다. Free 모나드(Ex07)로 명령을 데이터로 만들면 인터프리터를 갈아끼울 수 있다:
--
--     data StoreF a
--       = AddLink Text Text Int (Text -> a)
--       | FindByCode Text (Maybe LinkRow -> a)
--       | FindByUrl Text (Maybe LinkRow -> a)
--       | AddClick Text Int a
--       | ClicksFor Text ([UTCTime] -> a)
--
--     type Store = Free StoreF
--
--   그리고 인터프리터 두 개:
--     * sqlite 인터프리터  — foldFree 로 StoreF (IO a) -> IO a
--     * 메모리 인터프리터  — State (Map Text LinkRow, Map Text [UTCTime])
--
--   그 다음 linkHandler 를 Store 위에서 돌리면, DB 없이 순수하게 핸들러 로직을
--   검증할 수 있다. 여기까지 가면 "효과 = 문법 + 대수 + 조합" 이 완성된다.
--   이 연습의 필수 범위는 아니다.
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- 공용 유틸 (연습 파일에도 그대로 주어진다)
-- ---------------------------------------------------------------------------

httpError :: Int -> Text -> ServerError
httpError code msg =
  ServerError
    { errHTTPCode = code
    , errReasonPhrase = T.unpack msg
    , errBody = encode (object ["error" .= msg])
    , errHeaders = [("Content-Type", "application/json")]
    }

argValue :: String -> [String] -> Maybe String
argValue flag args = lookup flag (pairs args)
  where
    pairs (k : v : rest) = (k, v) : pairs rest
    pairs _ = []

-- ---------------------------------------------------------------------------
-- 메인: TODO 1~6 을 채우면 각 print 의 결과가 주석과 같아야 한다.
-- 서버는 블로킹이므로 curl 은 다른 터미널에서 친다.
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let port = maybe 8080 read (argValue "--port" args)
      dbPath = fromMaybe "ex12.db" (argValue "--db" args)
  putStrLn "=== Ex12: URL 단축 + 클릭 통계 서비스 ==="
  putStrLn ""
  putStrLn "-- 순수 코어 점검 (TODO 1~6). 주석의 기대값과 같아야 한다 --"
  print (base62Encode 0) -- "0"
  print (base62Encode 61) -- "Z"
  print (base62Encode 62) -- "10"
  print (base62Decode "10") -- Just 62
  print (normalizeUrl "haskell.org") -- Right "https://haskell.org"
  print (normalizeUrl "ftp://nope") -- Left "only http/https are supported: ftp"
  print (T.length (genCode 42)) -- 8
  print (countByDay []) -- []
  putStrLn ""
  conn <- openDb dbPath
  seedSample conn
  let base = "localhost:" <> T.pack (show port)
  TIO.putStrLn (renderUsage port base)
  run port (app conn port)
