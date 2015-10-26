{-# LANGUAGE OverloadedStrings #-}
module KifuDB.Parser (
  parseKifu
  ) where

import           Codec.Binary.UTF8.String               (decodeString)
import           Codec.Text.IConv                       (convert)
import           Control.Applicative                    ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8             as BL8 (pack, readFile,
                                                                unpack,
                                                                writeFile)
import           Data.Char                              (digitToInt)
import           Data.List
import           KifuDB.Model.Type
import           Text.Parsec
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

lexer  = P.makeTokenParser emptyDef
parens = P.parens lexer

type Header = [String]
type Body = [KifuLine]

data Position = Position {col :: Int, row ::  Int} | Dou | Uchi deriving (Eq, Show)

data KifuLine = KifuLine {
    kifuLineTekazu  :: Int
  , kifuLineKoma    :: String
--   , kifuLineKoma :: Koma
  , kifuLineToPos   :: Position
  , kifuLineFromPos :: Position
  , kifuLineNari    :: String
  } | Tohryo deriving (Eq)

instance Show KifuLine where
  show (KifuLine tekazu koma to from nari) = show tekazu ++ " " ++ show koma ++ show to ++ show from ++ show nari
  show Tohryo = "投了"

data Kifu = Kifu {
    kifuHeader :: Header
  , kifuBody   :: Body
  } deriving (Show, Eq)

parseKifu s =
  case parse kifu "(kifu)" s of
    Left err -> error $ show err
    Right res -> res

kifu :: Parser Kifu
kifu = do
  hs <- try headers
  kls <- try kifuLines
  return $ Kifu hs kls

headers :: Parser [String]
headers = endBy header eol

header :: Parser String
header = (header_ "開始日時：")
         <|>
         (header_ "棋戦：")
         <|>
         (header_ "持ち時間：")
         <|>
         (header_ "先手：")
         <|>
         (header_ "後手：")
         <|>
         (many (noneOf "1234567890\n") >>= return)

header_ :: String -> Parser String
header_ h = do
  string h
  b <- many (noneOf "\n")
  return $ h ++ b

kifuLines :: Parser [KifuLine]
kifuLines = endBy1 kifuLine eol

-- | 棋譜の行parser
-- >>> parseTest kifLine "20 ３八馬(29)   ( 00:03/00:00:53)"
-- KifuLine {getNumber = "20", getAction = 38馬(29), getTime = Just "( 00:03/00:00:53)"}
-- >>> parseTest kifLine "31 １一角成(66)   ( 00:04/00:02:12)"
-- KifuLine {getNumber = "31", getAction = 11角成(66), getTime = Just "( 00:04/00:02:12)"}
-- >>> parseTest kifLine "115 投了"
-- KifuLine {getNumber = "115", getAction = 投了, getTime = Nothing}
kifuLine :: Parser KifuLine
kifuLine = do
  n <- read <$> many1 digit
  space
  (tohryo <|> action n)

action :: Int -> Parser KifuLine
action n = do
  to <- toPosition
  koma <- koma
  nari <- string "成" <|> string ""
  from <- fromPosition
  time <- many (noneOf "\n")
  return $ KifuLine n koma to from nari


-- | piece parser
-- >>> parseTest piece "歩"
-- "\27497"
-- >>> parseTest piece "成桂"
-- "\25104\26690"
koma :: Parser String
koma = do
  nari <- string "成" <|> string ""
  p <- foldl1' (<|>)  $ map string ["歩","香","桂","銀","金","飛","角","玉","龍","竜","馬"]
  return $ nari ++ p


-- | 移動後の位置 parser
-- >>> parseTest toPosition "８六"
-- 86
toPosition :: Parser Position
toPosition = toPos_ <|> dou

-- | 移動前の位置 parser
-- >>> parseTest fromPosition "(32)"
-- 32
-- >>> parseTest fromPosition "打"
-- 打
fromPosition :: Parser Position
fromPosition = fromPos_ <|> uchi

fromPos_ :: Parser Position
fromPos_ = do
  c:[r] <- parens $ many1 $ oneOf "123456789"
  return $ Position (digitToInt c) (digitToInt r)

toPos_ :: Parser Position
toPos_ = do
  col <- many1 $ oneOf "１２３４５６７８９"
  row <- many1 $ oneOf "一二三四五六七八九"
  return $ Position (toInt col) (toInt row)

-- | "同銀"とかの"同"をparseするparser
-- >>> parseTest dou "同"
-- 同
dou :: Parser Position
dou = do
  string "同"
  string "　" <|> string ""
  return Dou

uchi :: Parser Position
uchi = string "打" >> return Uchi

tohryo :: Parser KifuLine
tohryo = string "投了" >> return Tohryo


toInt :: String -> Int
toInt "１" = 1
toInt "２" = 2
toInt "３" = 3
toInt "４" = 4
toInt "５" = 5
toInt "６" = 6
toInt "７" = 7
toInt "８" = 8
toInt "９" = 9
toInt "一" = 1
toInt "二" = 2
toInt "三" = 3
toInt "四" = 4
toInt "五" = 5
toInt "六" = 6
toInt "七" = 7
toInt "八" = 8
toInt "九" = 9
toInt s = read s

eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"


readAndConvertKif :: FilePath -> IO Kifu
readAndConvertKif filePath = do
  sjisbs <- BL8.readFile filePath
  let utf8bs = convert "SJIS" "UTF-8" sjisbs
  return $ parseKifu $ decodeString $ BL8.unpack utf8bs
