{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module KifuDB.Model.Type
    ( Player(..)
    , Koma(..)
    , Masu(..)
    , Pos(..)
    , Result(..)
    , Ban(..)
    , Fugou(..)
    , showKoma
    , posToKey
    , posFromKey
    , initBan
    , emptyBan
    , nextBan
    , drawBan
      ) where

import qualified Data.Aeson          as A
import           Data.Foldable       (foldl')
import           Data.Int            (Int64)
import qualified Data.Map.Strict     as M
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy
import           Database.Persist.TH (derivePersistField)
import           GHC.Generics

data Player = P1 | P2 deriving (Show, Read, Eq, Generic)
derivePersistField "Player"

instance A.FromJSON Player
instance A.ToJSON Player


data Koma = Fu | Ky | Ke | Gn | Kn | Hi | Kk | Ou
          | To | NKy | NKe | NGn | Ry | Um
          deriving (Show, Read, Eq, Generic)

derivePersistField "Koma"

showKoma :: Koma -> Text
showKoma Fu = "　歩"
showKoma Ky = "　香"
showKoma Ke = "　桂"
showKoma Gn = "　銀"
showKoma Kn = "　金"
showKoma Hi = "　飛"
showKoma Kk = "　角"
showKoma Ou = "　玉"
showKoma To = "　と"
showKoma NKy = "成香"
showKoma NKe = "成桂"
showKoma NGn = "成銀"
showKoma Ry = "　龍"
showKoma Um = "　馬"


instance A.FromJSON Koma
instance A.ToJSON Koma


data Pos = Pos { posX :: Int, posY :: Int } deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Pos"

instance A.FromJSON Pos
instance A.ToJSON Pos


data Result = Sennichite | P1Win | P2Win deriving (Show, Read, Eq, Generic)
derivePersistField "Result"

instance A.FromJSON Result
instance A.ToJSON Result


data Masu = Masu { masuKoma :: Koma, masuPlayer :: Player } deriving (Show, Read, Eq, Generic)
data Ban = Ban { ban :: M.Map String (Maybe Masu) } deriving (Show, Read, Eq, Generic) -- Mapのキーは PosからStringに変換する（じゃないとToJSON,FromJSONのインスタンスじゃないから）

derivePersistField "Ban"

instance A.FromJSON Masu
instance A.ToJSON Masu

instance A.FromJSON Ban
instance A.ToJSON Ban

data Fugou = Fugou
 { fugouKifuId   :: Int64
 , fugouOrderId  :: Int
 , fugouPlayer   :: Player
 , fugouTo       :: Pos
 , fugouFrom     :: Maybe Pos
 , fugouKoma     :: Koma
 , fugouNari     :: Bool
 , preKyokumenId :: Int64
} deriving (Show, Read, Eq, Generic)

instance A.FromJSON Fugou
instance A.ToJSON Fugou

derivePersistField "Fugou"

posToKey :: Pos -> String
posToKey (Pos x y) = show x ++ show y

posFromKey :: String -> Pos
posFromKey key = Pos (posInt `div` 10) (posInt `mod` 10)
  where
    posInt :: Int
    posInt = read key


initBan :: Ban
initBan = Ban $ foldl' (\b (masu, pos) -> M.adjust (const masu) pos b) (ban emptyBan)
          [ (Just (Masu Ky P2), posToKey $ Pos 1 1)
          , (Just (Masu Ke P2), posToKey $ Pos 2 1)
          , (Just (Masu Gn P2), posToKey $ Pos 3 1)
          , (Just (Masu Kn P2), posToKey $ Pos 4 1)
          , (Just (Masu Ou P2), posToKey $ Pos 5 1)
          , (Just (Masu Kn P2), posToKey $ Pos 6 1)
          , (Just (Masu Gn P2), posToKey $ Pos 7 1)
          , (Just (Masu Ke P2), posToKey $ Pos 8 1)
          , (Just (Masu Ky P2), posToKey $ Pos 9 1)
          , (Just (Masu Kk P2), posToKey $ Pos 2 2)
          , (Just (Masu Hi P2), posToKey $ Pos 8 2)
          , (Just (Masu Fu P2), posToKey $ Pos 1 3)
          , (Just (Masu Fu P2), posToKey $ Pos 2 3)
          , (Just (Masu Fu P2), posToKey $ Pos 3 3)
          , (Just (Masu Fu P2), posToKey $ Pos 4 3)
          , (Just (Masu Fu P2), posToKey $ Pos 5 3)
          , (Just (Masu Fu P2), posToKey $ Pos 6 3)
          , (Just (Masu Fu P2), posToKey $ Pos 7 3)
          , (Just (Masu Fu P2), posToKey $ Pos 8 3)
          , (Just (Masu Fu P2), posToKey $ Pos 9 3)
          , (Just (Masu Fu P1), posToKey $ Pos 1 7)
          , (Just (Masu Fu P1), posToKey $ Pos 2 7)
          , (Just (Masu Fu P1), posToKey $ Pos 3 7)
          , (Just (Masu Fu P1), posToKey $ Pos 4 7)
          , (Just (Masu Fu P1), posToKey $ Pos 5 7)
          , (Just (Masu Fu P1), posToKey $ Pos 6 7)
          , (Just (Masu Fu P1), posToKey $ Pos 7 7)
          , (Just (Masu Fu P1), posToKey $ Pos 8 7)
          , (Just (Masu Fu P1), posToKey $ Pos 9 7)
          , (Just (Masu Hi P1), posToKey $ Pos 2 8)
          , (Just (Masu Kk P1), posToKey $ Pos 8 8)
          , (Just (Masu Ky P1), posToKey $ Pos 1 9)
          , (Just (Masu Ke P1), posToKey $ Pos 2 9)
          , (Just (Masu Gn P1), posToKey $ Pos 3 9)
          , (Just (Masu Kn P1), posToKey $ Pos 4 9)
          , (Just (Masu Ou P1), posToKey $ Pos 5 9)
          , (Just (Masu Kn P1), posToKey $ Pos 6 9)
          , (Just (Masu Gn P1), posToKey $ Pos 7 9)
          , (Just (Masu Ke P1), posToKey $ Pos 8 9)
          , (Just (Masu Ky P1), posToKey $ Pos 9 9)
          ]


emptyBan :: Ban
emptyBan = Ban $ M.fromList $ zip (map posToKey possOnBan) (repeat Nothing)


possOnBan :: [Pos]
possOnBan = [Pos x y | x <- [1..9], y <- [1..9]]


nextBan :: Ban -> Fugou -> Ban
nextBan b f = changeTo (changeFrom b f) f
  where
    changeTo :: Ban -> Fugou -> Ban
    changeTo b f = Ban $ M.update (\masu -> Just (Just (Masu (fugouKoma f) (fugouPlayer f)))) (posToKey (fugouTo f)) (ban b)

    changeFrom :: Ban -> Fugou -> Ban
    changeFrom b f = case fugouFrom f of
      Nothing -> b
      Just pos -> Ban $ M.update (const (Just Nothing)) (posToKey pos) (ban b)

drawBan :: Ban -> Text
drawBan (Ban b) =
  Data.Text.Lazy.concat [
      "<html><body><table>"
    , Data.Text.Lazy.concat $ map ((\t -> Data.Text.Lazy.concat ["<tr>", t, "</tr>"]) . col) [1..9]
    , "</table></body></html>"
    ]
  where
    banList :: [((Int, Int), Text)]
    banList = map (\(k, mMas) -> ((posX $ posFromKey k, posY $ posFromKey k), mMasuToText mMas)) $ M.toList b

    mMasuToText :: Maybe Masu -> Text
    mMasuToText (Just (Masu koma player)) = Data.Text.Lazy.concat["<td>", showKoma koma, if player == P1 then "^" else "v", "</td>"]
    mMasuToText Nothing = "<td>　 </td>"

    col :: Int -> Text
    col y = Data.Text.Lazy.concat $ reverse $ map snd $ filter (\((_, y'), _) -> y' == y) banList
