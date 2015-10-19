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
      ) where

import qualified Data.Aeson              as A
import           Data.Int                (Int64)
import qualified Data.Map.Strict         as M
import           Data.Text.Lazy          (Text)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
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
