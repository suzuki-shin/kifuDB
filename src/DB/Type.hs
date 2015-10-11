{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DB.Type
    ( Player(..)
    , Koma(..)
    , Pos(..)
    , Result(..)
    , Ban(..)
      ) where

import qualified Data.Aeson as A
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Map.Strict as M
import GHC.Generics

data Player = P1 | P2 deriving (Show, Read, Eq, Generic)
derivePersistField "Player"

instance A.FromJSON Player
instance A.ToJSON Player


data Koma = Fu | Ky | Ke | Gn | Kn | Hi | Kk | Ou
          | To | NKy | NKe | NGn | Ry | Um
          deriving (Show, Read, Eq, Generic)
derivePersistField "Koma"

instance A.FromJSON Koma
instance A.ToJSON Koma


data Pos = Pos { x :: Int, y :: Int } deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Pos"

instance A.FromJSON Pos
instance A.ToJSON Pos


data Result = Sennichite | P1Win | P2Win deriving (Show, Read, Eq, Generic)
derivePersistField "Result"

instance A.FromJSON Result
instance A.ToJSON Result


data Masu = Masu { masuKoma :: Koma, masuPlayer :: Player } deriving (Show, Read, Eq, Generic)
data Ban = Ban { ban :: M.Map String Masu } deriving (Show, Read, Eq, Generic) -- Mapのキーは PosからStringに変換する（じゃないとToJSON,FromJSONのインスタンスじゃないから） 

derivePersistField "Ban"

instance A.FromJSON Masu
instance A.ToJSON Masu

instance A.FromJSON Ban
instance A.ToJSON Ban

