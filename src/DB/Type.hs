{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell            #-}
module DB.Type
    ( Player(..)
    , Koma(..)
    , Pos(..)
      ) where

import qualified Data.Aeson as A
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

data Player = P1 | P2 deriving (Show, Read, Eq, Generic)
derivePersistField "Player"

instance A.FromJSON Player
instance A.ToJSON Player


data Koma = Fu | Hi | Kk | Kn | Ou deriving (Show, Read, Eq, Generic)
derivePersistField "Koma"

instance A.FromJSON Koma
instance A.ToJSON Koma


data Pos = Pos { x :: Int, y :: Int } deriving (Show, Read, Eq, Generic)
derivePersistField "Pos"

instance A.FromJSON Pos
instance A.ToJSON Pos
