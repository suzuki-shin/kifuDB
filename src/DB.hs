{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB
    (
      Fugou(..)
    , FugouId
    , selectFugous
    , insertFugou
    , getFugou
    , toSqlKey
    , entityVal
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics


dbname = "db.sqlite"

-- data Player = P1 | P2 deriving (Show, Read, Eq)
-- derivePersistField "Player"

-- data Pos = Pos { x :: Int, y :: Int } deriving (Show, Read, Eq)
-- data Koma = Fu | Kn | Ou deriving (Show, Read, Eq)
-- derivePersistField "Koma"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Fugou
  player Int
  to Int
  from Int Maybe
  koma String
  nari Bool
  deriving Show Generic
|]

instance A.FromJSON Fugou
instance A.ToJSON Fugou

insertFugou :: (MonadBaseControl IO m, MonadIO m) => Fugou -> m (Key Fugou)
insertFugou f = runSqlite dbname $ do
  runMigration migrateAll
  insert f

selectFugous :: (MonadBaseControl IO m, MonadIO m) => m [Entity Fugou]
selectFugous = runSqlite dbname $ do
  runMigration migrateAll
  selectList ([] :: [Filter Fugou]) []

getFugou :: (MonadBaseControl IO m, MonadIO m) => FugouId -> m (Maybe Fugou)
getFugou fugouId = runSqlite dbname $ do
  runMigration migrateAll
  get fugouId

