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
import qualified Data.Aeson as A
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import GHC.Generics

import DB.Type

dbname = "db.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Fugou
  player Player
  to Pos
  from Pos Maybe
  koma Koma
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

