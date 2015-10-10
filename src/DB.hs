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
{-# LANGUAGE TypeSynonymInstances #-}

module DB
    ( Fugou(..)
    , FugouId
    , Kifu(..)
    , KifuId
    , selectFugous
    , insertFugou
    , getFugou
    , getKifu
    , insertKifu
    , toSqlKey
    , entityVal
    , testAPI
    ) where

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Data.Tree as Tree
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text
import           Data.Text (Text)
import           GHC.Generics

import DB.Type

dbname = "db.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Kifu
  result Result Maybe
  kifu [Kyokumen]
  deriving Show Generic
Kyokumen
  kifuId KifuId
  ban Ban
  fugou Fugou
  mochiGoma1 [Koma]
  mochiGoma2 [Koma]
  deriving Show Generic
Fugou
  player Player
  to Pos
  from Pos Maybe
  koma Koma
  nari Bool
  deriving Show Generic
|]


-- data FugouTree = FugouTree { getFugouTree :: Tree Fugou } deriving (Show, Read, Generic)
-- derivePersistField "FugouTree"

-- instance A.FromJSON FugouTree
-- instance A.ToJSON FugouTree

instance A.FromJSON Fugou
instance A.ToJSON Fugou

instance A.FromJSON Kifu
instance A.ToJSON Kifu

instance A.FromJSON Kyokumen
instance A.ToJSON Kyokumen

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

insertKifu :: (MonadBaseControl IO m, MonadIO m) => Kifu -> m (Key Kifu)
insertKifu k = runSqlite dbname $ do
  runMigration migrateAll
  insert k

getKifu :: (MonadBaseControl IO m, MonadIO m) => KifuId -> m (Maybe Kifu)
getKifu kifuId = runSqlite dbname $ do
  runMigration migrateAll
  get kifuId

testAPI :: (MonadBaseControl IO m, MonadIO m) => m (Key Fugou)
testAPI = runSqlite dbname $ do
  runMigration migrateAll
  liftIO $ print "getKifu"
  kifu <- getKifu ((toSqlKey 1)::KifuId)
  case kifu of
    Nothing -> do
      insertKifu $ Kifu Nothing [kyokumen1]
      liftIO $ print "insert kifu"
    otherwise -> liftIO $ print kifu
  liftIO $ print "insertKifu"
  insertFugou fu84

  where
    fu76 = Fugou P1 (Pos 7 6) (Just (Pos 7 7)) Fu False
    fu84 = Fugou P2 (Pos 8 4) (Just (Pos 8 3)) Fu False
    kyokumen1 :: Kyokumen
    kyokumen1 = Kyokumen ((toSqlKey 1)::KifuId) (Ban (M.empty)) fu76 [] []
