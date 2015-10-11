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
    , get
    , insert
    , P.toSqlKey
    , P.entityVal
    , testAPI
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
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
  orderId Int
  ban Ban
  fugou Fugou
  mochiGoma1 [Koma]
  mochiGoma2 [Koma]
  KyokumenOrder kifuId orderId
  deriving Show Generic
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

instance A.FromJSON Kifu
instance A.ToJSON Kifu

instance A.FromJSON Kyokumen
instance A.ToJSON Kyokumen

runDB :: MonadIO m => P.SqlPersistT (Control.Monad.Logger.NoLoggingT (Control.Monad.Trans.Resource.Internal.ResourceT IO)) a -> m a
runDB query = liftIO $ P.runSqlite "db.sqlite" $ do
  P.runMigration migrateAll
  query

selectFugous :: (MonadBaseControl IO m, MonadIO m) => m [P.Entity Fugou]
selectFugous = P.runSqlite dbname $ do
  P.selectList ([] :: [P.Filter Fugou]) []

get :: (P.PersistEntity val, MonadIO m, P.PersistEntityBackend val ~ P.SqlBackend) => P.Key val -> m (Maybe val)
get = runDB . P.get

insert :: (P.PersistEntity val, MonadIO m, P.PersistEntityBackend val ~ P.SqlBackend) => val -> m (P.Key val)
insert = runDB . P.insert

testAPI :: (MonadBaseControl IO m, MonadIO m) => m (P.Key Fugou)
testAPI = P.runSqlite dbname $ do
  liftIO $ print "getKifu"
  kifu <- get ((P.toSqlKey 1)::KifuId)
  case kifu of
    Nothing -> do
      insert $ Kifu Nothing [kyokumen1]
      liftIO $ print "insert kifu"
    otherwise -> liftIO $ print kifu
  liftIO $ print "insertKifu"
  insert fu84

  where
    fu76 = Fugou P1 (Pos 7 6) (Just (Pos 7 7)) Fu False
    fu84 = Fugou P2 (Pos 8 4) (Just (Pos 8 3)) Fu False
    kyokumen1 :: Kyokumen
    kyokumen1 = Kyokumen ((P.toSqlKey 1)::KifuId) 1 (Ban (M.empty)) fu76 [] []
