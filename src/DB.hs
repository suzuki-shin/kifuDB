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
    , getFugou
    , getKifu
    , insertKifu
    , insertFugou
    , P.toSqlKey
    , P.entityVal
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
import           Database.Persist.TH
import qualified Data.Aeson as A
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import qualified Data.Text
import           Data.Text (Text)
import           GHC.Generics

import DB.Type

dbname = "db.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Kifu
  result Result Maybe
  info Text
  deriving Show Generic
Kyokumen
  kifuId KifuId
  orderId Int
  ban Ban
  mochiGoma1 [Koma]
  mochiGoma2 [Koma]
  KyokumenOrder kifuId orderId
  deriving Show Generic
Fugou
  kifuId KifuId
  orderId Int
  player Player
  to Pos
  from Pos Maybe
  koma Koma
  nari Bool
  FugouOrder kifuId orderId
  deriving Show Generic
|]

instance A.FromJSON Fugou
instance A.ToJSON Fugou

instance A.FromJSON Kifu
instance A.ToJSON Kifu

instance A.FromJSON Kyokumen
instance A.ToJSON Kyokumen


{-| insertFugou
insert Fugou and Kyokumen, and update Kifu.
-}
insertFugou :: MonadIO m => Fugou -> m (Maybe Fugou)
insertFugou f = runDB $ do
  let
    kifuId = fugouKifuId f
    orderId = fugouOrderId f
  kyokumen <- P.entityVal . fromJust <$> lastKyokumen kifuId orderId -- TODO: fromJustをちゃんとハンドリングする
  fugouId <- P.insert f
  fugou <- P.get fugouId
  P.insert $ Kyokumen kifuId orderId (nextBan (kyokumenBan kyokumen) f) [] []
  return fugou
  where
    lastKyokumen kid oid = P.getBy $ KyokumenOrder kid (oid - 1)


{-| insertKifu
insert Kifu and insert initial Kyokumen.
-}
insertKifu :: MonadIO m => Kifu -> m KifuId
insertKifu k = runDB $ do
  kifuId <- P.insert k
  P.insert $ initKyokumen kifuId
  return kifuId



getFugou fid = runDB $ P.get (P.toSqlKey fid :: FugouId)

getKifu kid = runDB $ P.get (P.toSqlKey kid :: KifuId)



-- runDB :: MonadIO m => P.SqlPersistT (Control.Monad.Logger.NoLoggingT (Control.Monad.Trans.Resource.Internal.ResourceT IO)) a -> m a
runDB :: MonadIO m => P.SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB query = liftIO $ P.runSqlite dbname $ do
  P.runMigration migrateAll
  query

initKyokumen :: KifuId -> Kyokumen
initKyokumen kifuId = Kyokumen kifuId 0 initBan [] []

-- TODO あとでちゃんとする
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
          , (Just (Masu Hi P2), posToKey $ Pos 2 2)
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

-- TODO あとでちゃんとする
nextBan :: Ban -> Fugou -> Ban
nextBan b f = Ban $ M.update (\masu -> Just (Just (Masu Ky P2))) (posToKey (Pos 1 2)) (ban b)

posToKey :: Pos -> String
posToKey (Pos x y) = show x ++ show y


selectFugous :: (MonadBaseControl IO m, MonadIO m) => m [P.Entity Fugou]
selectFugous = P.runSqlite dbname $
  P.selectList ([] :: [P.Filter Fugou]) []

-- get :: (P.PersistEntity val, MonadIO m, P.PersistEntityBackend val ~ P.SqlBackend)
--      => P.Key val -> m (Maybe val)
-- get = runDB . P.get

-- insert :: (P.PersistEntity val, MonadIO m, P.PersistEntityBackend val ~ P.SqlBackend)
--         => val -> m (P.Key val)
-- insert = runDB . P.insert
