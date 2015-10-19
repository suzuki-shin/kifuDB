{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module KifuDB.Model
    ( Fugou(..)
    , Kifu(..)
    , Kyokumen(..)
    , KifuId
    , getKifu
    , getKyokumen
    , insertKifu
    , insertKyokumen
    , drawBan
    ) where

import           Control.Applicative                   ((<$>))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.Aeson                            as A
import qualified Data.Map.Strict                       as M
import           Data.Maybe                            (fromJust)
import           Data.Text.Lazy                        (Text, append)
import qualified Database.Persist                      as P
import qualified Database.Persist.Sqlite               as P
import           Database.Persist.TH
import           GHC.Int                               (Int64)

import           KifuDB.Model.Type

dbname = "db.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Kifu json
  result Result Maybe
  info Text
  deriving Show
Kyokumen json
  kifuId KifuId
  orderId Int
  ban Ban
  fugou Fugou Maybe
  mochiGoma1 [Koma]
  mochiGoma2 [Koma]
  preId KyokumenId Maybe
  deriving Show
|]


{-| insertKyokumen
insert Kyokumen, and update Kifu.
-}
insertKyokumen :: MonadIO m => Fugou -> m KyokumenId
insertKyokumen f = runDB $ do
  let
    preId :: KyokumenId
    preId = P.toSqlKey (preKyokumenId f)
  kyokumen <- fromJust <$> P.get preId -- TODO: fromJustをちゃんとハンドリングする
  P.insert $
    Kyokumen
      (P.toSqlKey (fugouKifuId f))
      (fugouOrderId f)
      (nextBan (kyokumenBan kyokumen) f)
      (Just f)
      []
      []
      (Just preId)


{-| insertKifu
insert Kifu and insert initial Kyokumen.
-}
insertKifu :: MonadIO m => Kifu -> m KifuId
insertKifu k = runDB $ do
  kifuId <- P.insert k
  P.insert $ initKyokumen kifuId
  return kifuId

getKifu :: MonadIO m => Int64 -> m (Maybe Kifu)
getKifu kid = runDB $ P.get (P.toSqlKey kid :: KifuId)

getKyokumen :: MonadIO m => Int64 -> m (Maybe Kyokumen)
getKyokumen kid = runDB $ P.get (P.toSqlKey kid :: KyokumenId)


runDB :: MonadIO m => P.SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB query = liftIO $ P.runSqlite dbname $ do
  P.runMigration migrateAll
  query

initKyokumen :: KifuId -> Kyokumen
initKyokumen kifuId = Kyokumen kifuId 0 initBan Nothing [] [] Nothing
