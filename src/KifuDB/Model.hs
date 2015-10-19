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
    , P.toSqlKey
    , P.entityVal
    , drawBan
    ) where

import           Control.Applicative                   ((<$>))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.Aeson                            as A
import           Data.Foldable                         (foldl')
import qualified Data.Map.Strict                       as M
import           Data.Maybe                            (fromJust)
import           Data.Text.Lazy                        (Text, append)
import qualified Data.Text.Lazy
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

posToKey :: Pos -> String
posToKey (Pos x y) = show x ++ show y

posFromKey :: String -> Pos
posFromKey key = Pos (posInt `div` 10) (posInt `mod` 10)
  where
    posInt :: Int
    posInt = read key

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
