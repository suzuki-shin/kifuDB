{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib
    ( someFunc
    ) where

import           Control.Applicative                  ((<$>))
import           Control.Monad.IO.Class               (liftIO)
import qualified Network.Wai.Middleware.RequestLogger as L
import qualified Web.Scotty                           as S
import           Data.Maybe (fromJust)
import qualified DB

someFunc :: IO ()
someFunc = S.scotty 3000 $ do
  S.middleware L.logStdoutDev

  -- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"fugouNari":false,"fugouPlayer":"P1","fugouFrom":{"x":7,"y":7},"fugouKifuId":1,"fugouTo":{"x":7,"y":6},"fugouKoma":"Fu", "fugouKifuId":1, "fugouOrderId":1, "preKyokumenId":1}'  http://localhost:3000/fugou
  S.post "/fugou" $ do
    f <- S.jsonData :: S.ActionM DB.Fugou
    kyokumenId <- DB.insertKyokumen f
    S.json kyokumenId

  -- curl localhost:3000/kifu/1
  S.get "/kifu/:id" $ do
    kifuId <- S.param "id"
    kifu <- DB.getKifu kifuId
    S.json kifu

  -- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"kifuResult":null, "kifuInfo":"test"}'  http://localhost:3000/game/start
  S.post "/game/start" $ do
    k <- S.jsonData :: S.ActionM DB.Kifu
    x <- DB.insertKifu k
    liftIO $ print x

  S.get "/ban/:id" $ do
    kyokumenId <- S.param "id"
    liftIO $ print "xxx/ban1"
    liftIO $ print kyokumenId
    liftIO $ print "xxx/ban2"
    mKyokumen <- DB.getKyokumen kyokumenId
    liftIO $ print mKyokumen
    case mKyokumen of
      Just kyokumen ->
--         liftIO $ print kyokumen
        S.text $ DB.drawBan (DB.kyokumenBan kyokumen)
--         S.json $ DB.drawBan (DB.kyokumenBan kyokumen)
--       Nothing -> S.json ("hoge" :: String)

  S.notFound $
    S.text "there is no such route."


