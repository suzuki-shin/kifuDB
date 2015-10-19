{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module KifuDB
    ( run
    ) where

import           Control.Applicative                  ((<$>))
import           Control.Monad.IO.Class               (liftIO)
import           Data.Maybe                           (fromJust)
import qualified KifuDB.Model as Model
import qualified Network.Wai.Middleware.RequestLogger as L
import qualified Web.Scotty                           as S

run :: IO ()
run = S.scotty 3000 $ do
  S.middleware L.logStdoutDev

  -- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"fugouNari":false,"fugouPlayer":"P1","fugouFrom":{"posX":7,"posY":7},"fugouKifuId":1,"fugouTo":{"posX":7,"posY":6},"fugouKoma":"Fu", "fugouKifuId":1, "fugouOrderId":1, "preKyokumenId":1}'  http://localhost:3000/fugou
  S.post "/fugou" $ do
    f <- S.jsonData :: S.ActionM Model.Fugou
    kyokumenId <- Model.insertKyokumen f
    S.json kyokumenId

  -- curl localhost:3000/kifu/1
  S.get "/kifu/:id" $ do
    kifuId <- S.param "id"
    kifu <- Model.getKifu kifuId
    S.json kifu

  -- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"kifuResult":null, "kifuInfo":"test"}'  http://localhost:3000/game/start
  S.post "/game/start" $ do
    k <- S.jsonData :: S.ActionM Model.Kifu
    x <- Model.insertKifu k
    liftIO $ print x

  S.get "/ban/:id" $ do
    kyokumenId <- S.param "id"
    mKyokumen <- Model.getKyokumen kyokumenId
    case mKyokumen of
      Just kyokumen -> S.html $ Model.drawBan (Model.kyokumenBan kyokumen)
      Nothing -> S.text "error"

  S.notFound $
    S.text "there is no such route."
