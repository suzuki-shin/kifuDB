{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class  (liftIO)
import qualified Network.Wai.Middleware.RequestLogger as L
import qualified Web.Scotty as S

import qualified DB

someFunc :: IO ()
someFunc = S.scotty 3000 $ do
  S.middleware L.logStdoutDev

  S.get "/fugou/:id" $ do
    fugouId <- S.param "id"
    fugou <- DB.getFugou fugouId
    S.json fugou

  S.post "/fugou" $ do
    f <- S.jsonData :: S.ActionM DB.Fugou
    fugou <- DB.insertFugou f
    S.json fugou

  S.get "/kifu/:id" $ do
    kifuId <- S.param "id"
    kifu <- DB.getKifu kifuId
--     kifu <- DB.runDB $ P.get (DB.toSqlKey kifuId :: DB.KifuId)
    S.json kifu

  S.post "/game/start" $ do
    k <- S.jsonData :: S.ActionM DB.Kifu
    x <- DB.insertKifu k
    liftIO $ print x

  S.notFound $
    S.text "there is no such route."


-- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"fugouNari":false,"fugouPlayer":"P1","fugouFrom":{"x":7,"y":7},"fugouKifuId":1,"fugouTo":{"x":7,"y":6},"fugouKoma":"Fu"}'  http://localhost:3000/fugou
