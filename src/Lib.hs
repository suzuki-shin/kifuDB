{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import DB

someFunc :: IO ()
someFunc = S.scotty 3000 $ do
  S.middleware L.logStdoutDev

  S.get "/fugou/:id" $ do
    fugouId <- S.param "id"
    fugou <- getFugou ((toSqlKey fugouId)::FugouId)
    S.json fugou

  S.post "/fugou" $ do
    f <- S.jsonData :: S.ActionM Fugou
    fugouId <- insertFugou f
    fugou <- getFugou fugouId
    S.json fugou

  S.get "/fugous" $ do
    fugous <- selectFugous
    S.json $ map entityVal fugous

  S.notFound $
    S.text "there is no such route."


-- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"fugouPlayer":1, "fugouTo":76, "fugouFrom":77, "fugouKoma":"Fu", "fugouNari":false}'  http://localhost:3000/fugou
