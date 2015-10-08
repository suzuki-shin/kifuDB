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

  S.get "/person/:id" $ do
    personId <- S.param "id"
    person <- getPerson ((toSqlKey personId)::PersonId)
    S.json person

  S.post "/person" $ do
    p <- S.jsonData :: S.ActionM Person
    personId <- liftIO $ insertPerson p
    person <- getPerson personId
    S.json person

  S.get "/persons" $ do
    persons <- liftIO selectPersons
    S.json $ map entityVal persons

  S.notFound $
    S.text "there is no such route."


-- curl -v -H "Accept: application/json" -H "Content-type: application/json" -XPOST -d "{\"personName\" : \"hoge\", \"personAge\" : 10}" localhost:3000/person
