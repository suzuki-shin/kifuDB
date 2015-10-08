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

module Lib
    ( someFunc
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import qualified Network.Wai.Middleware.RequestLogger as L
import qualified Web.Scotty as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name Text
  age Int
  deriving Show Generic
|]

instance A.FromJSON Person
instance A.ToJSON Person

selectPersons :: IO [Entity Person]
selectPersons = runSqlite "db.sqlite" $ do
    runMigration migrateAll
    selectList ([] :: [Filter Person]) []

insertPerson p = runSqlite "db.sqlite" $ do
    runMigration migrateAll
    insert p

getPerson personId = runSqlite "db.sqlite" $ do
    runMigration migrateAll
    get personId

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
