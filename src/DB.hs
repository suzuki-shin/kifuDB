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

module DB
    ( Person(..)
    , PersonId
    , selectPersons
    , insertPerson
    , getPerson
    , toSqlKey
    , entityVal
    ) where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics

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

