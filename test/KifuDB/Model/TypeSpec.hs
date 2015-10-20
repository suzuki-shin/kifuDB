{-# LANGUAGE OverloadedStrings    #-}
module KifuDB.Model.TypeSpec where

import Test.Hspec
import KifuDB.Model.Type

spec :: Spec
spec = do
  describe "KifuDB.Model.Type" $ do
    describe "showKoma" $ do
      it "showKomas Fu is 歩" $
        showKoma Fu `shouldBe` "歩"
      it "showKomas To is と" $
        showKoma To `shouldBe` "と"
      it "showKomas Ry is 龍" $
        showKoma Ry `shouldBe` "龍"
      it "showKomas Hi is 飛" $
        showKoma Hi `shouldBe` "飛"

    describe "initBan" $ do
      it "getAt (Pos 2 1) initBan" $
        getAt (Pos 2 1) initBan `shouldBe` Just (Just (Masu {masuKoma = Ke, masuPlayer = P2}))
      it "getAt (Pos 1 2) initBan" $
        getAt (Pos 1 2) initBan `shouldBe` Just Nothing
--       it "posToKey" $
--         posToKey (Pos 1 2) `shouldBe` "12"
--         it "emptyBan two paddings" $
--             emptyBan "cGFkZGluZyAgMg==" `shouldBe` "padding  2"
