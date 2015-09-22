module Network.Wai.TransSpec (spec) where

import Network.Wai.Trans

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Network.Wai.Trans"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
