{-# LANGUAGE UnicodeSyntax #-}
import Test.QuickCheck

import           Test.Hspec (hspec)
import Test.HUnit
-- import Test.Framework
-- import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Game.Rules (isEnd)
import Game.Types as GT (CoorOnField, Field(F), State(..), Winer(..),
    toCoorOnField)
import Prelude.Unicode
import Field.Algorithms as FA(findFreePos, getFreePos)
-- import Utils
-- prop_reverseReverse :: [Int] -> Bool
-- prop_reverseReverse xs = reverse (reverse xs) == xs
fields = fmap F [
            ["XOO", "   ", "   "],
            ["XXX", "   ", "   "],
            ["XXX", "OO ", "   "],
            ["XXX", "   ", "   "]]
            
-- isEndTest ∷ [
isEndTest = let
        t1 = isEnd (toCoorOnField 0 0) X $ fields !! 0
        t2 = isEnd (toCoorOnField 0 0) X $ fields !! 1
        t3 = isEnd (toCoorOnField 0 0) X $ fields !! 2
        t4 = isEnd (toCoorOnField 0 2) X $ fields !! 3
            in [TestCase $ assertEqual " isEnd Test 1 " True (t1 ≡ GA),TestCase $ assertEqual " isEnd Test 2 " True (t2 ≡ XW), TestCase $ assertEqual " isEnd Test 2 " True (t3 ≡ XW)]

testGetFreePos = let
                     gf1 = getFreePos (fields !! 0)
                     rlgf1 = [toCoorOnField 1 0, toCoorOnField 1 1, toCoorOnField 1 2, toCoorOnField 2 0, toCoorOnField 2 1, toCoorOnField 2 2]
                 in [TestCase $ assertEqual " Test 1 getFreePos" rlgf1 gf1]
main :: IO Counts
main = runTestTT . TestList $ isEndTest ++ testGetFreePos
