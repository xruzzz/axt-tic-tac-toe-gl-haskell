{-# LANGUAGE UnicodeSyntax #-}
import Test.QuickCheck

-- import Test.Framework
-- import Test.Framework.Providers.HUnit
import Control.Monad
import Data.Monoid
import AXT.TicTacToe.Field as FA(findFreePos, getFreePos)
import AXT.TicTacToe.Rules (isEnd)
import AXT.TicTacToe.Types as GT (CoorOnField, Field(F), State(..), StepResult(..), toCoorOnField)
import Prelude.Unicode
import TestData as TD (fields)
import Test.HUnit
import Test.Hspec (hspec)
import qualified Tests.TicTacToe.Actions as SGA (spec)
-- import Utils
-- prop_reverseReverse :: [Int] -> Bool
-- prop_reverseReverse xs = reverse (reverse xs) == xs
import Helpers (mapTests)

isEndTest = mapTests " isEnd " $ [  (XWIN       , isEnd (toCoorOnField 2 0) O $ F ["XO ", "XO ", "   "]),
                                    (GA         , isEnd (toCoorOnField 1 2) O $ F ["XOX", "   ", "   "]),
                                    (WARNING1   , isEnd (toCoorOnField 0 2) O $ F ["XOX", "   ", "   "]),
                                    (ERROR1     , isEnd (toCoorOnField 0 0) X $ F ["XOO", "   ", "   "]),
                                    (ERROR1     , isEnd (toCoorOnField 0 0) X $ F ["XXX", "   ", "   "]),
                                    (ERROR1     , isEnd (toCoorOnField 0 2) X $ F ["XXX", "   ", "   "])]

testGetFreePos = let
                     gf1 = getFreePos (head fields)
                     rlgf1 = [toCoorOnField 1 0, toCoorOnField 1 1, toCoorOnField 1 2, toCoorOnField 2 0, toCoorOnField 2 1, toCoorOnField 2 2]
                 in [TestCase $ assertEqual " Test 1 getFreePos" rlgf1 gf1]
main :: IO Counts
main = runTestTT . TestList $ isEndTest ++ testGetFreePos ++ SGA.spec
