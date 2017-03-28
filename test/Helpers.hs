{-# LANGUAGE UnicodeSyntax #-}
module Helpers
    (
        mapTests
    )
    where

import Test.HUnit (Test(..), assertEqual)
-- На вход подаются пары
-- fst - то что ожидается
-- snd - результат тестируемой функции
mapTests ∷ (Eq a, Show a) => String -> [(a,a)] -> [Test]
mapTests s = fmap (\(x,y) -> TestCase $ assertEqual ("\n MTest " ++ s) x y)
