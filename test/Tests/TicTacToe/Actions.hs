{-# LANGUAGE UnicodeSyntax #-}
module Tests.TicTacToe.Actions
    (
        spec
    )
    where
import Test.HUnit
import AXT.TicTacToe.Actions as GA(changeWorld)
import TestData as TD (fields)
import AXT.TicTacToe.Types as GT (CoorOnField, GameType(..), Field(F), State(..), StepResult(..), toCoorOnField)
import Helpers (mapTests)

spec = let
           gg = [   ((GA, F ["XOO","X  ","   "]), changeWorld GA (head fields) (toCoorOnField 1 0) X VS_USER),
                    ((GA, F ["XOO","X  ","   "]), changeWorld GA (head fields) (toCoorOnField 1 0) X VS_USER),
                    ((GA, F ["XOO","X  ","   "]), changeWorld GA (head fields) (toCoorOnField 1 0) X VS_USER)]
       in mapTests " changeWorld " gg
