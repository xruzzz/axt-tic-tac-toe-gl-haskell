{-# LANGUAGE UnicodeSyntax #-}

module AXT.TicTacToe.Rules
    (
        isEnd
    )
    where

import Prelude.Unicode
import AXT.TicTacToe.Conversions
import AXT.TicTacToe.Types(CoorOnField, GameField, State(..), StepResult(..), Field(F), RangeCoor(..))
-- | Проверить выиграл ли кто
isEnd ∷ CoorOnField → State → GameField → StepResult
isEnd (RangeCoor l, RangeCoor n) s (F ss)
  | s ≡ X ∧ (eqToL ∨ eqToC ∨ diag1 ∨ diag2) = XWIN
  | s ≡ O ∧ (eqToL ∨ eqToC ∨ diag1 ∨ diag2) = OWIN
  | otherwise = GA
  where 
        diag1 = eqChar∧(((ss!! 0) !! 0) ≡ ((ss!! 1) !! 1))∧(((ss!! 0) !! 0) ≡ ((ss!! 2) !! 2))
        diag2 = eqChar∧(((ss!! 0) !! 2) ≡ ((ss!! 1) !! 1))∧(((ss!! 0) !! 2) ≡ ((ss!! 2) !! 0))
        eqToL = all (≡ce s) (ss !! l)
        eqToC = all (≡ce s) (map (!! n) ss)
        eqChar = ((ss!! 1) !! 1) ≡ ce s
