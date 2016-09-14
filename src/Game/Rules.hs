{-# LANGUAGE UnicodeSyntax #-}

module Game.Rules
    (
        isEnd
    )
    where
import Prelude.Unicode
import Game.Conversions
import Game.Types(GameField, State(..), Winer(..),Field(F))
-- | Проверить выиграл ли кто
isEnd ∷ (Int, Int) → State → GameField → Winer
isEnd (l,n) s (F ss)
  | s ≡ X ∧ (eqToL ∨ eqToC ∨ diag1 ∨ diag2) = XW
  | s ≡ O ∧ (eqToL ∨ eqToC ∨ diag1 ∨ diag2) = OW
  | otherwise = GA

  where 
        diag1 = eqChar∧(((ss!! 0) !! 0) ≡ ((ss!! 1) !! 1))∧(((ss!! 0) !! 0) ≡ ((ss!! 2) !! 2))
        diag2 = eqChar∧(((ss!! 0) !! 2) ≡ ((ss!! 1) !! 1))∧(((ss!! 0) !! 2) ≡ ((ss!! 2) !! 0))
        eqToL = all (≡ce s) (ss !! l)
        eqToC = all (≡ce s) (map (!! n) ss)
        eqChar = ((ss!! 1) !! 1) ≡ ce s
