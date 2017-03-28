{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.Actions
    (
        changeWorld
    )
    where

-- import AXT.TicTacToe.AI as AI (getPCstep)
import Data.Ratio(Ratio)
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import AXT.TicTacToe.Rules (isEnd)
import AXT.TicTacToe.Types as GT (CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..))
import Prelude.Unicode

-- | Изменить строку
changeLine ∷ Int → State → StepResult → String → (StepResult, String)
changeLine n s sr ss = let
                        isFree = (ss !! n) ≡ ' '
                    in if isFree then
                            (sr, map (\(i,c) → if (n ≡ i ∧ isFree) then (if s ≡ X then 'X' else 'O') else c) $ zip [0..] ss)
                        else
                            (WARNING1, ss)

changeWorld ∷ StepResult → GameField → CoorOnField → State → GameType → (StepResult, GameField)
changeWorld GA (F [l0, l1, l2]) rls@(RangeCoor l, RangeCoor sy) s t
  | l ≡ 0 = let (stR, cl) = sChange l0
            in (if l0 ≠ cl then (isEnd rls s (F [cl, l1, l2])) else GM1, F [cl, l1, l2])
  | l ≡ 1 = let (stR, cl) = sChange l1
            in (if l1 ≠ cl then (isEnd rls s (F [l0, cl, l2])) else GM2, F [l0, cl, l2])
  | l ≡ 2 = let (stR, cl) = sChange l2
            in (if l2 ≠ cl then (isEnd rls s (F [l0, l1, cl])) else GM3, F [l0, l1, cl])
  where
      sChange = changeLine sy s GA
changeWorld w (F [l0, l1, l2]) _ _ _ = (w, (F [l0, l1, l2]))
