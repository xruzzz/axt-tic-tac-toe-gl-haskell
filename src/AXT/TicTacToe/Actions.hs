{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.Actions
    (
        step
    )
    where

-- import AXT.TicTacToe.AI as AI (getPCstep)
import Data.Ratio(Ratio)
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import AXT.TicTacToe.Rules (isEnd)
import AXT.TicTacToe.Types as GT (CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..))
import Prelude.Unicode

-- | Изменить строку
changeLine ∷ Int → State → String → String
changeLine n s ss = let isFree = (ss !! n) ≡ ' '
                    in
                        if isFree then
                            map (\(i,c) → if (n ≡ i ∧ isFree) then (if s ≡ X then 'X' else 'O') else c) $ zip [0..] ss
                        else
                            ss

step ∷ State → GameField → CoorOnField →  GameType → (StepResult, GameField)
step s (F [l0, l1, l2]) rls@(RangeCoor l, RangeCoor sy) t
  | l ≡ 0 = let cl= sChange l0
            in (if l0 ≠ cl then (isEnd rls s (F [cl, l1, l2])) else GM1, F [cl, l1, l2])
  | l ≡ 1 = let cl = sChange l1
            in (if l1 ≠ cl then (isEnd rls s (F [l0, cl, l2])) else GM2, F [l0, cl, l2])
  | l ≡ 2 = let cl = sChange l2
            in (if l2 ≠ cl then (isEnd rls s (F [l0, l1, cl])) else GM3, F [l0, l1, cl])
  where
      sChange = changeLine sy s
