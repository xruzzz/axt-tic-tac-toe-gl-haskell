{-# LANGUAGE UnicodeSyntax #-}
module Game.Actions
    (
        changeLine
    )
    where

import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.Ratio(Ratio)
import Game.Types
import Prelude.Unicode

-- | Изменить строку
changeLine ∷ Int → State → String → String
changeLine n s ss = map (\(i,c) → if (n ≡ i ∧ (ss !! n) ≡ ' ') then (if s ≡ X then 'X' else 'O') else c) $ zip [0..] ss
