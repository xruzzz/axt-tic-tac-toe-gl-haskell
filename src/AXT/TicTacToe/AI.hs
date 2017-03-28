{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.AI
    (
        getRandomPCstep
    )where
import AXT.TicTacToe.Types as GT (CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..))
import System.Random(mkStdGen, randomR)
{-
getPCstep ∷ GameField → GameLevel → GameField
getPCstep (F [s1, s2, s3]) l = do
    case l of
        0 → let
                sg1 = mkStdGen 42
                fp = map findFreePos [s1, s2, s3]
                fp0 = fp !! 0
                fp1 = fp !! 1
                fp2 = fp !! 2
                n = if (length fp0)>0 then (head $ map fst fp0) else
                        if (length fp1)>0 then (head $ map fst fp1) else
                            if (length fp2)>0 then (head $ map fst fp2) else 4
                fch s fd = if (length fd) > 0 then (replaceNth n 'O' s) else s
                
                in (F [s1, s2, s3]) -- (F (map fch [s1, s2,s3]))
        1 → undefined
-}
getRandomPCstep ∷ [CoorOnField] → CoorOnField
getRandomPCstep xs =
    let
        l = length xs
        (x, sg) = randomR (0, l-1) (mkStdGen l)
    in (xs !! x)
getRandomPCstep [] = undefined
