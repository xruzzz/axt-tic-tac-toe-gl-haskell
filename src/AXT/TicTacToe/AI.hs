{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.AI
    (
        getRandomPCstep,
        getAXTLevel2PCstep,
        getAXTLevel3PCstep
    )where
import AXT.TicTacToe.Actions as ATA (step)
import AXT.TicTacToe.Conversions as ATC(cher)
import AXT.TicTacToe.Field as FA(findFreePos, getFreePos)
import AXT.TicTacToe.Rules as ATR(isEnd)
import AXT.TicTacToe.Types as GT (CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..))
import Control.Monad(guard)
import System.Random(mkStdGen, randomR)
import Prelude.Unicode
import qualified Debug.Trace as DT (trace)
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

findLastWinSteps ∷ GameField → State → [CoorOnField]
findLastWinSteps fi@(F [l0, l1, l2]) s = do
    cop <- getFreePos fi
    let (w1, f1) = step s fi cop VS_PC
    guard (w1 == OWIN)
    return cop

-- Найти список ходов, при которых при следующем ходе будет выигрыш
findToWinSteps ∷ GameField → State → [CoorOnField]
findToWinSteps fi@(F [l0, l1, l2]) s = do
        let fp1 = getFreePos fi
        cop1 <- fp1
        let (w1, f1) = step s fi cop1 VS_PC
        cop2 <-  getFreePos f1 -- DT.trace (" внутри findStep coordinate 1 " ++ show cop1) $
        let (w2, f2) = step s f1 cop2 VS_PC
{-        case w of
            OWIN -> return cop1
            GA -> do
                 undefined
            _ -> undefined
            -}
        guard (w2 == OWIN) -- DT.trace (" внутри findStep coordinate 2 " ++ show cop2) $ 
        DT.trace (" внутри findStep после guard" ++ show fi ++"\n coordinate 1 " ++ show cop1 ++ "\n coordinate 2 " ++ show cop2 ++ "\n w1 " ++ show w1  ++ "\n w2 " ++ show w2 ) $ return cop1
        
getAXTLevel2PCstep ∷ GameField → CoorOnField → State → CoorOnField
getAXTLevel2PCstep fi@(F [l0, l1, l2]) co@(RangeCoor l, RangeCoor n) s = let
        fp = getFreePos fi
        lastSteps = findLastWinSteps fi s
    in if (lastSteps == []) then getRandomPCstep fp else head lastSteps

getAXTLevel3PCstep ∷ GameField → CoorOnField → State → CoorOnField
getAXTLevel3PCstep fi co@(RangeCoor l, RangeCoor n) s =
        let
            fp = getFreePos fi
            lastWinSteps = findLastWinSteps fi s
            toWinSteps = findToWinSteps fi s
        in
            if (lastWinSteps == []) then
                if (toWinSteps == []) then
                    (getRandomPCstep fp)
                else
                   head (DT.trace (" состояние " ++ show s ++"\nвход в findStep toWinSteps " ++ show toWinSteps) toWinSteps)
            else
                head (DT.trace (" состояние " ++ show s ++"\nвход в findStep " ++ show lastWinSteps) lastWinSteps)
