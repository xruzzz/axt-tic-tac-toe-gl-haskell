{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.Field
    (
        findFreePos,
        getFreePos
    ) where
import Data.List as DT (elemIndices)

import AXT.TicTacToe.Types as AT (Coor3DRI(..), CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..))
-- | Функция поиска свободных позиций в строке
-- findFreePos ∷ BSC8.ByteString → [Int]
-- findFreePos = (\xs → if xs == [] then id  else (map fst )). filter ((== ' ').snd) . zip [0..]
getFreePos ∷ GameField → [CoorOnField]
getFreePos (F ss) = concatMap (\(x, ss) -> strToIndices x ss) $zip [0..] ss

strToIndices y = map (\x -> (RangeCoor y, RangeCoor x)) . elemIndices ' '
{-
    let dd = map findFreePos [s1, s2, s3]
    in filter (\x -> length (snd x) > 0) . zip [0..] $ dd
                             
getFreePos2 ∷ [[Int]] → [SCoor]
getFreePos2 d = getFreePosHelper d 0 []
getFreePosHelper [] _ acc = acc
getFreePosHelper (x:xs) n acc =  getFreePosHelper xs (n+1) (acc ++ (map (\din -> C3D {x=n, y=din , z=0}) xs))
-}

findFreePos ∷ GameField → [[Int]]
findFreePos (F s) = map (elemIndices ' ') s
