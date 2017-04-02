{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.Types
    (
        Coor3DRI(..),
        CoorOnField,
        Field(..),
        GameLevel(..),
        GameField(..),
        GameType(..),
        RangeCoor(..),
        RatI,
        SCoor(..),
        State(..),
        StepResult(..),
        initField,
        nums,
        toCoorOnField
    )
    where

import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.Ratio(Ratio)
-- import qualified Data.ByteString.Char8 as BSC8
-- (Номер строки, Номер символа)
type CoorOnField = (RangeCoor, RangeCoor)

toCoorOnField ∷ Int → Int → CoorOnField
toCoorOnField l s = (RangeCoor l, RangeCoor s)

data Field y = F [y] deriving (Eq, Show)
data State = O | X | Z deriving (Eq, Show)
data GameType = VS_USER | VS_PC deriving (Eq, Show)
data StepResult = OWIN | XWIN | GA | GM1 |GM2 | GM3 | WARNING1 | ERROR1 | END deriving (Eq, Show)

newtype KeyCoor = KeyCoor {getCC ∷ Char}

instance Bounded KeyCoor where
    minBound = KeyCoor '1'
    maxBound = KeyCoor '9'

newtype RangeCoor = RangeCoor {getRC ∷ Int}
    deriving (Eq, Show)

instance Bounded RangeCoor where
    minBound = RangeCoor 0
    maxBound = RangeCoor 2

newtype GameLevel = GameLevel {getLvl ∷Int}
    deriving (Eq, Ord, Show)

instance Enum GameLevel where
    toEnum x
            | x < getLvl minB = minB
            | x > getLvl maxB = maxB
            | otherwise = GameLevel x
        where 
            minB = minBound::GameLevel
            maxB = maxBound::GameLevel
    fromEnum ( GameLevel x )= x

instance Bounded GameLevel where
    minBound = GameLevel 0
    maxBound = GameLevel 4

type Coor3DRI = Coor3D (Ratio Int)
type SCoor = Coor3D Int


type RatI = Ratio Int
type GameField = Field String

initField = F ["   ","   ","   "]

-- | Пронумеровать клетки поля
nums = zip [0..] . map (zip [0..])
-- type GameField = Field BSC8.ByteString
-- initField = F $ map BSC8.pack ["   ","   ","   "]
