{-# LANGUAGE UnicodeSyntax #-}
module Game.Types
    (
        Coor3DRI(..),
        Field(..),
        GameLevel(..),
        GameField(..),
        GameType(..),
        RatI,
        SCoor(..),
        State(..),
        Winer(..),
        initField,
        nums
    )
    where

import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.Ratio(Ratio)
-- import qualified Data.ByteString.Char8 as BSC8


data Field y = F [y] deriving (Eq, Show)
data State = O | X | Z deriving (Eq, Show)
data GameType = VS_USER | VS_PC deriving (Eq, Show)
data Winer = OW | XW | GA | GM1 |GM2 |GM3 | END deriving (Eq, Show)

type Coor3DRI = Coor3D (Ratio Int)
type SCoor = Coor3D Int
type GameLevel = Int

type RatI = Ratio Int
type GameField = Field String

initField = F ["   ","   ","   "]

-- | Пронумеровать клетки поля
nums = zip [0..] . map (zip [0..])
-- type GameField = Field BSC8.ByteString
-- initField = F $ map BSC8.pack ["   ","   ","   "]
