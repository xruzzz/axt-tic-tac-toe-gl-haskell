{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.Conversions
    (
        c3D,
        ce,
        cher,
        cv,
        toBSField,
        toGLFloat,
        re,
        numToC
    )
    where

import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.Ratio(Ratio)
import qualified Data.ByteString.Char8 as BSC8
import Graphics.UI.GLUT

import AXT.TicTacToe.Types

-- toBSField ∷ [String] -> GameField
toBSField = F . map BSC8.pack

toGLFloat ∷ RatI → GLfloat
toGLFloat x = fromRational (toRational x) ∷ GLfloat

re X = "x"
re O = "o"
re Z = " "

ce X = 'X'
ce O = 'O'
ce Z = ' '

cv 'X' = X
cv 'O' = O
cv ' ' = Z

c3D x y = C3D x y (0∷Int)

numToC '1' = c3D 0 2
numToC '2' = c3D 1 2
numToC '3' = c3D 2 2
numToC '4' = c3D 0 1
numToC '5' = c3D 1 1
numToC '6' = c3D 2 1
numToC '7' = c3D 0 0
numToC '8' = c3D 1 0
numToC '9' = c3D 2 0

cher X = O
cher O = X
