{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad
import Data.IORef
import Prelude.Unicode
import TLib
import Graphics.UI.GLUT
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.Ratio

type Coor3DRI = Coor3D (Ratio Int)
type SCoor = Coor3D Int
data Field y = F [y] deriving Show
data State = O | X | Z deriving (Eq, Show)

xₘᵢₙ = negate xₘₐₓ
xₘₐₓ = 300∷Ratio Int
yₘᵢₙ = negate yₘₐₓ
yₘₐₓ = xₘₐₓ

nₓ = 3
ny = nₓ

stₓ = (xₘₐₓ - xₘᵢₙ) /nₓ
sty = (yₘₐₓ - yₘᵢₙ) /ny
offsetₓ = 50∷Ratio Int
offsety = 80∷Ratio Int

toGLFloat ∷ Ratio Int → GLfloat
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

numToC '1' = (C3D 2 0 (0∷Int))
numToC '2' = (C3D 2 1 (0∷Int))
numToC '3' = (C3D 2 2 (0∷Int))
numToC '4' = (C3D 1 0 (0∷Int))
numToC '5' = (C3D 1 1 (0∷Int))
numToC '6' = (C3D 1 2 (0∷Int))
numToC '7' = (C3D 0 0 (0∷Int))
numToC '8' = (C3D 0 1 (0∷Int))
numToC '9' = (C3D 0 2 (0∷Int))

changeWorld ∷ Field String → SCoor→ State → Field String
changeWorld (F [s1, s2, s3]) (C3D x y z) s
  | y == 0 = F [changeLine s1 x s, s2, s3]
  | y == 1 = F [s1, changeLine s2 x s, s3]
  | y == 2 = F [s1, s2, changeLine s3 x s]

changeLine ∷ String → Int → State → String
changeLine ss n s = map (\(x,y) → if n == x then (if s == X then 'X' else 'O') else y) numSt
  where numSt = zip [0..] ss

class F a where
    tron∷(a → b) → Bool

instance F State where
    tron d = True

func∷Int → Int
func b = 5

step = 100 -- для текста 0.6

changeState∷Char → State → Field String → IO ()
changeState  'q' _ _ = do putStrLn "The End"
changeState c s f = do
        line ← getChar
        putStrLn $ show f
        changeState line X (F ["   ", "   ", "   "])

cher X = O
cher O = X

keyboardMouse∷IORef GLfloat → IORef (GLfloat, GLfloat) → IORef (Field String)→ IORef State→ KeyboardMouseCallback
keyboardMouse a p ff ch key Down _ _ = do
  cx ← get ch
  ch $~! \x → cher x
  case key of
    (MouseButton LeftButton) → ff $~! (\x →changeWorld x (C3D 1 1 (0∷Int)) X)
    (Char ' ') → a $~! negate
    (Char '+') → a $~! (* 2)
    (Char '-') → a $~! (/ 2)
    (Char '1') → ff $~! (\x →changeWorld x (C3D 0 2 (0∷Int)) cx)
    (Char '2') → ff $~! (\x →changeWorld x (C3D 1 2 (0∷Int)) cx)
    (Char '3') → ff $~! (\x →changeWorld x (C3D 2 2 (0∷Int)) cx)
    (Char '4') → ff $~! (\x →changeWorld x (C3D 0 1 (0∷Int)) cx)
    (Char '5') → ff $~! (\x →changeWorld x (C3D 1 1 (0∷Int)) cx)
    (Char '6') → ff $~! (\x →changeWorld x (C3D 2 1 (0∷Int)) cx)
    (Char '7') → ff $~! (\x →changeWorld x (C3D 0 0 (0∷Int)) cx)
    (Char '8') → ff $~! (\x →changeWorld x (C3D 1 0 (0∷Int)) cx)
    (Char '9') → ff $~! (\x →changeWorld x (C3D 2 0 (0∷Int)) cx)
    (SpecialKey KeyLeft ) → p $~! \(x,y) → (x-0.1,y)
    (SpecialKey KeyRight) → p $~! \(x,y) → (x+0.1,y)
    (SpecialKey KeyUp   ) → p $~! \(x,y) → (x,y+0.1)
--    (SpecialKey KeyDown ) →  -- \(x,y) → (x,y-0.1)
    (SpecialKey KeyF1   ) → ff $~! (\x → F ["   ", "   ", "   "])
    _ → return ()
keyboardMouse _ _ _ _ _ _ _ _ = return ()

main∷IO ()
main = do
--    let fi = F ["   ", "   ", "   "]
    let width = 1280
    let height = 1024
    let orthoWidth = 40
    let orthoHeight = 30
    (_progName, _args) ← getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size width height
    createWindow "AXT GL - www.axi.su - xruzzzz@gmail.com"
    angle ← newIORef $ pi/2
    delta ← newIORef $ pi/360
    pos ← newIORef (0, 0)
    field ← newIORef (F ["   ", "   ", "   "])
    cheri ← newIORef X
    keyboardMouseCallback $= Just (keyboardMouse delta pos field cheri)
    idleCallback $= Just (idle field)
    displayCallback $= display angle field
    matrixMode $= Projection
    loadIdentity
    ortho2D (-340) 340 (-330) 330 -- (-5) 20
    matrixMode $= Modelview 0

    mainLoop

idle∷IORef (Field String) → IdleCallback
idle fs = do
--  d <- get delta
--  angle $~! (+ d)
--  (F fi) ← get fs
--  ds 
  postRedisplay Nothing

nums = zip [0..] . map (zip [0..])

showState ∷ Field String → IO ()
showState (F fs) = do
  let dd = nums fs
  forM_ dd $ \(i,ss) → do
--    putStrLn $ show i
--    raw X (C3D 0 0 (0∷Ratio Int))
--    line3D (C3D (stₓ/2) (-50) 0) (C3D 40 (stₓ/2) 0)
 
    forM_ ss $ \(j,ts) → do
      let sd = (cv ts)
      case sd of
           X → showR X (C3D j i (0∷Int))
           O → showR O (C3D j i (0∷Int))
           otherwise → return ()

display ∷ IORef GLfloat → IORef (Field String) → DisplayCallback
display ang fs = do
    clear [ColorBuffer, DepthBuffer]
    a ← get ang
    preservingMatrix $ do
--        scale 0.5 0.5 (0.5∷GLfloat)
        color $ Color3 (0.2∷GLfloat) 0.8 0.8
--        rotate a $ Vector3 1 0 0
        (F fi) ← get fs
        renderPrimitive Lines $ do
          field3D               
--      rotate a $ Vector3 0 0 1
--      translate $ Vector3 (-200∷GLfloat) 0 0 
--      rasterPos (Vertex2 0 0)
--      renderString Roman "Xк☺-✔+×"
--        scale 1.0 1.0 (1.0∷GLfloat)

          showState (F fi)
    swapBuffers
{-
showA∷Field String → IO ()
showA (F fi) = forM_ [0..2] $ \x → do
       forM_ [0..2] fi
       -}
field3D∷IO ()
field3D = forM_ [negate stₓ/2,stₓ/2] $ \x → do
       line3D (C3D x yₘᵢₙ 0) (C3D x yₘₐₓ 0)
       line3D (C3D xₘᵢₙ x 0) (C3D xₘₐₓ x 0)

xToRawX,yToRawY∷Ratio Int → GLfloat
xToRawX x = toGLFloat $ xₘᵢₙ + x

yToRawY y = toGLFloat $ yₘₐₓ - y

showS∷State → SCoor → IO ()
showS s (C3D x y z) = rawT s (C3D (xₘᵢₙ + (150 * x % 1)) (yₘₐₓ - (150 * y % 1)) 0)

showR∷State → SCoor → IO ()
showR s (C3D x y z) = raw s (C3D (xₘᵢₙ + (stₓ* (x % 1))) (yₘₐₓ - (sty * (y % 1))) 0)

raw∷State → Coor3DRI→IO ()
raw s (C3D x y z) =
      case s of
            X → do
              line3D (C3D (x₁) y₁ z) (C3D x₂ y₂ z)
              line3D (C3D x₁ y₂ z) (C3D x₂ y₁ z)
              line3D (C3D x₂ y₁ z) (C3D x₂ y₂ z)
              line3D (C3D x₁ y₁ z) (C3D x₁ y₂ z)
            O → do
              line3D (C3D x₁ y₁ z) (C3D x₂ y₁ z)
              line3D (C3D x₁ y₁ z) (C3D x₁ y₂ z)
              line3D (C3D x₁ y₂ z) (C3D x₂ y₂ z) 
              line3D (C3D x₂ y₁ z) (C3D x₂ y₂ z)
  where
    x₁ = x + offsetₓ
    x₂ = x₁ + step
    y₁ = y - offsety
    y₂ = y₁ - step

rawT ∷ State → Coor3DRI → IO ()
rawT s (C3D x y z) =
        do
              translate $ Vector3 (toGLFloat x₁) (toGLFloat y₁) 0
              renderString Roman $ re s
      where
        x₁ = x + offsetₓ
        y₁ = y - offsety

line3D∷Coor3DRI → Coor3DRI → IO ()
line3D beg end = do
        vertex (Vertex3 (toGLFloat (x beg)) (toGLFloat (y beg)) (toGLFloat (z beg)))
        vertex (Vertex3 (toGLFloat (x end)) (toGLFloat (y end)) (toGLFloat (z end)))
