{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad
import Data.IORef
import Prelude.Unicode
import TLib
import Graphics.UI.GLUT
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.Ratio
import Data.Typeable
type Coor3DRI = Coor3D (Ratio Int)
type SCoor = Coor3D Int
data Field y = F [y] deriving (Eq, Show)
data State = O | X | Z deriving (Eq, Show)
data Winer = OW | XW | GA | GM1 |GM2 |GM3 | END deriving (Eq, Show)
data GameType = VS1 | VS2 deriving (Eq, Show)
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

numToC '1' = (C3D 0 2 (0∷Int))
numToC '2' = (C3D 1 2 (0∷Int))
numToC '3' = (C3D 2 2 (0∷Int))
numToC '4' = (C3D 0 1 (0∷Int))
numToC '5' = (C3D 1 1 (0∷Int))
numToC '6' = (C3D 2 1 (0∷Int))
numToC '7' = (C3D 0 0 (0∷Int))
numToC '8' = (C3D 1 0 (0∷Int))
numToC '9' = (C3D 2 0 (0∷Int))

xsa ∷ (Int, Int) → State → Field String → Winer
xsa (l,n) s (F ss)
  | s ≡ X ∧ (eqToL ∨ eqToC ∨ diag1 ∨ diag2) = XW
  | s ≡ O ∧ (eqToL ∨ eqToC ∨ diag1 ∨ diag2) = OW
  | otherwise = GA
  where diag1 = eqChar∧(((ss!! 0) !! 0) ≡ ((ss!! 1) !! 1))∧(((ss!! 0) !! 0) ≡ ((ss!! 2) !! 2))
        diag2 = eqChar∧(((ss!! 0) !! 2) ≡ ((ss!! 1) !! 1))∧(((ss!! 0) !! 2) ≡ ((ss!! 2) !! 0))
        eqToL = all (≡ce s) (ss !! l)
        eqToC = all (≡ce s) (map (!! n) ss)
        eqChar = ((ss!! 1) !! 1) ≡ ce s

changeWorld ∷ Winer → Field String → SCoor→ State →GameType → (Winer, Field String)
changeWorld GA (F [s1, s2, s3]) (C3D x y z) s t
  | y ≡ 0 = let cl = sChange s1
            in (if s1 ≠ cl then (xsa (y,x) s (F [cl, s2, s3])) else GM1, F [cl, s2, s3])
  | y ≡ 1 = let cl = sChange s2
            in (if s2 ≠ cl then (xsa (y,x) s (F [s1, cl, s3])) else GM2, F [s1, cl, s3])
  | y ≡ 2 = let cl = sChange s3
            in (if s3 ≠ cl then (xsa (y,x) s (F [s1, s2, cl])) else GM3, F [s1, s2, cl])
  where sChange = changeLine x s
changeWorld w (F [s1, s2, s3]) (C3D x y z) _ _ = (w,(F [s1, s2, s3]))
  
changeLine ∷ Int → State → String → String
changeLine n s ss = map (\(i,c) → if (n ≡ i ∧ (ss !! n) ≡ ' ') then (if s ≡ X then 'X' else 'O') else c) numSt
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

keyboardMouse∷IORef GLfloat → IORef (GLfloat, GLfloat) → IORef (Winer, Field String)→ IORef State→IORef GameType→ KeyboardMouseCallback
keyboardMouse a p gg ch gtr key Down _ _ = do
  (w,ffs1) ← get gg
  cx ← get ch
  gt ← get gtr
  case key of           -- 
--    (MouseButton LeftButton) → ff $~! (\x →changeWorld x (C3D 1 1 (0∷Int)) X)
    (Char ' ') → a $~! negate
    (Char '+') → a $~! (* 2)
    (Char '-') → a $~! (/ 2)
    (Char '1') → gg $~! ccWw w (0, 2) cx gt
    (Char '2') → gg $~! ccWw w (1, 2) cx gt
    (Char '3') → gg $~! ccWw w (2, 2) cx gt
    (Char '4') → gg $~! ccWw w (0, 1) cx gt
    (Char '5') → gg $~! ccWw w (1, 1) cx gt
    (Char '6') → gg $~! ccWw w (2, 1) cx gt
    (Char '7') → gg $~! ccWw w (0, 0) cx gt
    (Char '8') → gg $~! ccWw w (1, 0) cx gt
    (Char '9') → gg $~! ccWw w (2, 0) cx gt
    (SpecialKey KeyLeft ) → p $~! \(x,y) → (x-0.1,y)
    (SpecialKey KeyRight) → p $~! \(x,y) → (x+0.1,y)
    (SpecialKey KeyUp   ) → p $~! \(x,y) → (x,y+0.1)
--    (SpecialKey KeyDown ) →  -- \(x,y) → (x,y-0.1)
    (SpecialKey KeyF1   ) → gg $~! (\(_,_)→(GA, F ["   ", "   ", "   "]))     -- новая игра 1 игрок
    (SpecialKey KeyF2   ) → gg $~! (\(_,_)→(GA, F ["   ", "   ", "   "]))     -- новая игра 2 игрока
    _ → return ()
  (w,ffs2) ← get gg
  print w
  if ffs1 ≠ ffs2 then do
      case gt of
           VS1 → putStrLn "ход компьютера"
           VS2 → ch $~! \x → cher x
           _ → return ()
      print $ ffs2
      case w of
           XW → putStrLn " X выиграли!"
           OW → putStrLn " O выиграли!"
           END → putStrLn " Ничья! Нажмите F1"
           _ → return ()
--      checkLine ffs2
                 else return ()
  where ccWw wl (x, y) c g = \(w,f) → changeWorld wl f (C3D x y (0∷Int)) c g
keyboardMouse _ _ _ _ _ _ _ _ _ = return ()

xsaTest ∷ IO ()
xsaTest = do
  let t1 = xsa (0,0) X (F ["XOO", "   ", "   "])
  let t2 = xsa (0,0) X (F ["XXX", "   ", "   "])
  let t3 = xsa (0,0) X (F ["XXX", "OO ", "   "])
  let t4 = xsa (0,2) X (F ["XXX", "   ", "   "])
  print $ t1
  print $ t1 ≡ GA
  print $ t2
  print $ t2 ≡ XW
  print $ t3
  print $ t3 ≡ XW

main∷IO ()
main = do
    let width = 1280
    let height = 1024
    let orthoWidth = 40
    let orthoHeight = 30
    (_progName, _args) ← getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size width height
    createWindow "AXT GL - www.axi.su - xruzzzz@gmail.com"
    xsaTest
    angle ← newIORef $ pi/2
    delta ← newIORef $ pi/360
    pos ← newIORef (0, 0)
    game ← newIORef (GA, F ["   ", "   ", "   "])
    cheri ← newIORef X
    gameType ← newIORef VS2
    keyboardMouseCallback $= Just (keyboardMouse delta pos game cheri gameType)
    idleCallback $= Just (idle game)
    displayCallback $= display angle game
    matrixMode $= Projection
    loadIdentity
    ortho2D (-340) 340 (-330) 330 -- (-5) 20
    matrixMode $= Modelview 0

    mainLoop

idle∷IORef (Winer, Field String) → IdleCallback
idle reWF = do
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

display ∷ IORef GLfloat → IORef (Winer, Field String) → DisplayCallback
display ang gs = do
    clear [ColorBuffer, DepthBuffer]
    a ← get ang
    preservingMatrix $ do
--        scale 0.5 0.5 (0.5∷GLfloat)
        color $ Color3 (0.2∷GLfloat) 0.8 0.8
--        rotate a $ Vector3 1 0 0
        (w, F fi) ← get gs
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
