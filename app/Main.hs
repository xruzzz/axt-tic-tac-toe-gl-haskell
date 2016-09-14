{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad
import Data.IORef
import Prelude.Unicode
import Graphics.UI.GLUT
import Data.Ratio(Ratio, (%))

import Data.Typeable
import System.Random(mkStdGen, randomR)
-- import Data.Sequence (update, fromList)
-- import qualified Data.ByteString.Char8 as BSC8 (ByteString, elemIndices, index, pack, unpack)
import Data.List (elemIndices)
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Game.Types
import Game.Actions(changeLine)
import Game.Conversions (c3D, cv, cher, re, toGLFloat)
import Game.Rules (isEnd)
import AI.Algorithms

xₘᵢₙ = negate xₘₐₓ
xₘₐₓ = 300 ∷ RatI
yₘᵢₙ = negate yₘₐₓ
yₘₐₓ = xₘₐₓ

nₓ = 3
ny = nₓ

stₓ = (xₘₐₓ - xₘᵢₙ) /nₓ
sty = (yₘₐₓ - yₘᵢₙ) /ny
offsetₓ = 50∷RatI
offsety = 80∷RatI

        
changeWorld ∷ Winer → GameField → SCoor → State → GameType → (Winer, GameField)
changeWorld GA (F [s1, s2, s3]) (C3D x y z) s t
  | y ≡ 0 = let
                cl = sChange s1
                (w1,f1) = (if s1 ≠ cl then (isEnd (y,x) s (F [cl, s2, s3])) else GM1, F [cl, s2, s3])
            in (w1, getPCstep f1 0)
  | y ≡ 1 = let cl = sChange s2
            in (if s2 ≠ cl then (isEnd (y,x) s (F [s1, cl, s3])) else GM2, F [s1, cl, s3])
  | y ≡ 2 = let cl = sChange s3
            in (if s3 ≠ cl then (isEnd (y,x) s (F [s1, s2, cl])) else GM3, F [s1, s2, cl])
  where sChange = changeLine x s
changeWorld w (F [s1, s2, s3]) (C3D x y z) _ _ = (w,(F [s1, s2, s3]))

getPCstep ∷ GameField → GameLevel → GameField
getPCstep (F [s1, s2, s3]) l = do
    case l of
        0 → let
                sg1 = mkStdGen 42
                {-fp = map findFreePos [s1, s2, s3]
                fp0 = fp !! 0
                fp1 = fp !! 1
                fp2 = fp !! 2
                n = if (length fp0)>0 then (head $ map fst fp0) else
                        if (length fp1)>0 then (head $ map fst fp1) else
                            if (length fp2)>0 then (head $ map fst fp2) else 4
                fch s fd = if (length fd) > 0 then (replaceNth n 'O' s) else s
                -}
                in (F [s1, s2, s3]) -- (F (map fch [s1, s2,s3]))
        1 → undefined


replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

-- | Функции поиска свободных позиций
-- return [0,2],[],..


{-
getFreePos ∷ GameField → [SCoor]
getFreePos (F [s1, s2, s3]) = let dd = map findFreePos [s1, s2, s3]
                              in filter (\x -> length (snd x) > 0) . zip [0..] $ dd
                              
getFreePos2 ∷ [[Int]] → [SCoor]
getFreePos2 d = getFreePosHelper d 0 []
getFreePosHelper [] _ acc = acc
getFreePosHelper (x:xs) n acc =  getFreePosHelper xs (n+1) (acc ++ (map (\din -> C3D {x=n, y=din , z=0}) xs))
-}

findFreePos ∷ GameField → [[Int]]
findFreePos (F s) = map (elemIndices ' ') s

class F a where
    tron ∷ (a → b) → Bool

instance F State where
    tron d = True

step = 100 -- для текста 0.6

changeState ∷ Char → State → GameField → IO ()
changeState  'q' _ _ = do putStrLn "The End"
changeState c s f = do
    line ← getChar
    putStrLn $ show f
    changeState line X initField

keyboardMouse ∷ IORef GLfloat → IORef (GLfloat, GLfloat) → IORef (Winer, GameField)→ IORef State→IORef GameType→ KeyboardMouseCallback
keyboardMouse a p gg ch gtr key Down _ _ = do
  (w,ffs1) ← get gg
  cx ← get ch
  gt ← get gtr
  case key of
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
    (SpecialKey KeyF1   ) → gg $~! (\(_,_) → (GA, initField))     -- новая игра 1 игрок
    (SpecialKey KeyF2   ) → gg $~! (\(_,_) → (GA, initField))     -- новая игра 2 игрока
    _ → return ()
  (w,ffs2) ← get gg
  print w
  if ffs1 ≠ ffs2 then do
      case gt of
           VS_PC → putStrLn "ход компьютера"
           VS_USER → ch $~! \x → cher x
           _ → return ()
      print $ ffs2
      case w of
           XW → putStrLn " X выиграли!"
           OW → putStrLn " O выиграли!"
           END → putStrLn " Ничья! Нажмите F1"
           _ → return ()
--      checkLine ffs2
                 else return ()
    where ccWw wl (x, y) c g = \(w,f) → changeWorld wl f (c3D x y) c g
keyboardMouse _ _ _ _ _ _ _ _ _ = return ()

isEndTest ∷ IO ()
isEndTest = do
  let t1 = isEnd (0,0) X $ F ["XOO", "   ", "   "]
  let t2 = isEnd (0,0) X $ F ["XXX", "   ", "   "]
  let t3 = isEnd (0,0) X $ F ["XXX", "OO ", "   "]
  let t4 = isEnd (0,2) X $ F ["XXX", "   ", "   "]
  print $ t1
  print $ t1 ≡ GA
  print $ t2
  print $ t2 ≡ XW
  print $ t3
  print $ t3 ≡ XW

main ∷ IO ()
main = do
    let width = 1280
    let height = 1024
    let orthoWidth = 40
    let orthoHeight = 30
    (_progName, _args) ← getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size width height
    createWindow "AXT GL - www.axi.su - xruzzzz@gmail.com"
    isEndTest
    angle ← newIORef $ pi/2
    delta ← newIORef $ pi/360
    pos ← newIORef (0, 0)
    game ← newIORef (GA, initField)
    cheri ← newIORef X
    gameType ← newIORef VS_USER
    keyboardMouseCallback $= Just (keyboardMouse delta pos game cheri gameType)
    idleCallback $= Just (idle game)
    displayCallback $= display angle game
    matrixMode $= Projection
    
    loadIdentity
    ortho2D (-340) 340 (-330) 330 -- (-5) 20
    matrixMode $= Modelview 0

    mainLoop

idle ∷ IORef (Winer, GameField) → IdleCallback
idle reWF = do
--  d <- get delta
--  angle $~! (+ d)
--  (F fi) ← get fs
--  ds 
  postRedisplay Nothing

showState ∷ GameField → IO ()
showState (F fs) = do
  let
    dd = zip [0..] $ map (zip [0..]) fs
  forM_ dd $ \(i,ss) → do
--    putStrLn $ show i
--    raw X (C3D 0 0 (0∷RatI))
--    line3D (C3D (stₓ/2) (-50) 0) (C3D 40 (stₓ/2) 0)
    forM_ ss $ \(j,ts) → do
      let sd = cv ts
      case sd of
           X → showR X $ c3D j i
           O → showR O $ c3D j i
           otherwise → return ()

display ∷ IORef GLfloat → IORef (Winer, GameField) → DisplayCallback
display ang gs = do
    clear [ColorBuffer, DepthBuffer]
    a ← get ang
    preservingMatrix $ do
--        scale 0.5 0.5 (0.5∷GLfloat)
        color $ Color3 (0.2 ∷ GLfloat) 0.8 0.8
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
field3D ∷ IO ()
field3D = forM_ [negate stₓ/2,stₓ/2] $ \x → do
       line3D (C3D x yₘᵢₙ 0) (C3D x yₘₐₓ 0)
       line3D (C3D xₘᵢₙ x 0) (C3D xₘₐₓ x 0)

xToRawX, yToRawY ∷ RatI → GLfloat
xToRawX x = toGLFloat $ xₘᵢₙ + x

yToRawY y = toGLFloat $ yₘₐₓ - y

showS ∷ State → SCoor → IO ()
showS s (C3D x y z) = rawT s (C3D (xₘᵢₙ + (150 * x % 1)) (yₘₐₓ - (150 * y % 1)) 0)

showR ∷ State → SCoor → IO ()
showR s (C3D x y z) = raw s (C3D (xₘᵢₙ + (stₓ* (x % 1))) (yₘₐₓ - (sty * (y % 1))) 0)

-- | Нарисовать знак
raw ∷ State → Coor3DRI → IO ()
raw s (C3D x y z) =
      case s of
            X → do
              line3D (C3D x₁ y₁ z) (C3D x₂ y₂ z)
              line3D (C3D x₁ y₂ z) (C3D x₂ y₁ z)
--              line3D (C3D x₂ y₁ z) (C3D x₂ y₂ z)
--              line3D (C3D x₁ y₁ z) (C3D x₁ y₂ z)
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

line3D ∷ Coor3DRI → Coor3DRI → IO ()
line3D beg end = do
        vertex (Vertex3 (toGLFloat (x beg)) (toGLFloat (y beg)) (toGLFloat (z beg)))
        vertex (Vertex3 (toGLFloat (x end)) (toGLFloat (y end)) (toGLFloat (z end)))
