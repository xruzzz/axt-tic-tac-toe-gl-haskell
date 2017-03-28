{-# LANGUAGE UnicodeSyntax #-}
module AXT.TicTacToe.Graphics
    (
        display,
        idle
    ) where
import Data.Ratio(Ratio, (%))
import Control.Monad(forM_)
import AXT.TicTacToe.Types as AT (Coor3DRI(..), CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..))
import GL.Types as GLT (V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Data.IORef as DI(IORef)
import Graphics.UI.GLUT as GUG(ClearBuffer(ColorBuffer, DepthBuffer), Color3, DisplayCallback, GLfloat, IdleCallback, PrimitiveMode(Lines), StrokeFont( Roman ), Vector3,
    clear, color, get, postRedisplay, preservingMatrix, renderPrimitive, renderString, swapBuffers, translate, vertex)
import Graphics.Rendering.OpenGL.GL.VertexSpec as GROGL( Color3(..) )
import Graphics.Rendering.OpenGL.GL.Tensor as GROGL ( Vector3(..), Vertex3(..) )
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import AXT.TicTacToe.Conversions (c3D, cv, cher, re, toGLFloat)

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

step = 100 -- для текста 0.6

idle ∷ IORef (StepResult, GameField) → IdleCallback
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

display ∷ IORef GLfloat → IORef (StepResult, GameField) → DisplayCallback
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
