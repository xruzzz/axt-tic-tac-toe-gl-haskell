{-# LANGUAGE UnicodeSyntax #-}
module Main where

-- import Data.Sequence (update, fromList)
-- import qualified Data.ByteString.Char8 as BSC8 (ByteString, elemIndices, index, pack, unpack)
import AXT.TicTacToe.AI( getRandomPCstep)
import AXT.TicTacToe.Actions(changeWorld)
import AXT.TicTacToe.Conversions (c3D, cher, re, toGLFloat)
import AXT.TicTacToe.Field as FA(findFreePos, getFreePos)
import AXT.TicTacToe.Graphics as ATG (display, idle)
import AXT.TicTacToe.Types as GT (Coor3DRI(..), CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..), initField, toCoorOnField)
import Control.Monad(forM_)
import Data.IORef
import Data.Typeable
import Debug.Trace (trace)
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Graphics.UI.GLUT
import Prelude.Unicode
import qualified System.Console.ANSI as SCA (Color(..), ColorIntensity(..), ConsoleIntensity(..), ConsoleLayer(..), SGR (..), setSGR)

type StateAngle = IORef GLfloat
type StatePosition = IORef (GLfloat, GLfloat)
type StateGame = IORef (StepResult, GameField)
type StateGameType = IORef GameType

replaceNth n newVal (x:xs)
     | n ≡ 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

changeState ∷ Char → State → GameField → IO ()
changeState  'q' _ _ = do putStrLn "The End"
changeState c s f = do
    line ← getChar
    putStrLn $ show f
    changeState line X initField

keyboardMouse ∷ StateAngle → StatePosition → StateGame → IORef State → StateGameType → KeyboardMouseCallback
keyboardMouse a p gg ch gameType key Down _ _ = do
    (w,ffs1) ← get gg
    let lf = getFreePos ffs1
    cx ← get ch
    gt ← get gameType
    case key of
    --    (MouseButton LeftButton) → ff $~! (\x →changeWorld x (C3D 1 1 (0∷Int)) X)
        (Char ' ') → a $~! negate
        (Char '+') → a $~! (* 2)
        (Char '-') → a $~! (/ 2)
        (Char '1') → sel (toCoorOnField 2 0) lf w cx gt
        (Char '2') → sel (toCoorOnField 2 1) lf w cx gt
        (Char '3') → sel (toCoorOnField 2 2) lf w cx gt
        (Char '4') → sel (toCoorOnField 1 0) lf w cx gt
        (Char '5') → sel (toCoorOnField 1 1) lf w cx gt
        (Char '6') → sel (toCoorOnField 1 2) lf w cx gt
        (Char '7') → sel (toCoorOnField 0 0) lf w cx gt
        (Char '8') → sel (toCoorOnField 0 1) lf w cx gt
        (Char '9') → sel (toCoorOnField 0 2) lf w cx gt
        (SpecialKey KeyLeft ) → p $~! \(x,y) → (x-0.1,y)
        (SpecialKey KeyRight) → p $~! \(x,y) → (x+0.1,y)
        (SpecialKey KeyUp   ) → p $~! \(x,y) → (x,y+0.1)
    --    (SpecialKey KeyDown ) →  -- \(x,y) → (x,y-0.1)
        (SpecialKey KeyF1   ) → do
                putStrLn "\t\t\tAXT TicTacToe\n\t\tСправка"
                mapM_ putStrLn ["\tF1 - Справка",
                                "\tF2 - 2 игрока",
                                "\tF3 - Игрок против компьютера"]
        (SpecialKey KeyF2   ) → do
                gg $~! (\(_,_) → (GA, initField))     -- новая игра 2 игрока
                ch $~! \_ → X
                gameType  $~! \_ → VS_USER
        (SpecialKey KeyF3   ) → do
                gg $~! (\(_,_) → (GA, initField))     -- новая игра 1 игрока
                ch $~! \_ → X
                gameType  $~! \_ → VS_PC
        _ → return ()
    (w,ffs2) ← get gg
    let lf2 = getFreePos ffs2
    print w
    if (ffs1 ≠ ffs2) ∧ (lf2 ≠ []) then do
        case gt of
            VS_PC → do
                SCA.setSGR [SCA.SetConsoleIntensity SCA.BoldIntensity, SCA.SetColor SCA.Foreground SCA.Vivid SCA.Red]
                putStrLn "Ход компьютера"
                SCA.setSGR [SCA.Reset]
                ch $~! \x → cher x
                cx ← get ch
                let l = GameLevel 1
                let coor = case l of
                     GameLevel 1 -> getRandomPCstep lf2
                     _ -> head lf2
                sel coor lf w cx gt
                ch $~! \x → cher x
            VS_USER → ch $~! \x → cher x
            _ → return ()
        print $ ffs2
        caseWiner w
    else caseWiner w

  where
      ccWw wl cr c g = \(w,f) → changeWorld wl f cr c g
      sel pos freePos win cxin gtin = do
          trace (show pos ++ "\n" ++ show freePos) $ if pos `elem` freePos then (gg $~! ccWw win pos cxin gtin) else (return ())
      caseWiner x = do
                let f1 = " Нажмите F3"
                case x of
                        XWIN → putStrLn $ " X выиграли!" ++ f1
                        OWIN → putStrLn $ " O выиграли!" ++ f1
                        END → putStrLn $ " Ничья!" ++ f1
                        _ → return ()
keyboardMouse _ _ _ _ _ _ _ _ _ = return ()


main ∷ IO ()
main = do
    let
        width = 1024
        height = 768
        orthoWidth = 40
        orthoHeight = 30
    (_progName, _args) ← getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size width height
    createWindow "AXT Крестики - нолики"
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
