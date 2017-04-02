{-# LANGUAGE UnicodeSyntax #-}
module Main where

-- import Data.Sequence (update, fromList)
-- import qualified Data.ByteString.Char8 as BSC8 (ByteString, elemIndices, index, pack, unpack)
import AXT.TicTacToe.AI( getRandomPCstep, getAXTLevel2PCstep, getAXTLevel3PCstep)
import AXT.TicTacToe.Actions as ATA (step)
import AXT.TicTacToe.Conversions (c3D, cher, re, toGLFloat)
import AXT.TicTacToe.Field as FA(findFreePos, getFreePos)
import AXT.TicTacToe.Graphics as ATG (display, idle)
import AXT.TicTacToe.Types as GT (Coor3DRI(..), CoorOnField, GameField, GameType(..), Field(F), GameLevel(..), RangeCoor(..), RatI, SCoor(..), State(..), StepResult(..), initField, toCoorOnField)
import Control.Monad(forM_)
import Data.IORef
import Data.Typeable
-- import qualified Debug.Trace as DT (trace)
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), Coor3D(..), fromDegrees)
import Graphics.UI.GLUT
import Prelude.Unicode
import qualified System.Console.ANSI as SCA (Color(..), ColorIntensity(..), ConsoleIntensity(..), ConsoleLayer(..), SGR (..), setSGR)

type StateAngle = IORef GLfloat
type StatePosition = IORef (GLfloat, GLfloat)
type StateGame = IORef (StepResult, GameField)
type StateGameType = IORef GameType
type StateGameLevel =  IORef GameLevel

replaceNth n newVal (x:xs)
     | n ≡ 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

changeState ∷ Char → State → GameField → IO ()
changeState  'q' _ _ = do putStrLn "The End"
changeState c s f = do
    line ← getChar
    putStrLn $ show f
    changeState line X initField

keyboardMouse ∷ StateAngle → StatePosition → StateGame → IORef State → StateGameType → StateGameLevel → KeyboardMouseCallback
keyboardMouse a p gg ch gameType level key Down _ _ = do
    (w, ffs1) ← get gg
    let
        lf = getFreePos ffs1
        coor = ck key
    cx ← get ch
    gt ← get gameType
    
    case key of
--    (MouseButton LeftButton) → ff $~! (\x → changeWorld x (C3D 1 1 (0∷Int)) X)
        (Char ' ') → a $~! negate
        (Char '+') → do
            level $~! succ
            glvl ← get level
            print glvl
        (Char '-') → do
            level $~! pred
            glvl ← get level
            print glvl
        (Char '1') → sel coor lf w cx gt
        (Char '2') → sel coor lf w cx gt
        (Char '3') → sel coor lf w cx gt
        (Char '4') → sel coor lf w cx gt
        (Char '5') → sel coor lf w cx gt
        (Char '6') → sel coor lf w cx gt
        (Char '7') → sel coor lf w cx gt
        (Char '8') → sel coor lf w cx gt
        (Char '9') → sel coor lf w cx gt
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
    
    lvl ← get level
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
                let coor2 = case lvl of
                        GameLevel 1 → getRandomPCstep lf2
                        GameLevel 2 → case coor of
                                        (Just xc) → getAXTLevel2PCstep ffs2 xc cx
                                        Nothing → getRandomPCstep lf2
                        GameLevel 3 → case coor of
                                            (Just xc) → getAXTLevel3PCstep ffs2 xc cx
                                            Nothing → getRandomPCstep lf2
                        _ → head lf2
                sel (Just coor2) lf w cx gt
                ch $~! \x → cher x
            VS_USER → ch $~! \x → cher x
            _ → return ()
        print $ ffs2
        caseWiner w
    else caseWiner w

  where
      ccWw cr c g = \(w,f) → step c f cr g
      sel pos freePos win cxin gtin = do
          case pos of
               (Just x) → if (x `elem` freePos) ∧ (win == GA) then (gg $~! ccWw x cxin gtin) else (return ())
               Nothing → return ()
      caseWiner x = do
                let f1 = " Нажмите F3"
                case x of
                        XWIN → putStrLn $ " X выиграли!" ++ f1
                        OWIN → putStrLn $ " O выиграли!" ++ f1
                        END → putStrLn $ " Ничья!" ++ f1
                        _ → return ()
      ck k = case k of
            (Char '1') → Just $ toCoorOnField 2 0
            (Char '2') → Just $ toCoorOnField 2 1
            (Char '3') → Just $ toCoorOnField 2 2
            (Char '4') → Just $ toCoorOnField 1 0
            (Char '5') → Just $ toCoorOnField 1 1
            (Char '6') → Just $ toCoorOnField 1 2
            (Char '7') → Just $ toCoorOnField 0 0
            (Char '8') → Just $ toCoorOnField 0 1
            (Char '9') → Just $ toCoorOnField 0 2
            _ → Nothing
keyboardMouse _ _ _ _ _ _ _ _ _ _ = return ()


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
    createWindow "AXT Tic-Tac-Toe"
    angle ← newIORef $ pi/2
    delta ← newIORef $ pi/360
    level ← newIORef $ GameLevel 1
    pos ← newIORef (0, 0)
    game ← newIORef (GA, initField)
    cheri ← newIORef X
    gameType ← newIORef VS_PC
    keyboardMouseCallback $= Just (keyboardMouse delta pos game cheri gameType level)
    idleCallback $= Just (idle game)
    displayCallback $= display angle game
    matrixMode $= Projection
    
    loadIdentity
    ortho2D (-340) 340 (-330) 330 -- (-5) 20
    matrixMode $= Modelview 0

    mainLoop
