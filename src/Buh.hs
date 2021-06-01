{-# LANGUAGE OverloadedStrings #-}

{-
See https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst
https://github.com/jtdaugherty/brick/blob/master/programs/CustomEventDemo.hs
https://github.com/NorfairKing/writing-a-text-editor-in-haskell-with-brick
https://hackage.haskell.org/package/brick-0.62/docs/Brick-Main.html#t:App
-}
module Buh where

import Brick (App (..), BrickEvent (..), CursorLocation, EventM, Next, ViewportType (..), Widget, attrMap, continue,
              customMain, getVtyHandle, halt, showCursorNamed, txt, vBox, vLimit, viewport)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent.Async (race)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (isLeft)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import qualified Graphics.Vty as V
-- import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)

newtype Pos = Pos Int deriving (Eq, Show, Ord)

data Span = Span
  { spanStart :: !Pos
  , spanEnd :: !Pos
  } deriving (Eq, Show)

data Elem e a = Elem
  { elemSpan :: !Span
  , elemContents :: !Text
  , elemParsed :: !(Either e a)
  } deriving (Eq, Show)

data Buffer e a = Buffer
  { bufferUp :: !(Maybe (Buffer e a))
  , bufferElem :: !(Elem e a)
  , bufferTail :: !Text
  } deriving (Eq, Show)

type Parser e a = Text -> Maybe (Elem e a, Text)

bufferDown :: Parser e a -> Buffer e a -> Maybe (Buffer e a)
bufferDown p b = fmap (uncurry (Buffer (Just b))) (p (bufferTail b))

bufferIsOk :: Buffer e a -> Bool
bufferIsOk = isLeft . elemParsed . bufferElem

bufferBottom :: Parser e a -> Buffer e a -> Buffer e a
bufferBottom p b =
  if bufferIsOk b
    then maybe b (bufferBottom p) (bufferDown p b)
    else b

bufferTop :: Buffer e a -> Buffer e a
bufferTop b = maybe b bufferTop (bufferUp b)

data Name = NameEditor | NameStatus | NameCommand
  deriving (Eq, Ord, Show)

data ReqBody =
    ReqBodyPing
  | ReqBodyBoom
  | ReqBodyOpen !FilePath
  deriving (Eq, Show)

data ResBody =
    ResBodyPong
  | ResBodyOpened !FilePath !Text
  | ResBodyNotFound !FilePath
  deriving (Eq, Show)

data Req = Req !Int !ReqBody
  deriving (Eq, Show)

data Ev =
    EvWorkerCrash !Text
  | EvRes !Int !ResBody
  deriving (Eq, Show)

data EdMode =
    EdModeNormal
  | EdModeInsert
  | EdModeCommand
  deriving (Eq, Show)

edModeFocus :: EdMode -> Name
edModeFocus emo =
  case emo of
    EdModeNormal -> NameEditor
    EdModeInsert -> NameEditor
    EdModeCommand -> NameCommand

edModeText :: EdMode -> Text
edModeText emo =
  case emo of
    EdModeNormal -> "Normal"
    EdModeInsert -> "Insert"
    EdModeCommand -> "Command"

data St = St
  { stFocus :: !Name
  , stEdMode :: !EdMode
  , stReqIndex :: !Int
  , stFilename :: !(Maybe FilePath)
  , stEditorContents :: !Text
  , stStatusLine :: !Text
  , stCommandLine :: !Text
  } deriving (Eq, Show)

mkSt :: St
mkSt = St NameEditor EdModeNormal 0 Nothing "" "" ""

stacked :: [Widget Name] -> [Widget Name]
stacked widgets = [vBox (intersperse hBorder widgets)]

myDraw :: St -> [Widget Name]
myDraw st =
  let editorArea = viewport NameEditor Both (txt (stEditorContents st))
      statusArea = vLimit 1 (viewport NameStatus Horizontal (txt (stStatusLine st)))
      commandArea = vLimit 1 (viewport NameCommand Horizontal (txt (stCommandLine st)))
  in stacked [editorArea, statusArea, commandArea]

myChoose :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
myChoose = showCursorNamed . stFocus

sendReq :: MonadIO m => BChan Req -> IORef St -> ReqBody -> m ()
sendReq reqChan ref reqb = do
  st <- liftIO (readIORef ref)
  let n = stReqIndex st
      req = Req n reqb
      st' = st { stReqIndex = n + 1 }
  liftIO (writeIORef ref st')
  liftIO (writeBChan reqChan req)

writeStatus :: MonadIO m => IORef St -> Text -> m ()
writeStatus ref line = liftIO (modifyIORef' ref (\st -> st { stStatusLine = line }))

setContents :: MonadIO m => IORef St -> Text -> m ()
setContents ref contents = liftIO (modifyIORef' ref (\st -> st { stEditorContents = contents }))

runSt :: MonadIO m => St -> (IORef St -> m a) -> m (a, St)
runSt st0 f = do
  ref <- liftIO (newIORef st0)
  a <- f ref
  st1 <- liftIO (readIORef ref)
  pure (a, st1)

execSt :: MonadIO m => St -> (IORef St -> m ()) -> m St
execSt st0 f = fmap snd (runSt st0 f)

getsSt :: MonadIO m => IORef St -> (St -> a) -> m a
getsSt ref f = liftIO (fmap f (readIORef ref))

data NextOp = NextOpHalt | NextOpContinue
  deriving (Eq, Show)

withNextSt :: St -> (IORef St -> EventM n NextOp) -> EventM n (Next St)
withNextSt st0 f = do
  (op, st1) <- runSt st0 f
  case op of
    NextOpHalt -> halt st1
    NextOpContinue -> continue st1

myStart :: BChan Req -> Maybe FilePath -> St -> EventM Name St
myStart reqChan mfp st = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  -- Enable paste
  when (V.supportsMode output V.BracketedPaste)
    (liftIO (V.setMode output V.BracketedPaste True))
  -- Enable mouse
  when (V.supportsMode output V.Mouse)
    (liftIO (V.setMode output V.Mouse True))
  -- Send open file request
  case mfp of
    Nothing -> pure st
    Just fp -> execSt st $ \ref -> do
      writeStatus ref ("Opening " <> T.pack fp)
      sendReq reqChan ref (ReqBodyOpen fp)

setEdMode :: MonadIO m => IORef St -> EdMode -> m ()
setEdMode ref emo = do
  let focus = edModeFocus emo
      comLine = if focus == NameCommand then ":" else ""
      text = edModeText emo
  liftIO (modifyIORef' ref (\st -> st { stFocus = focus, stEdMode = emo, stCommandLine = comLine }))
  writeStatus ref (text <> " mode")

myHandle :: BChan Req -> St -> BrickEvent Name Ev -> EventM Name (Next St)
myHandle _ st e = withNextSt st $ \ref -> do
  case e of
    -- Hard quit on mac: Left option + escape
    VtyEvent (V.EvKey V.KEsc [V.MMeta]) -> pure NextOpHalt
    -- Handle rest of keyboard input
    VtyEvent (V.EvKey key mods) -> do
      emo <- getsSt ref stEdMode
      case emo of
        EdModeNormal ->
          case (key, mods) of
            (V.KEsc, []) -> setEdMode ref EdModeNormal
            (V.KChar ':', []) -> setEdMode ref EdModeCommand
            (V.KChar 'i', []) -> setEdMode ref EdModeInsert
            _ -> pure ()
        EdModeInsert ->
          case (key, mods) of
            (V.KEsc, []) -> setEdMode ref EdModeNormal
            _ -> pure ()
        EdModeCommand ->
          case (key, mods) of
            (V.KEsc, []) -> setEdMode ref EdModeNormal
            _ -> pure ()
      pure NextOpContinue
    -- Handle worker events
    AppEvent ev -> do
      case ev of
        EvWorkerCrash msg -> error ("Worker crashed: " ++ T.unpack msg)
        EvRes _ resb ->
          case resb of
            ResBodyPong -> writeStatus ref "Pong"
            ResBodyOpened fp contents -> do
              setContents ref contents
              writeStatus ref ("Loaded: " <> T.pack fp)
            ResBodyNotFound fp -> writeStatus ref ("File not found: " <> T.pack fp)
      pure NextOpContinue
    _ -> pure NextOpContinue

mkApp :: BChan Req -> Maybe FilePath -> App St Ev Name
mkApp reqChan mfp = App
  { appDraw = myDraw
  , appChooseCursor = myChoose
  , appHandleEvent = myHandle reqChan
  , appStartEvent = myStart reqChan mfp
  , appAttrMap = const (attrMap V.defAttr [])
  }

worker :: BChan Req -> BChan Ev -> IO ()
worker reqChan evChan = forever $ do
  Req n reqb <- liftIO (readBChan reqChan)
  resb <- case reqb of
    ReqBodyPing -> pure ResBodyPong
    ReqBodyBoom -> error "Boom!"
    ReqBodyOpen fp -> do
      e <- doesFileExist fp
      if e
        then fmap (ResBodyOpened fp) (TIO.readFile fp)
        else pure (ResBodyNotFound fp)
  liftIO (writeBChan evChan (EvRes n resb))

exe :: IO ()
exe = do
  args <- getArgs
  mfn <- case args of
    [] -> pure Nothing
    [fn] -> pure (Just fn)
    _ -> die "Use: buffer.hs [filename]"
  putStrLn "Configuring app"
  let maxLen = 1000
  reqChan <- newBChan maxLen
  evChan <- newBChan maxLen
  let app = mkApp reqChan mfn
  let vtyBuilder = V.mkVty V.defaultConfig
  firstVty <- vtyBuilder
  let runUi = customMain firstVty vtyBuilder (Just evChan) app mkSt
  let runWorker = worker reqChan evChan
  putStrLn "Starting app"
  _ <- race runUi runWorker
  putStrLn "Stopped app"
