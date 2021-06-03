{-# LANGUAGE OverloadedStrings #-}

{-
See https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst
https://github.com/jtdaugherty/brick/blob/master/programs/CustomEventDemo.hs
https://github.com/NorfairKing/writing-a-text-editor-in-haskell-with-brick
https://hackage.haskell.org/package/brick-0.62/docs/Brick-Main.html#t:App
-}
module Buh where

import Brick (App (..), AttrMap, AttrName, BrickEvent (..), CursorLocation, EventM, Location (..), Next,
              ViewportType (..), Widget, attrMap, continue, customMain, getVtyHandle, hBox, halt, showCursor,
              showCursorNamed, textWidth, txt, vBox, vLimit, viewport, visibleRegion, withAttr)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Util (on)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent.Async (race)
import Control.Monad (forever, join, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (isLeft)
import Data.Functor (($>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import qualified Graphics.Vty as V
import LittleLogger.Manual (SimpleLogAction, fileSimpleLogAction, logDebug)
-- import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Zipper (TextZipper)
import qualified Data.Text.Zipper as Z
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
  { stEdMode :: !EdMode
  , stReqIndex :: !Int
  , stFilename :: !(Maybe FilePath)
  , stEditorContents :: !(TextZipper Text)
  , stStatusLine :: !Text
  , stCommandLine :: !(TextZipper Text)
  } deriving (Eq, Show)

stFocus :: St -> Name
stFocus = edModeFocus . stEdMode

emptyZipper :: TextZipper Text
emptyZipper = Z.textZipper [] Nothing

mkSt :: St
mkSt = St EdModeNormal 0 Nothing emptyZipper "" emptyZipper

stacked :: [Widget Name] -> [Widget Name]
stacked widgets = [vBox (intersperse hBorder widgets)]

-- TODO there should be a way to cache this...
-- (And support tab width > 1)
saniTxt :: Text -> Widget Name
saniTxt t = txt (if T.any (== '\t') t then T.map (\c -> if c == '\t' then ' ' else c) t else t)

charAtCursor :: TextZipper Text -> Maybe Text
charAtCursor z =
    let col = snd (Z.cursorPosition z)
        curLine = Z.currentLine z
        toRight = T.drop col curLine
    in if T.length toRight > 0
       then Just (T.take 1 toRight)
       else Nothing

renderEditor :: St -> [Widget Name]
renderEditor st =
    let z = stEditorContents st
        emo = stEdMode st
        foc = emo /= EdModeCommand
        (row, col) = Z.cursorPosition z
        toLeft = T.take col (Z.currentLine z)
        cursorLoc = Location (textWidth toLeft, row)
        atChar = charAtCursor z
        atCharWidth = maybe 1 textWidth atChar
        widget = withAttr plainOffAttr $
          viewport NameEditor Both $
          (if foc then showCursor NameEditor cursorLoc else id) $
          visibleRegion cursorLoc (atCharWidth, 1) $
          vBox . fmap saniTxt $
          Z.getText z
    in [widget]

renderStatus :: St -> [Widget Name]
renderStatus st =
  let line = stStatusLine st
  in [vLimit 1 (viewport NameStatus Horizontal (saniTxt line)) | not (T.null line) ]

renderCommand :: St -> [Widget Name]
renderCommand st =
    let z = stCommandLine st
        emo = stEdMode st
        foc = emo == EdModeCommand
        (row, col) = Z.cursorPosition z
        toLeft = T.take col (Z.currentLine z)
        cursorLoc = Location (textWidth toLeft, row)
        atChar = charAtCursor z
        atCharWidth = maybe 1 textWidth atChar
        innerWidget =
          withAttr plainOffAttr $
            viewport NameCommand Both $
            (if foc then showCursor NameCommand cursorLoc else id) $
            visibleRegion cursorLoc (atCharWidth, 1) $
            vBox . fmap saniTxt $
            Z.getText z
        outerWidget = hBox [txt ":", innerWidget]
    in [vLimit 1 outerWidget | foc]

myDraw :: St -> [Widget Name]
myDraw st = stacked (join [renderEditor st, renderStatus st, renderCommand st])

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
setContents ref contents = liftIO (modifyIORef' ref (\st -> st { stEditorContents = Z.textZipper (T.lines contents) Nothing }))

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
  liftIO $ modifyIORef' ref $ \st ->
    -- If moving away from command focus, clear command line
    let comLine = if emo /= EdModeCommand && stEdMode st == EdModeCommand then Z.textZipper [] Nothing else stCommandLine st
    in st { stEdMode = emo, stCommandLine = comLine }
  writeStatus ref (edModeText emo <> " mode")

editContents :: MonadIO m => IORef St -> (TextZipper Text -> TextZipper Text) -> m ()
editContents ref f = liftIO (modifyIORef' ref (\st -> st { stEditorContents = f (stEditorContents st) }))

editCommand :: MonadIO m => IORef St -> (TextZipper Text -> TextZipper Text) -> m ()
editCommand ref f = liftIO (modifyIORef' ref (\st -> st { stCommandLine = f (stCommandLine st) }))

data Clear = ClearYes | ClearNo deriving (Eq, Show)

myHandle :: BChan Req -> St -> BrickEvent Name Ev -> EventM Name (Next St)
myHandle _ st e = withNextSt st $ \ref -> do
  case e of
    -- Hard quit: Left option + escape, or ctrl-c
    VtyEvent (V.EvKey V.KEsc [V.MMeta]) -> pure NextOpHalt
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> pure NextOpHalt
    -- Handle rest of keyboard input
    VtyEvent (V.EvKey key mods) -> do
      emo <- getsSt ref stEdMode
      clear <- case emo of
        EdModeNormal ->
          case (key, mods) of
            -- Mode switching
            (V.KEsc, []) -> setEdMode ref EdModeNormal $> ClearNo
            (V.KChar ':', []) -> setEdMode ref EdModeCommand $> ClearNo
            (V.KChar 'i', []) -> setEdMode ref EdModeInsert $> ClearNo
            -- Chorded + special key movement
            (V.KChar 'a', [V.MCtrl]) -> editContents ref Z.gotoBOL $> ClearYes
            (V.KChar 'e', [V.MCtrl]) -> editContents ref Z.gotoEOL $> ClearYes
            (V.KChar 'k', [V.MCtrl]) -> editContents ref Z.killToEOL $> ClearYes
            (V.KChar 'u', [V.MCtrl]) -> editContents ref Z.killToBOL $> ClearYes
            (V.KUp, []) -> editContents ref Z.moveUp $> ClearYes
            (V.KChar 'p', [V.MCtrl]) -> editContents ref Z.moveUp $> ClearYes
            (V.KDown, []) -> editContents ref Z.moveDown $> ClearYes
            (V.KChar 'n', [V.MCtrl]) -> editContents ref Z.moveDown $> ClearYes
            (V.KLeft, []) -> editContents ref Z.moveLeft $> ClearYes
            (V.KChar 'b', [V.MCtrl]) -> editContents ref Z.moveLeft $> ClearYes
            (V.KRight, []) -> editContents ref Z.moveRight $> ClearYes
            (V.KChar 'f', [V.MCtrl]) -> editContents ref Z.moveRight $> ClearYes
            (V.KHome, []) -> editContents ref Z.gotoBOL $> ClearYes
            (V.KEnd, []) -> editContents ref Z.gotoEOL $> ClearYes
            -- Vim movement
            (V.KChar 'h', []) -> editContents ref Z.moveLeft $> ClearYes
            (V.KChar 'j', []) -> editContents ref Z.moveDown $> ClearYes
            (V.KChar 'k', []) -> editContents ref Z.moveUp $> ClearYes
            (V.KChar 'l', []) -> editContents ref Z.moveRight $> ClearYes
            (V.KChar '0', []) -> editContents ref Z.gotoBOL $> ClearYes
            (V.KChar '$', []) -> editContents ref Z.gotoEOL $> ClearYes
            _ -> pure ClearNo
        EdModeInsert ->
          case (key, mods) of
            -- Mode switching
            (V.KEsc, []) -> setEdMode ref EdModeNormal $> ClearNo
            -- Chorded + special key movement
            (V.KChar 'a', [V.MCtrl]) -> editContents ref Z.gotoBOL $> ClearYes
            (V.KChar 'e', [V.MCtrl]) -> editContents ref Z.gotoEOL $> ClearYes
            (V.KChar 'k', [V.MCtrl]) -> editContents ref Z.killToEOL $> ClearYes
            (V.KChar 'u', [V.MCtrl]) -> editContents ref Z.killToBOL $> ClearYes
            (V.KUp, []) -> editContents ref Z.moveUp $> ClearYes
            (V.KChar 'p', [V.MCtrl]) -> editContents ref Z.moveUp $> ClearYes
            (V.KDown, []) -> editContents ref Z.moveDown $> ClearYes
            (V.KChar 'n', [V.MCtrl]) -> editContents ref Z.moveDown $> ClearYes
            (V.KLeft, []) -> editContents ref Z.moveLeft $> ClearYes
            (V.KChar 'b', [V.MCtrl]) -> editContents ref Z.moveLeft $> ClearYes
            (V.KRight, []) -> editContents ref Z.moveRight $> ClearYes
            (V.KChar 'f', [V.MCtrl]) -> editContents ref Z.moveRight $> ClearYes
            (V.KHome, []) -> editContents ref Z.gotoBOL $> ClearYes
            (V.KEnd, []) -> editContents ref Z.gotoEOL $> ClearYes
            -- Editing
            (V.KEnter, []) -> editContents ref Z.breakLine $> ClearYes
            (V.KDel, []) -> editContents ref Z.deleteChar $> ClearYes
            (V.KChar 'd', [V.MCtrl]) -> editContents ref Z.deleteChar $> ClearYes
            (V.KBS, []) -> editContents ref Z.deletePrevChar $> ClearYes
            (V.KChar c, []) -> editContents ref (Z.insertChar c) $> ClearYes
            _ -> pure ClearNo
        EdModeCommand ->
          case (key, mods) of
            -- Mode switching
            (V.KEsc, []) -> setEdMode ref EdModeNormal $> ClearNo
            -- Chorded + special key movement
            (V.KChar 'a', [V.MCtrl]) -> editCommand ref Z.gotoBOL $> ClearYes
            (V.KChar 'e', [V.MCtrl]) -> editCommand ref Z.gotoEOL $> ClearYes
            (V.KChar 'k', [V.MCtrl]) -> editCommand ref Z.killToEOL $> ClearYes
            (V.KChar 'u', [V.MCtrl]) -> editCommand ref Z.killToBOL $> ClearYes
            (V.KLeft, []) -> editCommand ref Z.moveLeft $> ClearYes
            (V.KChar 'b', [V.MCtrl]) -> editCommand ref Z.moveLeft $> ClearYes
            (V.KRight, []) -> editCommand ref Z.moveRight $> ClearYes
            (V.KChar 'f', [V.MCtrl]) -> editCommand ref Z.moveRight $> ClearYes
            (V.KHome, []) -> editCommand ref Z.gotoBOL $> ClearYes
            (V.KEnd, []) -> editCommand ref Z.gotoEOL $> ClearYes
            -- Editing
            (V.KDel, []) -> editCommand ref Z.deleteChar $> ClearYes
            (V.KChar 'd', [V.MCtrl]) -> editCommand ref Z.deleteChar $> ClearYes
            (V.KBS, []) -> editCommand ref Z.deletePrevChar $> ClearYes
            (V.KChar c, []) -> editCommand ref (Z.insertChar c) $> ClearYes
            -- Sending command
            -- TODO enter
            _ -> pure ClearNo
      when (clear == ClearYes) (writeStatus ref "")
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

plainOnAttr :: AttrName
plainOnAttr = "plain" <> "on"

plainOffAttr :: AttrName
plainOffAttr = "plain" <> "off"

myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (plainOnAttr, V.black `on` V.white)
  , (plainOffAttr, V.white `on` V.black)
  ]

mkApp :: SimpleLogAction -> BChan Req -> Maybe FilePath -> App St Ev Name
mkApp _ reqChan mfp = App
  { appDraw = myDraw
  , appChooseCursor = myChoose
  , appHandleEvent = myHandle reqChan
  , appStartEvent = myStart reqChan mfp
  , appAttrMap = const myAttrMap
  }

worker :: SimpleLogAction -> BChan Req -> BChan Ev -> IO ()
worker action reqChan evChan = forever $ do
  req <- liftIO (readBChan reqChan)
  logDebug action ("Worker req: " <> T.pack (show req))
  let Req n reqb = req
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
  let vtyBuilder = V.mkVty V.defaultConfig
  firstVty <- vtyBuilder
  fileSimpleLogAction "/tmp/buh" $ \action -> do
    let app = mkApp action reqChan mfn
        runUi = customMain firstVty vtyBuilder (Just evChan) app mkSt
        runWorker = worker action reqChan evChan
    putStrLn "Starting app"
    _ <- race runUi runWorker
    putStrLn "Stopped app"
