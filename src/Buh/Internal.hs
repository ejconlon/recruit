{-# LANGUAGE OverloadedStrings #-}

{-
See https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst
https://github.com/jtdaugherty/brick/blob/master/programs/CustomEventDemo.hs
https://github.com/NorfairKing/writing-a-text-editor-in-haskell-with-brick
https://hackage.haskell.org/package/brick-0.62/docs/Brick-Main.html#t:App
-}
module Buh.Internal (mkExe) where

import Brick (App (..), AttrMap, AttrName, BrickEvent (..), CursorLocation, EventM, Location (..), Next,
              ViewportType (..), Widget, attrMap, continue, customMain, getVtyHandle, hBox, halt, showCursor,
              showCursorNamed, textWidth, txt, vBox, vLimit, viewport, visibleRegion, withAttr)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Util (on)
import Brick.Widgets.Border (hBorder)
import Buh.Interface (CustomDef (..), Iface (..), NextOp (..), ReqBody (..), ReqId, ResBody (..), UserCommandHandler,
                      UserEventHandler, UserWorker)
import Buh.Log (Log, markLog, newLog, peekLog, pushLog)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, join, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (intersperse)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Zipper (TextZipper, currentLine)
import qualified Data.Text.Zipper as Z
import qualified Graphics.Vty as V
import LittleLogger.Manual (SimpleLogAction, fileSimpleLogAction, logDebug)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)

data Name = NameEditor | NameStatus | NameCommand
  deriving stock (Eq, Ord, Show)

data Req req = Req !ReqId !(ReqBody req)
  deriving stock (Eq, Show)

data Res res = Res !ReqId !(ResBody res)
  deriving stock (Eq, Show)

data EdMode =
    EdModeNormal
  | EdModeInsert
  | EdModeCommand
  deriving stock (Eq, Show)

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
  , stNextReqId :: !ReqId
  , stFilename :: !(Maybe FilePath)
  , stEditorContents :: !(TextZipper Text)
  , stStatusLog :: !(Log Text)
  , stCommandLine :: !(TextZipper Text)
  } deriving (Eq, Show)

stFocus :: St -> Name
stFocus = edModeFocus . stEdMode

emptyZipper :: TextZipper Text
emptyZipper = Z.textZipper [] Nothing

mkSt :: Int -> St
mkSt maxLog = St EdModeNormal 0 Nothing emptyZipper (newLog maxLog) emptyZipper

stacked :: [Widget Name] -> [Widget Name]
stacked widgets = [vBox (intersperse hBorder widgets)]

-- TODO there should be a way to cache this...
-- (And support tab width > 1)
saniTxt :: Text -> Widget Name
saniTxt t = txt $
  if T.null t
    then " "
    else if T.any (== '\t') t
      then T.map (\c -> if c == '\t' then ' ' else c) t
      else t

charAtCursor :: TextZipper Text -> Maybe Text
charAtCursor z =
    let col = snd (Z.cursorPosition z)
        curLine = Z.currentLine z
        toRight = T.drop col curLine
    in if T.length toRight > 0
       then Just (T.take 1 toRight)
       else Nothing

previewEditor :: MonadIO m => SimpleLogAction -> TVar St -> m ()
previewEditor action ref = do
  st <- liftIO (readTVarIO ref)
  let z = stEditorContents st
      emo = stEdMode st
      foc = emo /= EdModeCommand
      (row, col) = Z.cursorPosition z
      toLeft = T.take col (Z.currentLine z)
      cursorLoc = Location (textWidth toLeft, row)
      atChar = charAtCursor z
      atCharWidth = maybe 1 textWidth atChar
  logDebug action (T.intercalate " " (fmap T.pack ["EDITOR", show emo, show foc, show row, show col, show toLeft, show cursorLoc, show atChar, show atCharWidth]))

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
  case peekLog (stStatusLog st) of
    Just (line, True) -> [vLimit 1 (viewport NameStatus Horizontal (saniTxt line))]
    _ -> []

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
            viewport NameCommand Horizontal $
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

newReq :: TVar St -> ReqBody req -> STM (Req req)
newReq ref reqb = stateTVar ref $ \st ->
  let n = stNextReqId st
      req = Req n reqb
      st' = st { stNextReqId = succ n }
  in (req, st')

writeStatus :: TVar St -> Text -> STM ()
writeStatus ref line = modifyTVar' ref (\st -> st { stStatusLog = pushLog (stStatusLog st) line })

clearStatus :: TVar St -> STM ()
clearStatus ref = modifyTVar' ref (\st -> st { stStatusLog = markLog (stStatusLog st) })

setContents :: TVar St -> Text -> STM ()
setContents ref contents = modifyTVar' ref (\st -> st { stEditorContents = Z.textZipper (T.lines contents) Nothing })

setEdMode :: TVar St -> EdMode -> STM ()
setEdMode ref emo = do
  modifyTVar' ref $ \st ->
    -- If moving away from command focus, clear command line
    let comLine = if emo /= EdModeCommand && stEdMode st == EdModeCommand then Z.textZipper [] Nothing else stCommandLine st
    in st { stEdMode = emo, stCommandLine = comLine }
  writeStatus ref (edModeText emo <> " mode")

editContents :: TVar St -> (TextZipper Text -> TextZipper Text) -> STM ()
editContents ref f = modifyTVar' ref (\st -> st { stEditorContents = f (stEditorContents st) })

editCommand :: TVar St -> (TextZipper Text -> TextZipper Text) -> STM ()
editCommand ref f = modifyTVar' ref (\st -> st { stCommandLine = f (stCommandLine st) })

addOutgoing :: TVar St -> TVar (Seq (Req req)) -> ReqBody req -> STM ReqId
addOutgoing ref outgoing rb = do
  r@(Req n _) <- newReq ref rb
  modifyTVar' outgoing (:|> r)
  pure n

data Clear = ClearYes | ClearNo | ClearQuit deriving (Eq, Show)

opToClear :: NextOp -> Clear
opToClear op =
  case op of
    NextOpContinue -> ClearNo
    NextOpHalt -> ClearQuit

type CommandHandler req = Iface req -> TVar St -> Text -> STM NextOp

type EventHandler req res = Iface req -> TVar St -> BrickEvent Name (Res res) -> STM NextOp

mkCommandHandler :: UserCommandHandler req -> CommandHandler req
mkCommandHandler userCommandHandler iface ref cmd = do
  (op, ok) <- case T.words cmd of
    [] -> pure (NextOpContinue, True)
    ["top"] -> editContents ref Z.gotoBOF $> (NextOpContinue, True)
    ["bottom"] -> editContents ref Z.gotoEOF $> (NextOpContinue, True)
    ["quit"] -> pure (NextOpHalt, True)
    _ -> userCommandHandler iface cmd
  setEdMode ref EdModeNormal
  unless ok (writeStatus ref ("Bad command: " <> cmd))
  pure op

mkEventHandler :: CommandHandler req -> UserEventHandler req res -> EventHandler req res
mkEventHandler commandHandler userEventHandler iface ref ev =
  case ev of
    -- Hard quit: Left option + escape, or ctrl-c
    VtyEvent (V.EvKey V.KEsc [V.MMeta]) -> pure NextOpHalt
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> pure NextOpHalt
    -- Handle rest of keyboard input
    VtyEvent (V.EvKey key mods) -> do
      emo <- fmap stEdMode (readTVar ref)
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
            (V.KEnter, []) -> do
              cmd <- fmap (currentLine . stCommandLine) (readTVar ref)
              fmap opToClear (commandHandler iface ref cmd)
            _ -> pure ClearNo
      case clear of
        ClearYes -> clearStatus ref $> NextOpContinue
        ClearNo -> pure NextOpContinue
        ClearQuit -> pure NextOpHalt
    -- Handle worker events
    AppEvent aev -> do
      case aev of
        Res _ resb ->
          case resb of
            ResBodyCustom cres -> userEventHandler iface cres
            ResBodyOpened fp contents -> do
              setContents ref contents
              writeStatus ref ("Loaded: " <> T.pack fp)
              pure NextOpContinue
            ResBodyNotFound fp -> do
              writeStatus ref ("File not found: " <> T.pack fp)
              pure NextOpContinue
            ResBodyErr msg -> do
              writeStatus ref ("Error: " <> msg)
              pure NextOpContinue
    -- Skip the rest
    _ -> pure NextOpContinue

runWithIface :: MonadIO m => St -> (Iface req -> TVar St -> STM a) -> m (a, St, Seq (Req req))
runWithIface st f = liftIO $ do
  ref <- newTVarIO st
  outgoing <- newTVarIO Seq.empty
  let iface = Iface (addOutgoing ref outgoing) (writeStatus ref)
  res <- atomically (f iface ref)
  st' <- readTVarIO ref
  outgoing' <- readTVarIO outgoing
  pure (res, st', outgoing')

sendOutgoing :: MonadIO m => BChan (Req req) -> Seq (Req req) -> m ()
sendOutgoing reqChan outgoing = liftIO (for_ outgoing (writeBChan reqChan))

myStart :: BChan (Req req) -> Maybe FilePath -> St -> EventM Name St
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
    Just fp -> do
      (_, st', outgoing) <- runWithIface st $ \iface _ -> do
        ifaceWriteStatus iface ("Opening " <> T.pack fp)
        ifaceSend iface (ReqBodyOpen fp)
      sendOutgoing reqChan outgoing
      pure st'

myHandle :: EventHandler req res -> SimpleLogAction -> BChan (Req req) -> St -> BrickEvent Name (Res res) -> EventM Name (Next St)
myHandle eventHandler _ reqChan st ev = do
  (nextOp, st', outgoing) <- runWithIface st (\iface ref -> eventHandler iface ref ev)
  case nextOp of
    NextOpHalt -> halt st'
    NextOpContinue -> do
      sendOutgoing reqChan outgoing
      continue st'

plainOnAttr :: AttrName
plainOnAttr = "plain" <> "on"

plainOffAttr :: AttrName
plainOffAttr = "plain" <> "off"

myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (plainOnAttr, V.black `on` V.white)
  , (plainOffAttr, V.white `on` V.black)
  ]

mkApp :: EventHandler req res -> SimpleLogAction -> BChan (Req req) -> Maybe FilePath -> App St (Res res) Name
mkApp eventHandler action reqChan mfp = App
  { appDraw = myDraw
  , appChooseCursor = myChoose
  , appHandleEvent = myHandle eventHandler action reqChan
  , appStartEvent = myStart reqChan mfp
  , appAttrMap = const myAttrMap
  }

mkWorker :: Show req => UserWorker req res -> SimpleLogAction -> BChan (Req req) -> BChan (Res res) -> IO ()
mkWorker userWorker action reqChan evChan = forever go where
  go = do
    req <- readBChan reqChan
    logDebug action ("Worker req: " <> T.pack (show req))
    let Req n reqb = req
    resb <- handle @SomeException (pure . ResBodyErr . T.pack . show) $ do
      case reqb of
        ReqBodyCustom creq -> userWorker action creq
        ReqBodyOpen fp -> do
          e <- doesFileExist fp
          if e
            then fmap (ResBodyOpened fp) (TIO.readFile fp)
            else pure (ResBodyNotFound fp)
    writeBChan evChan (Res n resb)

mkExe :: Show req => CustomDef req res -> IO ()
mkExe (CustomDef userCommandHandler userEventHandler userWorker) = do
  let commandHandler = mkCommandHandler userCommandHandler
  let eventHandler = mkEventHandler commandHandler userEventHandler
  args <- getArgs
  mfn <- case args of
    [] -> pure Nothing
    [fn] -> pure (Just fn)
    _ -> die "Use: buffer.hs [filename]"
  let maxChanLen = 1000
      maxLogLen = 100
  reqChan <- newBChan maxChanLen
  evChan <- newBChan maxChanLen
  let vtyBuilder = V.mkVty V.defaultConfig
  firstVty <- vtyBuilder
  fileSimpleLogAction "/tmp/buh" $ \action -> do
    let app = mkApp eventHandler action reqChan mfn
        st = mkSt maxLogLen
        runUi = customMain firstVty vtyBuilder (Just evChan) app st
        runWorker = mkWorker userWorker action reqChan evChan
    logDebug action "Starting app"
    _ <- race runUi runWorker
    logDebug action "Stopped app"
