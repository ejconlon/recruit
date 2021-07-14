{-# LANGUAGE OverloadedStrings #-}

{-
See https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst
https://github.com/jtdaugherty/brick/blob/master/programs/CustomEventDemo.hs
https://github.com/NorfairKing/writing-a-text-editor-in-haskell-with-brick
https://hackage.haskell.org/package/brick-0.62/docs/Brick-Main.html#t:App
-}
module Recruit.Internal (mkExe) where

import Brick (App (..), AttrMap, AttrName, BrickEvent (..), CursorLocation, EventM, Location (..), Next,
              ViewportType (..), Widget, attrMap, continue, customMain, getVtyHandle, hBox, halt, showCursor,
              showCursorNamed, textWidth, txt, vBox, vLimit, viewport, visibleRegion, withAttr)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Util (on)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, stateTVar)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, join, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Zipper (TextZipper, currentLine)
import qualified Data.Text.Zipper as Z
import GHC.Generics (Generic)
import qualified Graphics.Vty as V
import LittleLogger.Manual (Severity (Debug), SimpleLogAction, fileSimpleLogAction, logDebug, logWithSeverity)
import Recruit.Completes (Completes, Trie, advanceCompletes, mkCompletes, mkEmptyCompletes, mkTrie, readCompletes)
import Recruit.Interface (CommandNames, CommandStatus (CommandStatusOk), CustomConstraints, CustomDef (..), Iface (..),
                          NextOp (..), UserCommandHandler, UserEventHandler, UserWorker)
import Recruit.Log (Log, markLog, newLog, peekLog, pushLog, textLog)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))

newtype ReqId = ReqId { unReqId :: Int }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Enum, Ord, Num)
  deriving (TextShow) via (FromGeneric ReqId)

data ReqBody req =
    ReqBodyCustom !req
  | ReqBodyOpen !FilePath
  | ReqBodyLog !Severity !Text
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric (ReqBody req))

data ResBody err res =
    ResBodyCustom !res
  | ResBodyOpened !FilePath !Text
  | ResBodyNotFound !FilePath
  | ResBodyError !err
  | ResBodyException !Text
  | ResBodyLog
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric (ResBody err res))

data Req req = Req !ReqId !(ReqBody req)
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric (Req req))

data Res err res = Res !ReqId !(ResBody err res)
  deriving stock (Eq, Show, Generic)
  deriving (TextShow) via (FromGeneric (Res err res))

data Name = NameEditor | NameStatus | NameCommand
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (TextShow) via (FromGeneric Name)

data EdMode =
    EdModeNormal
  | EdModeInsert
  | EdModeCommand
  | EdModeLog
  | EdModeHelp
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (TextShow) via (FromGeneric EdMode)

edModeFocus :: EdMode -> Name
edModeFocus emo =
  case emo of
    EdModeNormal -> NameEditor
    EdModeInsert -> NameEditor
    EdModeCommand -> NameCommand
    EdModeLog -> NameEditor
    EdModeHelp -> NameEditor

edModeText :: EdMode -> Text
edModeText emo =
  case emo of
    EdModeNormal -> "Normal"
    EdModeInsert -> "Insert"
    EdModeCommand -> "Command"
    EdModeLog -> "Log"
    EdModeHelp -> "Help"

data St ctx = St
  { stEdMode :: !EdMode
  , stNextReqId :: !ReqId
  , stFilename :: !(Maybe FilePath)
  , stEditorContents :: !(TextZipper Text)
  , stStatusLog :: !(Log Text)
  , stCommandLine :: !(TextZipper Text)
  , stCommandCompletes :: !(Maybe Completes)
  , stCtx :: !ctx
  } deriving stock (Eq, Show, Generic)
    deriving (TextShow) via (FromGeneric (St ctx))

stFocus :: St ctx -> Name
stFocus = edModeFocus . stEdMode

emptyZipper :: TextZipper Text
emptyZipper = Z.textZipper [] Nothing

mkSt :: ctx -> Int -> St ctx
mkSt ctx maxLog = St EdModeNormal 0 Nothing emptyZipper (newLog maxLog) emptyZipper Nothing ctx

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

renderEditor :: CommandNames -> St ctx -> [Widget Name]
renderEditor commandNames st =
    let emo = stEdMode st
    in case emo of
      EdModeLog ->
        let lg = stStatusLog st
            contents = textLog lg
            cursorLoc = Location (0, 0)
            atCharWidth = 1
            widget = withAttr plainOffAttr $
              viewport NameEditor Both $
              visibleRegion cursorLoc (atCharWidth, 1) $
              vBox . fmap saniTxt $ contents
        in [widget]
      EdModeHelp ->
        let contents = fmap (\(a, b) -> a <> " - " <> b) commandNames
            cursorLoc = Location (0, 0)
            atCharWidth = 1
            widget = withAttr plainOffAttr $
              viewport NameEditor Both $
              visibleRegion cursorLoc (atCharWidth, 1) $
              vBox . fmap saniTxt $ contents
        in [widget]
      _ ->
        let z = stEditorContents st
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

renderStatus :: St ctx -> [Widget Name]
renderStatus st =
  case peekLog (stStatusLog st) of
    Just (line, True) -> [vLimit 1 (viewport NameStatus Horizontal (saniTxt line))]
    _ -> []

renderCommand :: St ctx -> [Widget Name]
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

myDraw :: CommandNames -> St ctx -> [Widget Name]
myDraw commandNames st = stacked (join [renderEditor commandNames st, renderStatus st, renderCommand st])

myChoose :: St ctx -> [CursorLocation Name] -> Maybe (CursorLocation Name)
myChoose = showCursorNamed . stFocus

newReq :: TVar (St ctx) -> ReqBody req -> STM (Req req)
newReq ref reqb = stateTVar ref $ \st ->
  let n = stNextReqId st
      req = Req n reqb
      st' = st { stNextReqId = succ n }
  in (req, st')

writeStatus :: TVar (St ctx) -> Text -> STM ()
writeStatus ref line = modifyTVar' ref (\st -> st { stStatusLog = pushLog (stStatusLog st) line })

clearStatus :: TVar (St ctx) -> STM ()
clearStatus ref = modifyTVar' ref (\st -> st { stStatusLog = markLog (stStatusLog st) })

setContents :: TVar (St ctx) -> Text -> STM ()
setContents ref contents = modifyTVar' ref (\st -> st { stEditorContents = Z.textZipper (T.lines contents) Nothing })

setEdMode :: TVar (St ctx) -> EdMode -> STM ()
setEdMode ref emo = do
  modifyTVar' ref $ \st ->
    -- If moving away from command focus, clear command line and completes too
    if emo /= EdModeCommand && stEdMode st == EdModeCommand
      then st { stEdMode = emo, stCommandLine = Z.textZipper [] Nothing, stCommandCompletes = Nothing }
      else st { stEdMode = emo }
  writeStatus ref (edModeText emo <> " mode")

editContents :: TVar (St ctx) -> (TextZipper Text -> TextZipper Text) -> STM ()
editContents ref f = modifyTVar' ref (\st -> st { stEditorContents = f (stEditorContents st) })

editCommand :: TVar (St ctx) -> (TextZipper Text -> TextZipper Text) -> STM ()
editCommand ref f = modifyTVar' ref (\st -> st { stCommandLine = f (stCommandLine st), stCommandCompletes = Nothing })

tabComplete :: TVar (St ctx) -> Trie -> STM ()
tabComplete ref commandTrie = modifyTVar' ref $ \st ->
  let completes' =
        case stCommandCompletes st of
          Nothing ->
            let query = T.intercalate "\n" (Z.getText (stCommandLine st))
            in if T.null query
              then mkEmptyCompletes ["help", "quit"]
              else mkCompletes commandTrie query
          Just completes -> advanceCompletes completes
      newText = readCompletes completes'
  in st { stCommandLine = Z.gotoEOF (Z.textZipper (T.lines newText) Nothing), stCommandCompletes = Just completes' }

addOutgoing :: TVar (St ctx) -> TQueue (Req req) -> ReqBody req -> STM ReqId
addOutgoing ref outgoing rb = do
  r@(Req n _) <- newReq ref rb
  writeTQueue outgoing r
  pure n

writeLog :: TVar (St ctx) -> TQueue (Req req) -> Severity -> Text -> STM ()
writeLog ref outgoing sev msg = void (addOutgoing ref outgoing (ReqBodyLog sev msg))

getCtx :: TVar (St ctx) -> STM ctx
getCtx = fmap stCtx . readTVar

putCtx :: TVar (St ctx) -> ctx -> STM ()
putCtx ref ctx = modifyTVar' ref (\st -> st { stCtx = ctx })

data Clear =
    ClearYes
  | ClearNo
  | ClearQuit
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (TextShow) via (FromGeneric Clear)

opToClear :: NextOp -> Clear
opToClear op =
  case op of
    NextOpContinue -> ClearNo
    NextOpHalt -> ClearQuit

data UiSt ctx req = UiSt
  { uiStIface :: !(Iface ctx req)
  , uiStRef :: !(TVar (St ctx))
  , uiStQueue :: !(TQueue (Req req))
  }

mkIface :: TVar (St ctx) -> TQueue (Req req) -> Iface ctx req
mkIface ref outgoing = Iface
  { ifaceSendReq = void . addOutgoing ref outgoing . ReqBodyCustom
  , ifaceSendLog = writeLog ref outgoing
  , ifaceWriteStatus = writeStatus ref
  , ifaceGetCtx = getCtx ref
  , ifacePutCtx = putCtx ref
  }

mkUiSt :: TVar (St ctx) -> TQueue (Req req) -> UiSt ctx req
mkUiSt ref outgoing = UiSt (mkIface ref outgoing) ref outgoing

type CommandHandler ctx req = UiSt ctx req -> Text -> STM NextOp

type EventHandler err ctx req res = UiSt ctx req -> BrickEvent Name (Res err res) -> STM NextOp

defaultCommandNames :: CommandNames
defaultCommandNames =
  [ ("help", "print all available commands")
  , ("quit", "quit without saving")
  , ("log", "display the status log")
  , ("scroll-top", "scroll the viewport to the top")
  , ("scroll-bottom", "scroll the viewport to the bottom")
  , ("parse-forward", "step the parser forward")
  , ("parse-backward", "step the parser backward")
  ]

mkCommandHandler :: UserCommandHandler ctx req -> CommandHandler ctx req
mkCommandHandler userCommandHandler (UiSt iface ref _) cmd = do
  (op, ok, mode) <- case T.words cmd of
    [] -> pure (NextOpContinue, CommandStatusOk, EdModeNormal)
    ["help"] -> pure (NextOpContinue, CommandStatusOk, EdModeHelp)
    ["quit"] -> pure (NextOpHalt, CommandStatusOk, EdModeNormal)
    ["log"] -> pure (NextOpContinue, CommandStatusOk, EdModeLog)
    ["scroll-top"] -> editContents ref Z.gotoBOF $> (NextOpContinue, CommandStatusOk, EdModeNormal)
    ["scroll-bottom"] -> editContents ref Z.gotoEOF $> (NextOpContinue, CommandStatusOk, EdModeNormal)
    ["parse-forward"] -> error "TODO"
    ["parse-backward"] -> error "TODO"
    _ -> do
      (op, ok) <- userCommandHandler iface cmd
      pure (op, ok, EdModeNormal)
  setEdMode ref mode
  unless (ok == CommandStatusOk) (writeStatus ref ("Bad command: " <> cmd))
  pure op

sendLog :: Iface ctx req -> Severity -> Text -> STM ()
sendLog iface sev msg = void (ifaceSendLog iface sev msg)

withStateLog :: TextShow b => UiSt ctx req -> Text -> (St ctx -> b) -> STM a -> STM a
withStateLog (UiSt iface ref _) name ex act = do
  let sev = Debug
  sendLog iface sev (name <> " - before")
  st0 <- readTVar ref
  let v0 = ex st0
  sendLog iface sev (showt v0)
  res <- act
  sendLog iface sev (name <> " - after")
  st1 <- readTVar ref
  let v1 = ex st1
  sendLog iface sev (showt v1)
  pure res

mkEventHandler :: TextShow err => Trie -> CommandHandler ctx req -> UserEventHandler ctx req res -> EventHandler err ctx req res
mkEventHandler commandTrie commandHandler userEventHandler uiSt@(UiSt iface ref _) ev =
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
            -- Tab completion
            (V.KChar '\t', []) -> tabComplete ref commandTrie $> ClearNo
            -- Editing
            (V.KDel, []) -> editCommand ref Z.deleteChar $> ClearYes
            (V.KChar 'd', [V.MCtrl]) -> editCommand ref Z.deleteChar $> ClearYes
            (V.KBS, []) -> editCommand ref Z.deletePrevChar $> ClearYes
            (V.KChar c, []) -> editCommand ref (Z.insertChar c) $> ClearYes
            (V.KEnter, []) -> do
              cmd <- fmap (currentLine . stCommandLine) (readTVar ref)
              fmap opToClear (commandHandler uiSt cmd)
            _ -> pure ClearNo
        _ ->
          -- Help and log modes - colon to command mode, otherwise to normal
          case (key, mods) of
            (V.KChar ':', []) -> setEdMode ref EdModeCommand $> ClearNo
            _ -> setEdMode ref EdModeNormal $> ClearNo
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
            ResBodyError msg -> do
              writeStatus ref ("Error: " <> showt msg)
              pure NextOpContinue
            ResBodyException msg -> do
              writeStatus ref ("Exception: " <> msg)
              pure NextOpContinue
            ResBodyLog -> pure NextOpContinue
    -- Skip the rest
    _ -> pure NextOpContinue

runWithUiSt :: MonadIO m => St ctx -> (UiSt ctx req -> STM a) -> m (a, St ctx, [Req req])
runWithUiSt st f = liftIO $ do
  -- TODO massage this to not create vars every invocation. Possbly thread state and ref through brick state.
  ref <- newTVarIO st
  outgoing <- newTQueueIO
  let uiSt = mkUiSt ref outgoing
  atomically $ do
    res <- f uiSt
    st' <- readTVar ref
    outList <- flushTQueue outgoing
    pure (res, st', outList)

sendOutgoing :: MonadIO m => BChan (Req req) -> [Req req] -> m ()
sendOutgoing reqChan outgoing = liftIO (for_ outgoing (writeBChan reqChan))

myStart :: BChan (Req req) -> Maybe FilePath -> St ctx -> EventM Name (St ctx)
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
      (_, st', outList) <- runWithUiSt st $ \(UiSt _ ref outgoing) -> do
        writeStatus ref ("Opening " <> T.pack fp)
        void (addOutgoing ref outgoing (ReqBodyOpen fp))
      sendOutgoing reqChan outList
      pure st'

myHandle :: EventHandler err ctx req res -> SimpleLogAction -> BChan (Req req) -> St ctx -> BrickEvent Name (Res err res) -> EventM Name (Next (St ctx))
myHandle eventHandler _ reqChan st ev = do
  (nextOp, st', outList) <- runWithUiSt st (`eventHandler` ev)
  case nextOp of
    NextOpHalt -> halt st'
    NextOpContinue -> do
      sendOutgoing reqChan outList
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

mkApp :: CommandNames -> EventHandler err ctx req res -> SimpleLogAction -> BChan (Req req) -> Maybe FilePath -> App (St ctx) (Res err res) Name
mkApp commandNames eventHandler action reqChan mfp = App
  { appDraw = myDraw commandNames
  , appChooseCursor = myChoose
  , appHandleEvent = myHandle eventHandler action reqChan
  , appStartEvent = myStart reqChan mfp
  , appAttrMap = const myAttrMap
  }

mkWorker :: TextShow req => UserWorker err req res -> SimpleLogAction -> BChan (Req req) -> BChan (Res err res) -> IO ()
mkWorker userWorker action reqChan evChan = forever go where
  go = do
    req <- readBChan reqChan
    logDebug action ("Worker req: " <> showt req)
    let Req n reqb = req
    resb <- handle @SomeException (pure . ResBodyException . T.pack . show) $ do
      case reqb of
        ReqBodyCustom creq -> do
          e <- userWorker action creq
          pure (either ResBodyError ResBodyCustom e)
        ReqBodyOpen fp -> do
          e <- doesFileExist fp
          if e
            then fmap (ResBodyOpened fp) (TIO.readFile fp)
            else pure (ResBodyNotFound fp)
        ReqBodyLog sev msg -> logWithSeverity action sev msg $> ResBodyLog
    writeBChan evChan (Res n resb)

mkExe :: CustomConstraints err ctx req res => CustomDef err ctx req res -> IO ()
mkExe (CustomDef userCommandNames userCommandHandler userEventHandler userWorker _ emptyCtx) = do
  let commandHandler = mkCommandHandler userCommandHandler
      commandNames = defaultCommandNames <> userCommandNames
      commandTrie = mkTrie (fmap fst commandNames)
  let eventHandler = mkEventHandler commandTrie commandHandler userEventHandler
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
    let app = mkApp commandNames eventHandler action reqChan mfn
        st = mkSt emptyCtx maxLogLen
        runUi = customMain firstVty vtyBuilder (Just evChan) app st
        runWorker = mkWorker userWorker action reqChan evChan
    logDebug action "Starting app"
    _ <- race runUi runWorker
    logDebug action "Stopped app"
