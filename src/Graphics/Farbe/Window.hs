{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}

{-| A basic window interface for running a single window.
Has to run on main thread and is not thread-safe.

This module provides only a fraction of GLFW's abilities.
If you are missing functionality,
you should copy/fork this module and adjust it to your needs.

-}


{- TODO:
* add usage examples in general and per function
* add swapBuffers to processEvents and add an alternative without buffer swap
* bracketed window creation
* hide window on close callback
-}


module Graphics.Farbe.Window
	-- * Window creation
	( runWindowT
	, Display (..)
	, WindowT (..)
	, MonadWindow
	, swapBuffers
	-- * Event processing
	, processEvents
	, shouldWindowClose
	, Event (..)
	, EventContext
	-- ** Cursor mode
	, setCursorMode
	, getCursorMode
	, CursorMode (..)
	, KeyState (..)
	-- ** GLFW re-export
	, W.Key (..)
	, W.MouseButton (..)
	-- * Error handling
	, WindowErr (..)
	-- * Utility
	, getTime
	, windowSize
	, glfwWindow
	-- ~ -- module re-export
	-- ~ , module Control.Monad
	-- ~ , module Control.Monad.IO.Class
	) where

import qualified Graphics.UI.GLFW as W
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.List (find)
import GHC.Clock
import GHC.Float
import System.Mem (performGC)
import qualified Data.Set as S

-- ~ import Graphics.GL

-- for transformer instances
import Control.Monad.State (StateT, MonadState)
import Control.Monad.Writer (WriterT, MonadWriter)
import Control.Monad.RWS (RWST)
import qualified Control.Monad.State.Strict as Strict (StateT)
import qualified Control.Monad.Writer.Strict as Strict (WriterT)
import qualified Control.Monad.RWS.Strict as Strict (RWST)
-- ~ import Control.Monad.Cont (ContT, MonadCont)
-- ~ import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Zip (MonadZip)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (Alternative)
-- ~ import Control.Monad.IO.Class

import Data.Bits

-- | Creates a fullscreen window, runs your action and terminates window after.
runWindowT :: MonadIO m => String -> Display -> WindowT m a -> m a
runWindowT s d m = do
	liftIO $ W.swapInterval 1 -- test rendering times using gl query methods instead
	ws <- createWindow s d
	f <- toBeRun $ W.destroyWindow $ wsGlfwWindow ws
	r <- runReaderT (runw $ m) ws
	f
	return r


data Display
	= Fullscreen
	| InWindow (Int,Int)
	| InWindowAt (Int,Int) (Int,Int)
	deriving (Read, Show, Eq)



-- Lift this entire pack into a failable monad. ErrGLFWInitFailed often drops when starting from a different thread than main. TODO
createWindow :: MonadIO m => String -> Display -> m WindowState
createWindow st d = do
	setErrorCallback $ Just $ \e s' -> throwIO $ toErr (e,s')
	b <- liftIO $ W.init
	when (not b) $ throwIO' ErrGLFWInitFailed

	windowHint $ W.WindowHint'DepthBits (Just 16)
	windowHint $ W.WindowHint'ClientAPI W.ClientAPI'OpenGLES
	windowHint $ W.WindowHint'ContextVersionMajor 2
	-- ~ windowHint $ W.WindowHint'ContextVersionMinor 2
	w <- case d of
		Fullscreen -> do
			e <- getMonitor
			W.VideoMode mx my _ _ _ _ <- getVideoMode e
			windowHint $ W.WindowHint'Decorated False
			glfwCreateWindow mx my st (Just e) Nothing
		InWindow (x,y) -> glfwCreateWindow x y st Nothing Nothing
		InWindowAt (x,y) (px,py) -> do
			w <- glfwCreateWindow x y st Nothing Nothing
			liftIO $ W.setWindowPos w px py
			return w

	liftIO $ W.makeContextCurrent (Just w)

	mes <- liftIO $ newMVar []
	mc <- liftIO $ newMVar S.empty
	t0 <- liftIO $ getMonotonicTime
	mm <- liftIO $ newMVar (0,0)

	let hsw = WindowState w mes mc t0 mm

	liftIO $ do
		let throwEvent e = mlAdd mes e

		W.setWindowCloseCallback w $ Just $ \_ -> throwEvent $ EventClose
		W.setKeyCallback w $ Just $ \_ k i kst _ -> do
			s <- fromMaybe "<?>" <$> W.getKeyName k i
			case kst of
				W.KeyState'Pressed -> throwEvent $ EventKey k Down s
				W.KeyState'Released -> throwEvent $ EventKey k Up s
				W.KeyState'Repeating -> throwEvent $ EventKey k Down s
		W.setCharCallback w $ Just $ \_ c -> throwEvent $ EventTyping c
		W.setMouseButtonCallback w $ Just $ \_ m mst _ -> do
			(x,y) <- W.getCursorPos w
			throwEvent $ EventMouseKey (d2f x, d2f y) m (convertMouseKeyState mst)
		W.setCursorPosCallback w $ Just $ \_ x y -> throwEvent $ EventMouseMove (d2f x, d2f y)
		W.setScrollCallback w $ Just $ \_ x y -> throwEvent $ EventScroll (d2f x) (d2f y)
		W.setDropCallback w $ Just $ \_ ss -> throwEvent $ EventDrop ss
		W.setWindowFocusCallback w $ Just $ \_ bb -> throwEvent $ EventFocus bb
		W.setCursorEnterCallback w $ Just $ \_ c ->
			throwEvent $ EventMouseInOutWindow $ c == W.CursorState'InWindow

	return $ hsw
	where
		windowHint h = liftIO $ W.windowHint h
		getMonitor = do
			mm <- (join . fmap listToMaybe) <$> liftIO W.getMonitors
			mte ErrNoMonitor mm
		getVideoMode m = do
			v <- liftIO $ W.getVideoMode m
			mte ErrNoVideoMode v
		glfwCreateWindow x y s a b = do
			w <- liftIO $ W.createWindow x y s a b
			mte ErrWindowCreationFailed w
		-- ~ makeContextCurrent w = liftIO $ W.makeContextCurrent w
		-- ~ swapInterval n = liftIO $ W.swapInterval n
		setErrorCallback f = liftIO $ W.setErrorCallback f

d2f :: Double -> Float
d2f = double2Float

mte :: MonadIO m => WindowErr -> Maybe a -> m a
mte e = maybe (throwIO' e) return

throwIO' :: (MonadIO m, Exception e) => e -> m a
throwIO' = liftIO . throwIO

convertMouseKeyState :: W.MouseButtonState -> KeyState
convertMouseKeyState W.MouseButtonState'Released = Up
convertMouseKeyState _ = Down

convertKeyState :: W.KeyState -> KeyState
convertKeyState W.KeyState'Released = Up
convertKeyState _ = Down

toErr :: (W.Error, String) -> WindowErr
toErr (e,s) = case e of
	W.Error'NotInitialized -> ErrGLFWInitFailed
	W.Error'NoCurrentContext -> ErrNoContext
	W.Error'ApiUnavailable -> ErrApiUnavailable
	W.Error'VersionUnavailable -> ErrVersionUnavailable
	W.Error'PlatformError -> ErrPlatformError
	_ -> ErrElse s


-- EVENTS ----------------------------------------------------------------------

type EventContext = S.Set (Either W.MouseButton W.Key)

eventContextFromGLFW :: MonadWindow m => m EventContext
eventContextFromGLFW = do
	kk <- getKeys
	mk <- getMouseKeys
	return $ S.fromList $ map Right kk ++ map Left mk

toEventContext1 :: MonadWindow m => EventContext -> Event -> m EventContext
toEventContext1 _ (EventFocus True) = eventContextFromGLFW
toEventContext1 c e = return $ case e of
	(EventKey k Down _) -> S.insert (Right k) c
	(EventKey k Up _) -> S.delete (Right k) c
	(EventMouseKey _ k Down) -> S.insert (Left k) c
	(EventMouseKey _ k Up) -> S.delete (Left k) c
	_ -> error "idk events"

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM f y (x:xs) = do
	y' <- f y x
	ys <- scanM f y' xs
	return $ y' : ys
scanM _ _ [] = return []

toEventContext :: MonadWindow m => [Event] -> m [(Event, EventContext)]
toEventContext [] = return []
toEventContext es = do
	mc <- wsEventContext <$> windowState
	c <- liftIO $ readMVar mc
	cs <- scanM toEventContext1 c es
	liftIO $ void $ swapMVar mc $ last $ cs
	return $ zip es cs

-- | Process and fetch events.
processEvents :: MonadWindow m => m [(Event, EventContext)]
processEvents = do
	eq <- eventQueue
	liftIO $ W.pollEvents
	es <- reverse <$> mlRemoveAll eq >>= eventsOnLocked
	ecs <- toEventContext es
	return ecs

-- | Process and fetch events.
--   Delivers Events to a function.
--   The function is run, unless window is signaled to be closed.
-- ~ processEvents :: MonadWindow m => ([(Event, EventContext)] -> m ()) -> m ()
-- ~ processEvents f = do
	-- ~ es <- processEvents'
	-- ~ w <- glfwWindow
	-- ~ b <- liftIO $ W.windowShouldClose w
	-- ~ when (not $ b || elem EventClose (map fst es)) $ f es

shouldWindowClose :: MonadWindow m => m Bool
shouldWindowClose = do
	w <- glfwWindow
	liftIO $ W.windowShouldClose w



data Event
	 -- | The Key is mapped to US keyboard layout and String is the localized name of the key.
	= EventKey W.Key KeyState String
	 -- | Localized character. Use these for text fields.
	| EventTyping Char
	-- | Coordinate origin (0,0) is left top corner.
	| EventMouseKey (Float, Float) W.MouseButton KeyState
	| EventMouseMove (Float, Float)
	-- | See locked state.
	| EventMouseKeyLocked W.MouseButton KeyState
	| EventMouseMoveLocked (Float, Float)
	| EventScroll Float Float
	| EventFocus Bool
	| EventMouseInOutWindow Bool
	| EventDrop [String]
	| EventClose
	deriving (Read, Show, Eq)

data KeyState = Down | Up deriving (Read, Show, Eq)

ksToBool :: KeyState -> Bool
ksToBool Down = True
ksToBool Up = False


eventsOnLocked :: MonadWindow m => [Event] -> m [Event]
eventsOnLocked es = do
	es' <- mapM eventOnLocked es
	updateLastCoord
	return es'
	where
	eventOnLocked :: MonadWindow m => Event -> m Event
	eventOnLocked e = do
		m <- getCursorMode
		case (m, e) of
			(CursorLocked, EventMouseMove (x1,y1)) -> do
				(x0,y0) <- liftIO . readMVar =<< wsLastCoord <$> windowState
				return $ EventMouseMoveLocked (x1 - x0, y1 - y0)
			(CursorLocked, EventMouseKey _ k ks) -> return $ EventMouseKeyLocked k ks
			_ -> return e

	updateLastCoord :: MonadWindow m => m ()
	updateLastCoord = do
		w <- glfwWindow
		(x,y) <- liftIO $ W.getCursorPos w
		mxy <- wsLastCoord <$> windowState
		liftIO $ modifyMVar_ mxy (return . const (d2f x, d2f y))
-- getting the last coord from glfw instead of the last event could probably cause inaccuracies.
-- it seems flawed. needs fix. TODO
-- probably just move the coord update into the case statement


-- | Ask whether a key is still pressed.
--   Use this to get additional context, when processing events.
--   E.g. if shift is also pressed, when a key is fired.
getKey :: MonadWindow m => W.Key -> m KeyState
getKey k = do
	w <- glfwWindow
	liftIO $ convertKeyState <$> W.getKey w k

getMouseKey :: MonadWindow m => W.MouseButton -> m KeyState
getMouseKey k = do
	w <- glfwWindow
	liftIO $ convertMouseKeyState <$> W.getMouseButton w k

-- | Get all keys that are pressed down.
getKeys :: MonadWindow m => m [W.Key]
getKeys = filterM (fmap ksToBool . getKey) [succ minBound..maxBound]

getMouseKeys :: MonadWindow m => m [W.MouseButton]
getMouseKeys = filterM (fmap ksToBool . getMouseKey) [minBound..maxBound]




data CursorMode = CursorNormal | CursorHidden | CursorLocked
	deriving (Read, Show, Eq)

-- | Set cursor input mode.
--   When locked, the mouse cursor is hidden and can't leave the window.
--   Mouse move and clicks will give locked variants of events instead.
--   Unlike normal mouse position info from events,
--   locked mouse move gives relative position since last event processing.
setCursorMode :: MonadWindow m => CursorMode -> m ()
setCursorMode t = do
	w <- glfwWindow
	liftIO $ W.setCursorInputMode w $ translate' fst snd cmtable t

getCursorMode :: MonadWindow m => m CursorMode
getCursorMode = do
	w <- glfwWindow
	liftIO $ translate' snd fst cmtable <$> W.getCursorInputMode w

cmtable :: [(CursorMode,W.CursorInputMode)]
cmtable =
	[ (CursorNormal, W.CursorInputMode'Normal)
	, (CursorHidden, W.CursorInputMode'Hidden)
	, (CursorLocked, W.CursorInputMode'Disabled)
	]


-- | Translating between values from arbitrary data.
translate :: (Foldable t, Eq b) => (a -> b) -> (a -> c) -> t a -> b -> Maybe c
translate f g t x = fmap g $ find ((==x) . f) t

translate' :: (Foldable t, Eq b) => (a -> b) -> (a -> c) -> t a -> b -> c
translate' f g t x = fromJust $ translate f g t x
-- can probably be rewritten or just replaced in its use spots TODO



-- USER UTILITY ----------------------------------------------------------------

-- | Returns time in seconds since window launch.
getTime :: (MonadWindow m, Fractional a) => m a
getTime = do
	t0 <- wsStartTime <$> windowState
	t <- liftIO $ getMonotonicTime
	return $ realToFrac $ t - t0
-- There is a time function in GLFW,
-- it is counting from GLFW init and can return Nothing,
-- indicating that it may fail?




-- MONAD AND INSTANCES ---------------------------------------------------------

data WindowState = WindowState
  { wsGlfwWindow :: W.Window
  , wsEventQueue :: MVar [Event]
  , wsEventContext :: MVar EventContext
  , wsStartTime :: Double
  , wsLastCoord :: MVar (Float, Float)
  }

newtype WindowT m a = WindowT { runw :: ReaderT WindowState m a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadWriter w, MonadState s, MonadError e, MonadIO
		, MonadFix, MonadZip, MonadPlus
		)

instance MonadTrans WindowT where
  lift = WindowT . lift

instance MonadReader r m => MonadReader r (WindowT m) where
	ask = lift $ ask
	local f = withw $ mapReaderT (local f)
		where
		withw g = WindowT . g . runw

class MonadIO m => MonadWindow m where
	windowState :: m WindowState

-- | Access to underlying GLFW window.
glfwWindow :: MonadWindow m => m W.Window
glfwWindow = wsGlfwWindow <$> windowState

eventQueue :: MonadWindow m => m (MVar [Event])
eventQueue = wsEventQueue <$> windowState


windowSize :: (MonadIO m, MonadWindow m) => m (Int,Int)
windowSize = glfwWindow >>= liftIO . W.getWindowSize


instance MonadIO m => MonadWindow (WindowT m) where
	windowState = WindowT ask


instance MonadWindow m => MonadWindow (ReaderT r m) where
	windowState = lift windowState

instance MonadWindow m => MonadWindow (StateT s m) where
	windowState = lift windowState

instance (MonadWindow m, Monoid w) => MonadWindow (WriterT w m) where
	windowState = lift windowState

instance (MonadWindow m, Monoid w) => MonadWindow (RWST r w s m) where
	windowState = lift windowState

instance MonadWindow m => MonadWindow (Strict.StateT s m) where
	windowState = lift windowState

instance (MonadWindow m, Monoid w) => MonadWindow (Strict.WriterT w m) where
	windowState = lift windowState

instance (MonadWindow m, Monoid w) => MonadWindow (Strict.RWST r w s m) where
	windowState = lift windowState

instance MonadWindow m => MonadWindow (ExceptT e m) where
	windowState = lift windowState



-- ERRORS ----------------------------------------------------------------------

-- | The functions of this module throw exceptions.
data WindowErr
	= ErrNoMonitor | ErrNoVideoMode | ErrWindowCreationFailed
	| ErrNoContext | ErrGLFWInitFailed | ErrApiUnavailable | ErrVersionUnavailable
	| ErrPlatformError | ErrElse String deriving (Read,Show,Eq,Ord)
-- since all exceptions drop during window creation, catching it there and returning an Either Err might make more sense. TODO


instance Semigroup WindowErr where
	a <> b
		| a == mempty = b
		| otherwise = a

instance Monoid WindowErr where
	mempty = ErrElse "No error. "


instance Exception WindowErr where
	displayException ErrNoMonitor = "No monitors."
	displayException ErrNoVideoMode = "No video mode found."
	displayException ErrWindowCreationFailed = "Window creation failed."
	displayException ErrNoContext = "No GL context set."
	displayException ErrGLFWInitFailed = "GLFW failed to initialize."
	displayException ErrApiUnavailable = "Requested GL API unavailable."
	displayException ErrVersionUnavailable = "GL API Version unavailable."
	displayException ErrPlatformError = "GLFW Platform Error."
	displayException (ErrElse s) = s
-- perhaps reduce these back to their GLFW string. these errors are rare and the glfw helps to find the issue straight in its docs. TODO



-- OTHERS ----------------------------------------------------------------------


-- # Utils

-- | Hack for making sure an action is run at a later time.
--   Just so I don't need to get a lifted catch for this one time I need one.
--   This hack triggers, when the garbage collector is called.
toBeRun :: MonadIO m => IO () -> m (m ())
toBeRun a = liftIO $ do
	liftIO $ performGC -- clean up previous stuff
	s <- newEmptyMVar
	void $ forkIO $ catchJust isMVarBlock (takeMVar s) (\_ -> a)
	return $ liftIO $ do
		a
		putMVar s ()
	where
		isMVarBlock :: BlockedIndefinitelyOnMVar -> Maybe ()
		isMVarBlock _ = Just ()
-- This is only of concern when a program keeps running,
-- after exiting the Window monad with an exception that is handled outside.
-- I wonder if this comes back to bite me.

-- change this to weak finalizer with mkWeakIORef TODO



-- | Add to MVar list.
mlAdd :: MonadIO m => MVar [a] -> a -> m ()
mlAdd m a = liftIO $ modifyMVar_ m (return . (a:))

-- | Remove from MVar list.
mlRemove :: MonadIO m => MVar [a] -> (a -> Bool) -> m [a]
mlRemove m p = liftIO $ modifyMVar m (\xs -> return $ (filter (not . p) xs, filter p xs))

mlRemoveAll :: MonadIO m => MVar [a] -> m [a]
mlRemoveAll m = mlRemove m (const True)

-- ~ -- | Read MVar list.
-- ~ mlRead :: MonadIO m => MVar a -> m a
-- ~ mlRead m = liftIO $ readMVar m


-- ~ for = flip map



-- | Finish render and display it on screen.
swapBuffers :: MonadWindow m => m ()
swapBuffers = do
	w <- glfwWindow
	liftIO $ W.swapBuffers w


