{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Name where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.GL
import Graphics.Farbe.Utils
import Graphics.Farbe.Utility
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Array
import Graphics.Farbe.Texture
import Graphics.Farbe.State
-- ~ import Graphics.Farbe.Window
-- ~ import Graphics.Farbe.Utility
-- ~ import Graphics.Farbe.Delay


import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import Data.Hashable
import System.Mem.StableName
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))



import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import GHC.TypeNats

import Debug.Trace

#define bottom undefined

name :: (Farbe m, Functor m, GLtype a) => String -> a -> m String
name s a = generateName $ s ++ glShortName a

nameAttrib :: (Farbe m, Functor m, GLtype a) => String -> a -> m String
nameAttrib s a = (++ glShortName a) <$> generateName s

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f


generateName :: (Farbe m, Functor m) => String -> m String
generateName s = (s++) . ("_"++) . show <$> count



