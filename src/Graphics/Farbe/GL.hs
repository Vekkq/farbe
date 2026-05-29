{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}


module Graphics.Farbe.GL where




import Graphics.Farbe.Vec

import GHC.Generics (Generic)
import Data.Hashable

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Foreign hiding (void)

import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject as GLEXT
import Graphics.GL.Ext.OES.Mapbuffer as GLEXT
import Graphics.GL.Types



-- GL type information -------------------------------------------------------------------

data TypeS = TBool | TInt | TFloat | TVec2 TypeS | TVec3 TypeS | TVec4 TypeS | TTex
	deriving (Eq, Ord, Read, Show, Generic)

instance Hashable TypeS


class (Eq a) => GLtype a where
	slName :: a -> String
	toTypeS :: a -> TypeS
	glType :: a -> GLenum
	glComponents :: a -> GLint
	glComponents _ = 1
	glNormalized :: a -> GLboolean
	glNormalized _ = GL_FALSE
	glShortName :: a -> String
	glShortName a = take 1 $ slName a
	glPrecision :: a -> String
	glPrecision _ = "highp"
	slNameWithPrec :: a -> String
	slNameWithPrec a = glPrecision a ++ " " ++ slName a



instance GLtype Bool where
	slName _ = "bool"
	toTypeS _ = TBool
	glType _ = GL_BOOL

instance GLtype Int32 where
	slName _ = "int"
	toTypeS _ = TInt
	glType _ = GL_INT

instance GLtype Float where
	slName _ = "float"
	toTypeS _ = TFloat
	glType _ = GL_FLOAT

instance GLtype (V2 Float) where
	slName _ = "vec2"
	toTypeS _ = TVec2 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 2
	glShortName _ = "v2"

instance GLtype (V3 Float) where
	slName _ = "vec3"
	toTypeS _ = TVec3 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 3
	glShortName _ = "v3"

instance GLtype (V4 Float) where
	slName _ = "vec4"
	toTypeS _ = TVec4 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 4
	glShortName _ = "v4"


instance GLtype (V2 Int32) where
	slName _ = "ivec2"
	toTypeS _ = TVec2 TInt
	glType _ = GL_INT
	glComponents _ = 2
	glShortName _ = "v2i"

instance GLtype (V3 Int32) where
	slName _ = "ivec3"
	toTypeS _ = TVec3 TInt
	glType _ = GL_INT
	glComponents _ = 3
	glShortName _ = "v3i"

instance GLtype (V4 Int32) where
	slName _ = "ivec4"
	toTypeS _ = TVec4 TInt
	glType _ = GL_INT
	glComponents _ = 4
	glShortName _ = "v4i"

instance GLtype (V2 Bool) where
	slName _ = "bvec2"
	toTypeS _ = TVec2 TBool
	glType _ = GL_BOOL
	glComponents _ = 2
	glShortName _ = "v2b"

instance GLtype (V3 Bool) where
	slName _ = "bvec3"
	toTypeS _ = TVec3 TBool
	glType _ = GL_BOOL
	glComponents _ = 3
	glShortName _ = "v3b"

instance GLtype (V4 Bool) where
	slName _ = "bvec4"
	toTypeS _ = TVec4 TBool
	glType _ = GL_BOOL
	glComponents _ = 4
	glShortName _ = "v4b"

boolToInt :: Bool -> Int32
boolToInt True = 1
boolToInt _ = 0


instance GLtype (Mat V2 V2 Float) where
	slName _ = "mat2"
	toTypeS _ = TVec2 $ TVec2 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 4
	glShortName _ = "m2"

instance GLtype (Mat V3 V3 Float) where
	slName _ = "mat3"
	toTypeS _ = TVec3 $ TVec3 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 9
	glShortName _ = "m3"

instance GLtype (Mat V4 V4 Float) where
	slName _ = "mat4"
	toTypeS _ = TVec4 $ TVec4 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 16
	glShortName _ = "m4"



data Normalized a = Normalized { unNormalized :: a } deriving (Eq)

instance Functor Normalized where
	fmap f (Normalized a) = Normalized $ f a

#define bottom undefined

instance Storable a => Storable (Normalized a) where
	sizeOf _ = sizeOf (bottom :: a)
	alignment _ = alignment (bottom :: a)
	peek p = fmap Normalized $ peek $ castPtr p
	poke p (Normalized a) = poke (castPtr p) a

instance GLtype a => GLtype (Normalized a) where
	glNormalized _ = GL_TRUE
	slName _ = slName (bottom :: a)
	toTypeS _ = toTypeS (bottom :: a)
	glType _ = glType (bottom :: a)
	glComponents _ = glComponents (bottom :: a)
	glShortName _ = "n" ++ glShortName (bottom :: a)










