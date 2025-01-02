module Clay.Raylib.Raw.Types where

import Clay.Raw.Functions
import Clay.Raw.Types
import Data.Bits
import Data.ByteString (ByteString)
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable.Generic
import GHC.Generics
import Raylib.Core
import Raylib.Core.Textures

newtype CustomLayoutElementType = CustomLayoutElementType

data RaylibFont = RaylibFont
  { fontId :: Word32
  , font :: Font
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)
