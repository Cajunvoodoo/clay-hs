{-# LANGUAGE CPP #-}
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
import Raylib.Types.Core.Text

-- newtype CustomLayoutElementType = CustomLayoutElementType

data RaylibFont = RaylibFont
  { fontId :: Word32
  , font :: Font
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

pattern FLAG_VSYNC_HINT               = 0x00000040
pattern FLAG_FULLSCREEN_MODE          = 0x00000002
pattern FLAG_WINDOW_RESIZABLE         = 0x00000004
pattern FLAG_WINDOW_UNDECORATED       = 0x00000008
pattern FLAG_WINDOW_HIDDEN            = 0x00000080
pattern FLAG_WINDOW_MINIMIZED         = 0x00000200
pattern FLAG_WINDOW_MAXIMIZED         = 0x00000400
pattern FLAG_WINDOW_UNFOCUSED         = 0x00000800
pattern FLAG_WINDOW_TOPMOST           = 0x00001000
pattern FLAG_WINDOW_ALWAYS_RUN        = 0x00000100
pattern FLAG_WINDOW_TRANSPARENT       = 0x00000010
pattern FLAG_WINDOW_HIGHDPI           = 0x00002000
pattern FLAG_WINDOW_MOUSE_PASSTHROUGH = 0x00004000
pattern FLAG_BORDERLESS_WINDOWED_MODE = 0x00008000
pattern FLAG_MSAA_4X_HINT             = 0x00000020
pattern FLAG_INTERLACED_HINT          = 0x00010000
