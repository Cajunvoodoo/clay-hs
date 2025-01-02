module Main (main) where

import Clay.Raw.Functions
import Clay.Raw.Types
import Foreign.Marshal.Alloc (mallocBytes)
import Data.ByteString (ByteString)
import Data.Word
import Data.Bits
import Raylib.Core
import Raylib.Core.Textures

raylibMeasureText :: MeasureTextFunction
raylibMeasureText = undefined

getScreenDims :: IO ClayDimensions
getScreenDims = undefined

-- TODO: foreign import for Clay__ErrorHandlerDefault
clayErrorHandlerDefault :: ClayErrorHandler
clayErrorHandlerDefault = undefined

clayRaylibInitialize :: Int -> Int -> ByteString -> Word32 -> IO ()
clayRaylibInitialize = undefined

-- temporary
pattern FLAG_VSYNC_HINT = 0x4
pattern FLAG_WINDOW_RESIZABLE = 0x3
pattern FLAG_WINDOW_HIGHDPI = 0x2
pattern FLAG_MSAA_4X_HINT = 0x1

loadTextureFromImage = undefined
loadImage = undefined

main :: IO ()
main = do
  totalMemorySize <- fromIntegral <$> clayMinMemorySize
  totalMemoryArena <- mallocBytes (fromIntegral totalMemorySize)
  clayMemory <- clayCreateArenaWithCapacityAndMemory totalMemorySize totalMemoryArena

  _measureTextFuncPtr <- claySetMeasureTextFunction raylibMeasureText

  screenDims <- getScreenDims
  clayInitialize clayMemory  screenDims clayErrorHandlerDefault

  let flags = FLAG_VSYNC_HINT .|. FLAG_WINDOW_RESIZABLE .|. FLAG_WINDOW_HIGHDPI .|. FLAG_MSAA_4X_HINT
  clayRaylibInitialize 10224 768 "Clay - Raylib Renderer Example" flags

  profilePicture <- loadTextureFromImage =<< loadImage "resources/profile-picture.png"

  putStrLn "Test suite not yet implemented."
