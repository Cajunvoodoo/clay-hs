module Main (main) where

import Clay.Raw.Functions
import Clay.Raw.Types
import Clay.Raylib.Raw.Types
import Foreign.Marshal.Alloc (mallocBytes)
import Data.ByteString (ByteString)
import Data.Word
import Data.Bits
import Raylib.Core
import Raylib.Core.Textures
import Raylib.Core.Text
import Linear.V2
import Data.IORef
import Foreign.Ptr
import Control.Monad

raylibMeasureText :: MeasureTextFunction
raylibMeasureText = undefined

getScreenDims :: IO ClayDimensions
getScreenDims = undefined

-- TODO: foreign import for Clay__ErrorHandlerDefault
clayErrorHandlerDefault :: ClayErrorHandler
clayErrorHandlerDefault = undefined

getErrorHandler :: IO ClayErrorHandler
getErrorHandler = undefined

clayRaylibInitialize :: Int -> Int -> ByteString -> Word32 -> IO ()
clayRaylibInitialize = undefined

-- loadTextureFromImage = undefined
-- loadImage = undefined

main :: IO ()
main = do
  totalMemorySize <- clayMinMemorySize
  totalMemoryArena <- mallocBytes (fromIntegral totalMemorySize)
  clayMemory <- clayCreateArenaWithCapacityAndMemory (fromIntegral totalMemorySize) totalMemoryArena

  _measureTextFuncPtr <- claySetMeasureTextFunction raylibMeasureText

  screenDims <- getScreenDims
  errorHandler <- getErrorHandler
  clayInitialize clayMemory  screenDims errorHandler

  let flags = FLAG_VSYNC_HINT .|. FLAG_WINDOW_RESIZABLE -- .|. FLAG_WINDOW_HIGHDPI .|. FLAG_MSAA_4X_HINT
  clayRaylibInitialize 10224 768 "Clay - Raylib Renderer Example" flags

  profilePicture  <- loadTextureFromImage =<< loadImage "resources/profile-picture.png"

  reinitializeClayRef <- newIORef False

  -- TODO: font loading
  -- loadFont =<< getFileName
  mainLoop reinitializeClayRef errorHandler False

mainLoop :: IORef Bool -> ClayErrorHandler -> Bool -> IO ()
mainLoop _ _ True = pure ()
mainLoop reinitializeClayRef errorHandler False = do
  reinitializeClay <- readIORef reinitializeClayRef
  when reinitializeClay do
    claySetMaxElementCount 8192
    totalMemorySize <- fromIntegral <$> clayMinMemorySize
    totalMemoryArena <- mallocBytes (fromIntegral totalMemorySize)
    clayMemory <- clayCreateArenaWithCapacityAndMemory totalMemorySize totalMemoryArena
    screenDims <- getScreenDims
    clayInitialize clayMemory  screenDims errorHandler
    writeIORef reinitializeClayRef False

  updateDrawFrame
  closeWindow <- windowShouldClose
  mainLoop reinitializeClayRef errorHandler closeWindow

updateDrawFrame :: IO ()
updateDrawFrame = do
  V2 mouseWheelX mouseWheelY <- getMouseWheelMoveV
  undefined
