{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoFieldSelectors #-}

module Clay.Raw.Types where

import Foreign
import Foreign.C.Types
import Foreign.Storable.Generic
import GHC.Generics
import Data.Coerce (coerce)

#define CLAY_IMPLEMENTATION
#include "clay.h"

{-
#poke      ⟨struct_type⟩, ⟨field⟩ :: Storable b => Ptr a -> b -> IO ()
#ptr       ⟨struct_type⟩, ⟨field⟩ :: Ptr a -> Ptr b
#offset    ⟨struct_type⟩, ⟨field⟩ :: Int
#size      ⟨struct_type⟩          :: Int
#alignment ⟨struct_type⟩          :: Int
-}

data ClayVector2 = ClayVector2
  { x :: Float
  , y :: Float
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayDimensions = ClayDimensions
  { width :: Float
  , height :: Float
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayArena = ClayArena
  { nextAllocation :: WordPtr
  , capacity :: CSize
  , memory :: Ptr Word8
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayColor = ClayColor
  { r :: Float
  , g :: Float
  , b :: Float
  , a :: Float
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayBoundingBox = ClayBoundingBox
  { x :: Float
  , y :: Float
  , width :: Float
  , height :: Float
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayString = ClayString
  { length :: Int
  , chars :: Ptr Word8
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayElementId = ClayElementId
  { id :: Word32
  , offset :: Word32
  , baseId :: Word32
  , stringId :: ClayString
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayCornerRadius = ClayCornerRadius
  { topLeft :: Float
  , topRight :: Float
  , bottomLeft :: Float
  , bottomRight :: Float
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayErrorData = ClayErrorData
  { errorType :: ClayErrorType
  , errorText :: ClayString
  , userData :: WordPtr
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayErrorHandler = ClayErrorHandler
  { errorHandlerFunction :: FunPtr (ClayErrorData -> IO ()) -- requires manual memory management!
  , userData :: WordPtr
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

-- | This  is an untyped union, where all the elements are pointers to various
-- 'Clay*Config' types.
type ClayElementConfigUnion = Ptr ()

data ClayRenderCommand = ClayRenderCommand
  { boundingBox :: ClayBoundingBox
  , config :: ClayElementConfigUnion
  , text :: ClayString
  , id :: Word32
  , commandType :: ClayRenderCommandType
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayRenderCommandArray = ClayRenderCommandArray
  { capacity :: Word32
  , length :: Word32
  , internalArray :: Ptr ClayRenderCommand
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayScrollElementConfig = ClayScrollElementConfig
  { horizontal :: Bool
  , vertical :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayScrollContainerData = ClayScrollContainerData
  { scrollPosition :: Ptr ClayVector2
  , scrollContainerDimensions :: ClayDimensions
  , contentDimensions :: ClayDimensions
  , config :: ClayScrollElementConfig
  , found :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayPointerData = ClayPointerData
  { position :: ClayVector2
  , state :: ClayPointerDataInteractionState
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

data ClayTextElementConfig = ClayTextElementConfig
  { textColor :: ClayColor
  , fontId :: Word16
  , fontSize :: Word16
  , letterSpacing :: Word16
  , lineHeight :: Word16
  , wrapMode :: ClayTextElementConfigWrapMode
  #ifdef CLAY_EXTEND_CONFIG_TEXT
  , CLAY_EXTEND_CONFIG_TEXT_HASKELL
  #endif
  }
  deriving stock (Show, Generic)
  deriving anyclass (GStorable)

-- ENUMERATIONS ----------------------------------------------------------------

newtype ClayLayoutDirection = ClayLayoutDirection {unClayLayoutDirection :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayLayoutDirection, ClayLayoutDirection
  , leftToRight = CLAY_LEFT_TO_RIGHT
  , topToBottom = CLAY_TOP_TO_BOTTOM
}

pattern CLAY_TOP_TO_BOTTOM <- CInt leftToRight
 where CLAY_TOP_TO_BOTTOM = coerce leftToRight
pattern CLAY_LEFT_TO_RIGHT <- CInt topToBottom
 where CLAY_LEFT_TO_RIGHT = coerce topToBottom

newtype ClayLayoutAlignmentX = ClayLayoutAlignmentX {unClayLayoutAlignmentX :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayLayoutAlignmentX, ClayLayoutAlignmentX
  , alignXLeft   = CLAY_ALIGN_X_LEFT
  , alignXRight  = CLAY_ALIGN_X_RIGHT
  , alignXCenter = CLAY_ALIGN_X_CENTER
 }
pattern CLAY_ALIGN_X_CENTER <- CInt alignXLeft
 where CLAY_ALIGN_X_CENTER = coerce alignXLeft
pattern CLAY_ALIGN_X_RIGHT  <- CInt alignXRight
 where CLAY_ALIGN_X_RIGHT  = coerce alignXRight
pattern CLAY_ALIGN_X_LEFT   <- CInt alignXCenter
 where CLAY_ALIGN_X_LEFT   = coerce alignXCenter

newtype ClayLayoutAlignmentY = ClayLayoutAlignmentY {unClayLayoutAlignmentY :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayLayoutAlignmentY, ClayLayoutAlignmentY
  , alignYTop    = CLAY_ALIGN_Y_TOP
  , alignYBottom = CLAY_ALIGN_Y_BOTTOM
  , alignYCenter = CLAY_ALIGN_Y_CENTER
}

pattern CLAY_ALIGN_Y_TOP    <- CInt alignYTop
 where CLAY_ALIGN_Y_TOP    = coerce alignYTop
pattern CLAY_ALIGN_Y_BOTTOM <- CInt alignYBottom
 where CLAY_ALIGN_Y_BOTTOM = coerce alignYBottom
pattern CLAY_ALIGN_Y_CENTER <- CInt alignYCenter
 where CLAY_ALIGN_Y_CENTER = coerce alignYCenter

newtype ClaySizingType = ClaySizingType {unClaySizingType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClaySizingType, ClaySizingType
  , sizingFit     = CLAY__SIZING_TYPE_FIT
  , sizingGrow    = CLAY__SIZING_TYPE_GROW
  , sizingPercent = CLAY__SIZING_TYPE_PERCENT
  , sizingFixed   = CLAY__SIZING_TYPE_FIXED
}

pattern CLAY__SIZING_TYPE_FIT     <- CInt sizingFit
 where CLAY__SIZING_TYPE_FIT     = coerce sizingFit
pattern CLAY__SIZING_TYPE_GROW    <- CInt sizingPercent
 where CLAY__SIZING_TYPE_GROW    = coerce sizingPercent
pattern CLAY__SIZING_TYPE_PERCENT <- CInt sizingGrow
 where CLAY__SIZING_TYPE_PERCENT = coerce sizingGrow
pattern CLAY__SIZING_TYPE_FIXED   <- CInt sizingFixed
 where CLAY__SIZING_TYPE_FIXED   = coerce sizingFixed

newtype ClayTextElementConfigWrapMode = ClayTextElementConfigWrapMode {unClayTextElementConfigWrapMode :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayTextElementConfigWrapMode, ClayTextElementConfigWrapMode
  , textWrapWords    = CLAY_TEXT_WRAP_WORDS
  , textWrapNewlines = CLAY_TEXT_WRAP_NEWLINES
  , textWrapNone     = CLAY_TEXT_WRAP_NONE
}

pattern CLAY_TEXT_WRAP_WORDS    <- CInt textWrapWords
 where CLAY_TEXT_WRAP_WORDS    = coerce textWrapWords
pattern CLAY_TEXT_WRAP_NEWLINES <- CInt textWrapNewlines
 where CLAY_TEXT_WRAP_NEWLINES = coerce textWrapNewlines
pattern CLAY_TEXT_WRAP_NONE     <- CInt textWrapNone
 where CLAY_TEXT_WRAP_NONE     = coerce textWrapNone


newtype ClayFloatingAttachPointType = ClayFloatingAttachPointType {unClayFloatingAttachPointType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayFloatingAttachPointType, ClayFloatingAttachPointType
  , pointLeftTop      = CLAY_ATTACH_POINT_LEFT_TOP
  , pointLeftCenter   = CLAY_ATTACH_POINT_LEFT_CENTER
  , pointLeftBottom   = CLAY_ATTACH_POINT_LEFT_BOTTOM
  , pointCenterTop    = CLAY_ATTACH_POINT_CENTER_TOP
  , pointCenterCenter = CLAY_ATTACH_POINT_CENTER_CENTER
  , pointCenterBottom = CLAY_ATTACH_POINT_CENTER_BOTTOM
  , pointRightTop     = CLAY_ATTACH_POINT_RIGHT_TOP
  , pointRightCenter  = CLAY_ATTACH_POINT_RIGHT_CENTER
  , pointRightBottom  = CLAY_ATTACH_POINT_RIGHT_BOTTOM
}

pattern CLAY_ATTACH_POINT_LEFT_TOP      <- CInt pointLeftTop
 where CLAY_ATTACH_POINT_LEFT_TOP      = coerce pointLeftTop
pattern CLAY_ATTACH_POINT_LEFT_CENTER   <- CInt pointLeftCenter
 where CLAY_ATTACH_POINT_LEFT_CENTER   = coerce pointLeftCenter
pattern CLAY_ATTACH_POINT_LEFT_BOTTOM   <- CInt pointLeftBottom
 where CLAY_ATTACH_POINT_LEFT_BOTTOM   = coerce pointLeftBottom
pattern CLAY_ATTACH_POINT_CENTER_TOP    <- CInt pointCenterTop
 where CLAY_ATTACH_POINT_CENTER_TOP    = coerce pointCenterTop
pattern CLAY_ATTACH_POINT_CENTER_CENTER <- CInt pointCenterCenter
 where CLAY_ATTACH_POINT_CENTER_CENTER = coerce pointCenterCenter
pattern CLAY_ATTACH_POINT_CENTER_BOTTOM <- CInt pointCenterBottom
 where CLAY_ATTACH_POINT_CENTER_BOTTOM = coerce pointCenterBottom
pattern CLAY_ATTACH_POINT_RIGHT_TOP     <- CInt pointRightTop
 where CLAY_ATTACH_POINT_RIGHT_TOP     = coerce pointRightTop
pattern CLAY_ATTACH_POINT_RIGHT_CENTER  <- CInt pointRightCenter
 where CLAY_ATTACH_POINT_RIGHT_CENTER  = coerce pointRightCenter
pattern CLAY_ATTACH_POINT_RIGHT_BOTTOM  <- CInt pointRightBottom
 where CLAY_ATTACH_POINT_RIGHT_BOTTOM  = coerce pointRightBottom

newtype ClayPointerCaptureMode = ClayPointerCaptureMode {unClayPointerCaptureMode :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayPointerCaptureMode, ClayPointerCaptureMode
  , captureModeCapture     = CLAY_POINTER_CAPTURE_MODE_CAPTURE
  , captureModePassthrough = CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH
}

pattern CLAY_POINTER_CAPTURE_MODE_CAPTURE     <- CInt captureModeCapture
 where CLAY_POINTER_CAPTURE_MODE_CAPTURE     = coerce captureModeCapture
pattern CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH <- CInt captureModePassthrough
 where CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH = coerce captureModePassthrough

newtype ClayRenderCommandType = ClayRenderCommandType {unClayRenderCommandType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayRenderCommandType, ClayRenderCommandType
  , renderNone          = CLAY_RENDER_COMMAND_TYPE_NONE
  , renderRectangle     = CLAY_RENDER_COMMAND_TYPE_RECTANGLE
  , renderBorder        = CLAY_RENDER_COMMAND_TYPE_BORDER
  , renderText          = CLAY_RENDER_COMMAND_TYPE_TEXT
  , renderImage         = CLAY_RENDER_COMMAND_TYPE_IMAGE
  , renderScissorStart = CLAY_RENDER_COMMAND_TYPE_SCISSOR_START
  , renderScissorEnd   = CLAY_RENDER_COMMAND_TYPE_SCISSOR_END
  , renderCustom        = CLAY_RENDER_COMMAND_TYPE_CUSTOM
}

pattern CLAY_RENDER_COMMAND_TYPE_NONE          <- CInt renderNone
 where CLAY_RENDER_COMMAND_TYPE_NONE          = coerce renderNone
pattern CLAY_RENDER_COMMAND_TYPE_RECTANGLE     <- CInt renderRectangle
 where CLAY_RENDER_COMMAND_TYPE_RECTANGLE     = coerce renderRectangle
pattern CLAY_RENDER_COMMAND_TYPE_BORDER        <- CInt renderBorder
 where CLAY_RENDER_COMMAND_TYPE_BORDER        = coerce renderBorder
pattern CLAY_RENDER_COMMAND_TYPE_TEXT          <- CInt renderText
 where CLAY_RENDER_COMMAND_TYPE_TEXT          = coerce renderText
pattern CLAY_RENDER_COMMAND_TYPE_IMAGE         <- CInt renderImage
 where CLAY_RENDER_COMMAND_TYPE_IMAGE         = coerce renderImage
pattern CLAY_RENDER_COMMAND_TYPE_SCISSOR_START <- CInt renderScissorStart
 where CLAY_RENDER_COMMAND_TYPE_SCISSOR_START = coerce renderScissorStart
pattern CLAY_RENDER_COMMAND_TYPE_SCISSOR_END   <- CInt renderScissorEnd
 where CLAY_RENDER_COMMAND_TYPE_SCISSOR_END   = coerce renderScissorEnd
pattern CLAY_RENDER_COMMAND_TYPE_CUSTOM        <- CInt renderCustom
 where CLAY_RENDER_COMMAND_TYPE_CUSTOM        = coerce renderCustom

newtype ClayPointerDataInteractionState = ClayPointerDataInteractionState {unClayPointerDataInteractionState :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayPointerDataInteractionState, ClayPointerDataInteractionState
  , ptrDataPressedThisFrame  = CLAY_POINTER_DATA_PRESSED_THIS_FRAME
  , ptrDataPressed           = CLAY_POINTER_DATA_PRESSED
  , ptrDataReleasedThisFrame = CLAY_POINTER_DATA_RELEASED_THIS_FRAME
  , ptrDataReleased          = CLAY_POINTER_DATA_RELEASED
}

pattern CLAY_POINTER_DATA_PRESSED_THIS_FRAME  <- CInt ptrDataPressedThisFrame
 where CLAY_POINTER_DATA_PRESSED_THIS_FRAME  = coerce ptrDataPressedThisFrame
pattern CLAY_POINTER_DATA_PRESSED             <- CInt ptrDataPressed
 where CLAY_POINTER_DATA_PRESSED             = coerce ptrDataPressed
pattern CLAY_POINTER_DATA_RELEASED_THIS_FRAME <- CInt ptrDataReleasedThisFrame
 where CLAY_POINTER_DATA_RELEASED_THIS_FRAME = coerce ptrDataReleasedThisFrame
pattern CLAY_POINTER_DATA_RELEASED            <- CInt ptrDataReleased
 where CLAY_POINTER_DATA_RELEASED            = coerce ptrDataReleased

newtype ClayErrorType = ClayErrorType {unClayErrorType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayErrorType, ClayErrorType
  , errorTextMeasurementFunctionNotProvided = CLAY_ERROR_TYPE_TEXT_MEASUREMENT_FUNCTION_NOT_PROVIDED
  , errorArenaCapacityExceeded              = CLAY_ERROR_TYPE_ARENA_CAPACITY_EXCEEDED
  , errorElementsCapacityExceeded           = CLAY_ERROR_TYPE_ELEMENTS_CAPACITY_EXCEEDED
  , errorTextMeasurementCapacityExceeded    = CLAY_ERROR_TYPE_TEXT_MEASUREMENT_CAPACITY_EXCEEDED
  , errorDuplicateId                        = CLAY_ERROR_TYPE_DUPLICATE_ID
  , errorFloatingContainerParentNotFound    = CLAY_ERROR_TYPE_FLOATING_CONTAINER_PARENT_NOT_FOUND
  , errorInternalError                      = CLAY_ERROR_TYPE_INTERNAL_ERROR
}
pattern CLAY_ERROR_TYPE_TEXT_MEASUREMENT_FUNCTION_NOT_PROVIDED <- CInt errorTextMeasurementFunctionNotProvided
 where CLAY_ERROR_TYPE_TEXT_MEASUREMENT_FUNCTION_NOT_PROVIDED = coerce errorTextMeasurementFunctionNotProvided
pattern CLAY_ERROR_TYPE_ARENA_CAPACITY_EXCEEDED                <- CInt errorArenaCapacityExceeded
 where CLAY_ERROR_TYPE_ARENA_CAPACITY_EXCEEDED                = coerce errorArenaCapacityExceeded
pattern CLAY_ERROR_TYPE_ELEMENTS_CAPACITY_EXCEEDED             <- CInt errorElementsCapacityExceeded
 where CLAY_ERROR_TYPE_ELEMENTS_CAPACITY_EXCEEDED             = coerce errorElementsCapacityExceeded
pattern CLAY_ERROR_TYPE_TEXT_MEASUREMENT_CAPACITY_EXCEEDED     <- CInt errorTextMeasurementCapacityExceeded
 where CLAY_ERROR_TYPE_TEXT_MEASUREMENT_CAPACITY_EXCEEDED     = coerce errorTextMeasurementCapacityExceeded
pattern CLAY_ERROR_TYPE_DUPLICATE_ID                           <- CInt errorDuplicateId
 where CLAY_ERROR_TYPE_DUPLICATE_ID                           = coerce errorDuplicateId
pattern CLAY_ERROR_TYPE_FLOATING_CONTAINER_PARENT_NOT_FOUND    <- CInt errorFloatingContainerParentNotFound
 where CLAY_ERROR_TYPE_FLOATING_CONTAINER_PARENT_NOT_FOUND    = coerce errorFloatingContainerParentNotFound
pattern CLAY_ERROR_TYPE_INTERNAL_ERROR                         <- CInt errorInternalError
 where CLAY_ERROR_TYPE_INTERNAL_ERROR                         = coerce errorInternalError

newtype Clay__SizeDistributionType = Clay__SizeDistributionType {unClay__SizeDistributionType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum Clay__SizeDistributionType, Clay__SizeDistributionType
  , sizeDistScrollContainer     = CLAY__SIZE_DISTRIBUTION_TYPE_SCROLL_CONTAINER
  , sizeDistResizeableContainer = CLAY__SIZE_DISTRIBUTION_TYPE_RESIZEABLE_CONTAINER
  , sizeDistGrowContainer       = CLAY__SIZE_DISTRIBUTION_TYPE_GROW_CONTAINER
}
foo (Clay__SizeDistributionType i) = i

pattern CLAY__SIZE_DISTRIBUTION_TYPE_SCROLL_CONTAINER <- CInt sizeDistScrollContainer
 where CLAY__SIZE_DISTRIBUTION_TYPE_SCROLL_CONTAINER  = coerce sizeDistScrollContainer
pattern CLAY__SIZE_DISTRIBUTION_TYPE_RESIZEABLE_CONTAINER <- CInt sizeDistResizeableContainer
 where CLAY__SIZE_DISTRIBUTION_TYPE_RESIZEABLE_CONTAINER  = coerce sizeDistResizeableContainer
pattern CLAY__SIZE_DISTRIBUTION_TYPE_GROW_CONTAINER <- CInt sizeDistGrowContainer
 where CLAY__SIZE_DISTRIBUTION_TYPE_GROW_CONTAINER = coerce sizeDistGrowContainer
