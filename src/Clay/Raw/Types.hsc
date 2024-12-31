{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoFieldSelectors #-}

module Clay.Raw.Types where

import Foreign
import Foreign.C.Types
import Foreign.Storable.Generic
import GHC.Generics

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

-- ENUMERATIONS ----------------------------------------------------------------

newtype ClayLayoutDirection = ClayLayoutDirection {unClayLayoutDirection :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayLayoutDirection, ClayLayoutDirection
  , leftToRight = CLAY_LEFT_TO_RIGHT
  , topToBottom = CLAY_TOP_TO_BOTTOM
}

newtype ClayLayoutAlignmentX = ClayLayoutAlignmentX {unClayLayoutAlignmentX :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayLayoutAlignmentX, ClayLayoutAlignmentX
  , alignXLeft   = CLAY_ALIGN_X_LEFT
  , alignXRight  = CLAY_ALIGN_X_RIGHT
  , alignXCenter = CLAY_ALIGN_X_CENTER
 }

newtype ClayLayoutAlignmentY = ClayLayoutAlignmentY {unClayLayoutAlignmentY :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayLayoutAlignmentY, ClayLayoutAlignmentY
  , alignYTop    = CLAY_ALIGN_Y_TOP
  , alignYBottom = CLAY_ALIGN_Y_BOTTOM
  , alignYCenter = CLAY_ALIGN_Y_CENTER
}

newtype ClaySizingType = ClaySizingType {unClaySizingType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClaySizingType, ClaySizingType
  , sizingFit     = CLAY__SIZING_TYPE_FIT
  , sizingGrow    = CLAY__SIZING_TYPE_GROW
  , sizingPercent = CLAY__SIZING_TYPE_PERCENT
  , sizingFixed   = CLAY__SIZING_TYPE_FIXED
}

newtype ClayTextElementConfigWrapMode = ClayTextElementConfigWrapMode {unClayTextElementConfigWrapMode :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayTextElementConfigWrapMode, ClayTextElementConfigWrapMode
  , textWrapWords    = CLAY_TEXT_WRAP_WORDS
  , textWrapNewlines = CLAY_TEXT_WRAP_NEWLINES
  , textWrapNone     = CLAY_TEXT_WRAP_NONE
}

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

newtype ClayPointerCaptureMode = ClayPointerCaptureMode {unClayPointerCaptureMode :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayPointerCaptureMode, ClayPointerCaptureMode
  , captureModeCapture     = CLAY_POINTER_CAPTURE_MODE_CAPTURE
  , captureModePassthrough = CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH
}

newtype ClayRenderCommandType = ClayRenderCommandType {unClayRenderCommandType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayRenderCommandType, ClayRenderCommandType
  , renderNone          = CLAY_RENDER_COMMAND_TYPE_NONE
  , renderRectangle     = CLAY_RENDER_COMMAND_TYPE_RECTANGLE
  , renderBorder        = CLAY_RENDER_COMMAND_TYPE_BORDER
  , renderText          = CLAY_RENDER_COMMAND_TYPE_TEXT
  , renderImage         = CLAY_RENDER_COMMAND_TYPE_IMAGE
  , renderScissor_Start = CLAY_RENDER_COMMAND_TYPE_SCISSOR_START
  , renderScissor_End   = CLAY_RENDER_COMMAND_TYPE_SCISSOR_END
  , renderCustom        = CLAY_RENDER_COMMAND_TYPE_CUSTOM
}

newtype ClayPointerDataInteractionState = ClayPointerDataInteractionState {unClayPointerDataInteractionState :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum ClayPointerDataInteractionState, ClayPointerDataInteractionState
  , ptrDataPressedThisFrame  = CLAY_POINTER_DATA_PRESSED_THIS_FRAME
  , ptrDataPressed           = CLAY_POINTER_DATA_PRESSED
  , ptrDataReleasedThisFrame = CLAY_POINTER_DATA_RELEASED_THIS_FRAME
  , ptrDataReleased          = CLAY_POINTER_DATA_RELEASED
}

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

newtype Clay__SizeDistributionType = Clay__SizeDistributionType {unClay__SizeDistributionType :: CInt}
  deriving stock (Show)
  deriving newtype (Storable)
#{enum Clay__SizeDistributionType, Clay__SizeDistributionType
  , sizeDistScrollContainer     = CLAY__SIZE_DISTRIBUTION_TYPE_SCROLL_CONTAINER
  , sizeDistResizeableContainer = CLAY__SIZE_DISTRIBUTION_TYPE_RESIZEABLE_CONTAINER
  , sizeDistGrowContainer       = CLAY__SIZE_DISTRIBUTION_TYPE_GROW_CONTAINER
}
