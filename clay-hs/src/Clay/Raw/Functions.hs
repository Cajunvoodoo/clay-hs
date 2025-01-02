{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Clay.Raw.Functions where

import Clay.Raw.Context (clayContext)
import Clay.Raw.Types
import Foreign
import Foreign.C.Types
import Language.C.Inline qualified as C

#define CLAY_IMPLEMENTATION
C.context clayContext
C.include "clay.h"

foreign import capi "clay.h Clay_MinMemorySize"
  clayMinMemorySize :: IO CUInt

-- | @ Clay_Arena Clay_CreateArenaWithCapacityAndMemory(uint32_t capacity, void *offset); @
clayCreateArenaWithCapacityAndMemory :: Word32 -> Ptr () -> IO ClayArena
clayCreateArenaWithCapacityAndMemory capacity offset =
  C.withPtr_ \arenaPtr ->
    [C.block| void {
       *$(Clay_Arena* arenaPtr) = Clay_CreateArenaWithCapacityAndMemory($(uint32_t capacity), $(void* offset));
       return;
    }|]

-- | @ void Clay_SetPointerState(Clay_Vector2 position, bool pointerDown); @
claySetPointerState :: ClayVector2 -> Bool -> IO ()
claySetPointerState position (fromBool -> pointerDown) =
  withValPtr position \ptrPos ->
    [C.exp| void {Clay_SetPointerState(*$(Clay_Vector2* ptrPos), $(bool pointerDown))} |]

-- | @ void Clay_Initialize(Clay_Arena arena, Clay_Dimensions layoutDimensions, Clay_ErrorHandler errorHandler); @
clayInitialize :: ClayArena -> ClayDimensions -> ClayErrorHandler -> IO ()
clayInitialize arena layoutDimensions errorHandler =
  withValPtr3 arena layoutDimensions errorHandler \arenaPtr ldPtr ehPtr ->
    [C.exp| void {Clay_Initialize(*$(Clay_Arena* arenaPtr), *$(Clay_Dimensions* ldPtr), *$(Clay_ErrorHandler* ehPtr))} |]

-- | @ void Clay_UpdateScrollContainers(bool enableDragScrolling, Clay_Vector2 scrollDelta, float deltaTime); @
-- foreign import capi "clay.h Clay_UpdateScrollContainers"
clayUpdateScrollContainers :: Bool -> ClayVector2 -> Float -> IO ()
clayUpdateScrollContainers (fromBool -> enableDragScrolling) scrollDelta (CFloat -> deltaTime) =
  withValPtr scrollDelta \sdPtr ->
    [C.exp| void {Clay_UpdateScrollContainers($(bool enableDragScrolling), *$(Clay_Vector2* sdPtr), $(float deltaTime))} |]

-- | @ void Clay_SetLayoutDimensions(Clay_Dimensions dimensions); @
-- foreign import capi "clay.h Clay_SetLayoutDimensions"
claySetLayoutDimensions :: ClayDimensions -> IO ()
claySetLayoutDimensions dimensions =
  withValPtr dimensions \dimsPtr ->
    [C.exp| void {Clay_SetLayoutDimensions(*$(Clay_Dimensions* dimsPtr))} |]

-- | @ void Clay_BeginLayout(void); @
foreign import capi "clay.h Clay_BeginLayout" clayBeginLayout :: IO ()

-- | @ Clay_RenderCommandArray Clay_EndLayout(void); @
clayEndLayout :: IO ClayRenderCommandArray
clayEndLayout =
  C.withPtr_ \cmdPtr ->
    [C.block| void {
       *$(Clay_RenderCommandArray* cmdPtr) = Clay_EndLayout();
       return;
    }|]

-- | @ Clay_ElementId Clay_GetElementId(Clay_String idString); @
clayGetElementId :: ClayString -> IO ClayElementId
clayGetElementId idString =
  C.withPtr_ \elemIdPtr ->
    withValPtr idString \idStrPtr ->
      [C.block| void {
         *$(Clay_ElementId* elemIdPtr) = Clay_GetElementId(*$(Clay_String* idStrPtr));
         return;
      }|]

-- | @ Clay_ElementId Clay_GetElementIdWithIndex(Clay_String idString, uint32_t index); @
clayGetElementIdWithIndex :: ClayString -> Word32 -> IO ClayElementId
clayGetElementIdWithIndex idString index =
  C.withPtr_ \elemIdPtr ->
    withValPtr idString \idStrPtr ->
      [C.block| void {
         *$(Clay_ElementId* elemIdPtr) = Clay_GetElementIdWithIndex(*$(Clay_String* idStrPtr), $(uint32_t index));
         return;
      }|]

-- | @ bool Clay_Hovered(void); @
foreign import capi "clay.h Clay_Hovered"
  clayHovered :: IO Bool

-- | A function to call on hover, provided to 'clayOnHover'. The type is wrapped;
-- the original C passes structs by-value, but structs cannot be marshalled.
-- Therefore, we wrap the values with pointers and pass a function ad-hoc to
-- @Clay_OnHover@.
type OnHoverFunction = Ptr ClayElementId -> Ptr ClayPointerData -> CIntPtr -> IO ()

-- | @ void Clay_OnHover(void (*onHoverFunction)(Clay_ElementId elementId, Clay_PointerData pointerData, intptr_t userData), intptr_t userData); @
--
-- The returned 'FunPtr' must be manually freed using 'freeHaskellFunPtr' when
-- it is no longer needed.
clayOnHover :: OnHoverFunction -> CIntPtr -> IO (FunPtr OnHoverFunction)
clayOnHover onHoverFunctionWrapped userData = do
  onHoverFunctionWrappedPtr <-
    $(C.mkFunPtr [t| OnHoverFunction |]) onHoverFunctionWrapped
  [C.block| void {
     void (*onHoverFunction)(Clay_ElementId, Clay_PointerData, intptr_t);
     onHoverFunction = (void (*)(Clay_ElementId, Clay_PointerData, intptr_t))({
       void innerWrapper(Clay_ElementId elementId, Clay_PointerData pointerData, intptr_t userData) {
         $(void (*onHoverFunctionWrappedPtr)(Clay_ElementId*, Clay_PointerData*, intptr_t))(&elementId, &pointerData, userData);
       }
       innerWrapper;
     });
     Clay_OnHover(onHoverFunction, $(intptr_t userData));
  }|]
  pure onHoverFunctionWrappedPtr

-- | @ bool Clay_PointerOver(Clay_ElementId elementId); @
clayPointerOver :: ClayElementId -> IO Bool
clayPointerOver elementId =
  withValPtr elementId \elemIdPtr ->
    toBool <$> [C.exp| bool {Clay_PointerOver(*$(Clay_ElementId* elemIdPtr))} |]

-- | @ Clay_ScrollContainerData Clay_GetScrollContainerData(Clay_ElementId id); @
clayGetScrollContainerData :: ClayElementId -> IO ClayScrollContainerData
clayGetScrollContainerData id =
  C.withPtr_ \containerPtr ->
    withValPtr id \idPtr ->
      [C.block| void {
         *$(Clay_ScrollContainerData* containerPtr) = Clay_GetScrollContainerData(*$(Clay_ElementId* idPtr));
         return;
      }|]

-- | Wrapped function pointer for 'claySetMeasureTextFunction', which wraps
-- @Clay_SetMeasureTextFunction@. Rather than accept the return value as a pointer,
-- it ought to be placed into the memory pointed to by the third argument.
type MeasureTextFunction
   = Ptr ClayString -- ^ text,[in]
  -> Ptr ClayTextElementConfig -- ^ config, [in]
  -> Ptr ClayDimensions -- ^ [out]
  -> IO ()

-- | @ void Clay_SetMeasureTextFunction(Clay_Dimensions (*measureTextFunction)(Clay_String *text, Clay_TextElementConfig *config)); @
--
-- The returned 'FunPtr' must be manually freed using 'freeHaskellFunPtr' when
-- it is no longer needed.
claySetMeasureTextFunction :: MeasureTextFunction -> IO (FunPtr MeasureTextFunction)
claySetMeasureTextFunction measureTextFunctionWrapped = do
  measureTextFunctionWrappedPtr <-
    $(C.mkFunPtr [t| MeasureTextFunction |]) measureTextFunctionWrapped
  [C.block| void {
     Clay_Dimensions (*measureTextFunction)(Clay_String*, Clay_TextElementConfig*);
     measureTextFunction = (Clay_Dimensions (*)(Clay_String*, Clay_TextElementConfig*))({
       Clay_Dimensions innerWrapper(Clay_String* text, Clay_TextElementConfig* config) {
         Clay_Dimensions retDimensions;
         $(void (*measureTextFunctionWrappedPtr)(Clay_String*, Clay_TextElementConfig*, Clay_Dimensions*))(text, config, &retDimensions);
         return retDimensions;
       }
       innerWrapper;
     });
     Clay_SetMeasureTextFunction(measureTextFunction);
  }|]
  pure measureTextFunctionWrappedPtr

-- | Wrapped function pointer for 'claySetQueryScrollOffsetFunction', which wraps
-- @Clay_SetQueryScrollOffsetFunction@. Rather than accept the return value as a
-- pointer, it ought to be placed into the memory pointed to by the second argument.
type QueryScrollOffsetFunction
   = Word32 -- ^ elementId, [in]
  -> Ptr ClayVector2 -- ^ [out]
  -> IO ()

-- | @ void Clay_SetQueryScrollOffsetFunction(Clay_Vector2 (*queryScrollOffsetFunction)(uint32_t elementId)); @
--
-- The returned 'FunPtr' must be manually freed using 'freeHaskellFunPtr' when
-- it is no longer needed.
claySetQueryScrollOffsetFunction :: QueryScrollOffsetFunction -> IO (FunPtr QueryScrollOffsetFunction)
claySetQueryScrollOffsetFunction qsofWrapped = do
  qsofWrappedPtr <- $(C.mkFunPtr [t| QueryScrollOffsetFunction |]) qsofWrapped
  [C.block| void {
     Clay_Vector2 (*qsof)(uint32_t);
     qsof = (Clay_Vector2 (*)(uint32_t))({
       Clay_Vector2 innerWrapper(uint32_t elementId) {
         Clay_Vector2 retVec;
         $(void (*qsofWrappedPtr)(uint32_t, Clay_Vector2*))(elementId, &retVec);
         return retVec;
       }
       innerWrapper;
     });
     Clay_SetQueryScrollOffsetFunction(qsof);
  }|]

  pure qsofWrappedPtr

-- | @ Clay_RenderCommand* Clay_RenderCommandArray_Get(Clay_RenderCommandArray* array, int32_t index); @
foreign import capi "clay.h Clay_RenderCommandArray_Get"
  clayRenderCommandArrayGet :: Ptr ClayRenderCommandArray -> Int32 -> IO (Ptr ClayRenderCommand)

-- | @ void Clay_SetDebugModeEnabled(bool enabled); @
foreign import capi "clay.h Clay_SetDebugModeEnabled"
  claySetDebugModeEnabled :: Bool -> IO ()

-- | @ void Clay_SetCullingEnabled(bool enabled); @
foreign import capi "clay.h Clay_SetCullingEnabled"
  claySetCullingEnabled :: Bool -> IO ()

-- | @ void Clay_SetMaxElementCount(uint32_t maxElementCount); @
foreign import capi "clay.h Clay_SetMaxElementCount"
  claySetMaxElementCount :: Word32 -> IO ()

-- | @ void Clay_SetMaxMeasureTextCacheWordCount(uint32_t maxMeasureTextCacheWordCount); @
foreign import capi "clay.h Clay_SetMaxMeasureTextCacheWordCount"
  claySetMaxMeasureTextCacheWordCount :: Word32 -> IO ()

-- Internal API Used by Macros -------------------------------------------------

{-
Functions to wrap:
- Public API:

- Private API (used by macros)
void Clay__OpenElement(void);
void Clay__CloseElement(void);
Clay_LayoutConfig * Clay__StoreLayoutConfig(Clay_LayoutConfig config);
void Clay__ElementPostConfiguration(void);
void Clay__AttachId(Clay_ElementId id);
void Clay__AttachLayoutConfig(Clay_LayoutConfig *config);
void Clay__AttachElementConfig(Clay_ElementConfigUnion config, Clay__ElementConfigType type);
Clay_RectangleElementConfig * Clay__StoreRectangleElementConfig(Clay_RectangleElementConfig config);
Clay_TextElementConfig * Clay__StoreTextElementConfig(Clay_TextElementConfig config);
Clay_ImageElementConfig * Clay__StoreImageElementConfig(Clay_ImageElementConfig config);
Clay_FloatingElementConfig * Clay__StoreFloatingElementConfig(Clay_FloatingElementConfig config);
Clay_CustomElementConfig * Clay__StoreCustomElementConfig(Clay_CustomElementConfig config);
Clay_ScrollElementConfig * Clay__StoreScrollElementConfig(Clay_ScrollElementConfig config);
Clay_BorderElementConfig * Clay__StoreBorderElementConfig(Clay_BorderElementConfig config);
Clay_ElementId Clay__HashString(Clay_String key, uint32_t offset, uint32_t seed);
void Clay__Noop(void);
void Clay__OpenTextElement(Clay_String text, Clay_TextElementConfig *textConfig);

Types to wrap:
- Clay_ErrorHandler
- Clay_RenderCommandArray*
-}

-- UTILITIES -------------------------------------------------------------------

-- | Like 'alloca', but 'poke's the provided value into the pointer before
-- calling the action with the allocated 'Ptr'.
withValPtr :: Storable a => a -> (Ptr a -> IO b) -> IO b
withValPtr a action =
  alloca \ptrA -> do
    poke ptrA a
    action ptrA

-- | Like 'withValPtr' but for two values.
withValPtr2 :: (Storable a, Storable b) => a -> b -> (Ptr a -> Ptr b -> IO c) -> IO c
withValPtr2 a b action =
  alloca \ptrA ->
    alloca \ptrB -> do
      poke ptrA a
      poke ptrB b
      action ptrA ptrB

-- | Like 'withValPtr' but for three values.
withValPtr3 :: (Storable a, Storable b, Storable c) => a -> b -> c -> (Ptr a -> Ptr b -> Ptr c -> IO d) -> IO d
withValPtr3 a b c action =
  alloca \ptrA ->
    alloca \ptrB ->
      alloca \ptrC -> do
        poke ptrA a
        poke ptrB b
        poke ptrC c
        action ptrA ptrB ptrC
