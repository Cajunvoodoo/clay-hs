module Clay.Raw.Functions where

import Clay.Raw.Context (clayContext)
import Clay.Raw.Types
import Foreign
import Foreign.C.Types
import Language.C.Inline qualified as C

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

{-
Functions to wrap:
- Public API:
Clay_RenderCommandArray Clay_EndLayout(void);
Clay_ElementId Clay_GetElementId(Clay_String idString);
Clay_ElementId Clay_GetElementIdWithIndex(Clay_String idString, uint32_t index);
bool Clay_Hovered(void);
void Clay_OnHover(void (*onHoverFunction)(Clay_ElementId elementId, Clay_PointerData pointerData, intptr_t userData), intptr_t userData);
bool Clay_PointerOver(Clay_ElementId elementId);
Clay_ScrollContainerData Clay_GetScrollContainerData(Clay_ElementId id);
void Clay_SetMeasureTextFunction(Clay_Dimensions (*measureTextFunction)(Clay_String *text, Clay_TextElementConfig *config));
void Clay_SetQueryScrollOffsetFunction(Clay_Vector2 (*queryScrollOffsetFunction)(uint32_t elementId));
Clay_RenderCommand * Clay_RenderCommandArray_Get(Clay_RenderCommandArray* array, int32_t index);
void Clay_SetDebugModeEnabled(bool enabled);
void Clay_SetCullingEnabled(bool enabled);
void Clay_SetMaxElementCount(uint32_t maxElementCount);
void Clay_SetMaxMeasureTextCacheWordCount(uint32_t maxMeasureTextCacheWordCount);

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
- Clay_PointerData
- Clay_TextElementConfig
- Clay_RenderCommandArray*
- (ClayString* text, Clay_TextElementConfig* config) -> Clay_Dimensions ("measureTextFunction")
- (uint32_t elementId) -> Clay_Vector2 ("queryScrollOffsetFunction")
- (Clay_ElementId elementId, Clay_PointerData pointerData, intptr_t userData) -> IO () ("onHoverFunction")
- Clay_
- Clay_
- Clay_
- Clay_
- Clay_
- Clay_
-}

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
