module Clay.Raw.Context where

import Clay.Raw.Types
import Data.Map qualified as Map
import Language.C.Inline.Context
import Language.C.Types (pattern TypeName)

clayContext :: Context
clayContext = baseCtx <> mempty
  { ctxTypesTable =
    Map.fromList
      [ (TypeName "Clay_Arena", [t| ClayArena |])
      , (TypeName "Clay_String", [t| ClayString |])
      , (TypeName "Clay_Vector2", [t| ClayVector2 |])
      , (TypeName "Clay_Dimensions", [t| ClayDimensions |])
      , (TypeName "Clay_ErrorHandler", [t| ClayErrorHandler |])
      , (TypeName "Clay_RenderCommandArray", [t| ClayRenderCommandArray |])
      , (TypeName "Clay_ElementId", [t| ClayElementId |])
      , (TypeName "Clay_ScrollContainerData", [t| ClayScrollContainerData |])
      , (TypeName "Clay_ScrollElementConfig", [t| ClayScrollElementConfig |])
      ]
  }
