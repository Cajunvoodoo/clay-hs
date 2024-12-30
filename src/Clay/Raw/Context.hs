module Clay.Raw.Context where

import Clay.Raw.Types
import Data.Map qualified as Map
import Language.C.Inline.Context
import Language.C.Types (pattern TypeName)
import Foreign.C.Types
import Foreign

clayContext :: Context
clayContext = baseCtx <> mempty
  { ctxTypesTable =
    Map.fromList
      [ (TypeName "Clay_Arena", [t| ClayArena |])
      , (TypeName "Clay_Vector2", [t| ClayVector2 |])
      , (TypeName "Clay_Dimensions", [t| ClayDimensions |])
      , (TypeName "Clay_ErrorHandler", [t| ClayErrorHandler |])
      -- ( TypeName "ImVec2", [t| ImVec2 |] )
      ]
  }
