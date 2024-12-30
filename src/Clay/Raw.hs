{-# LANGUAGE CPP #-}

-- | Raw CApiFFI declarations for Clay. It is recommended to use the 'Clay' module
-- instead.
module Clay.Raw where

import Foreign.C.Types

-- foreign import capi "header.h f" f :: CInt -> IO CInt
