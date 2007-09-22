{-# OPTIONS -fth #-}

module Templates(bakedString) where

import Foreign

bakedString file =
  let x = unsafePerformIO $ readFile file
  in [| x |]
