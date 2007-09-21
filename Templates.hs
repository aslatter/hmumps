{-# OPTIONS -fth #-}

module Templates(bakedString) where

import Foreign
import System.IO

bakedString file =
  let x = unsafePerformIO $ do handle <- openFile file ReadMode 
                               hGetContents handle
  in [| x |]
