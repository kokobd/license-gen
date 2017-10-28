{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import qualified JiaHe.LM.App.App as App

-- re-export main function
main :: IO ()
main =
#ifdef READ_ONLY
  App.mainReadOnly
#else
  App.main
#endif
