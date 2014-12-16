module Util
    (verbosePut)
where

import Control.Monad

verbosePut :: Int -> Int -> String -> IO ()
verbosePut verbosity level str =
        when (verbosity >= level) (putStrLn str)

