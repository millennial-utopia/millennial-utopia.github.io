module Debug where

import GHC.IO.Unsafe (unsafePerformIO)

{-# WARNING dbg, dbg' "Don't use these!" #-}

dbg :: Show a => a -> a
dbg x = seq (unsafePerformIO $ print x) x

dbg' :: Show a => String -> a -> a
dbg' m x = seq (unsafePerformIO $ putStrLn $ m ++ " - " ++ show x) x
