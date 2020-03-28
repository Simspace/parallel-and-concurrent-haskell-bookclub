module BoundedClass where

class BoundedChan b where
    readChan :: b a -> IO a
    writeChan :: b a -> a -> IO ()
