module ErrorHandler where

import qualified Control.Exception as Exc
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE safeCatch #-}
safeCatch :: a -> Maybe a
safeCatch x = unsafePerformIO $ Exc.catch (x `seq` return (Just x)) handler
  where handler exc = return Nothing  `const`  (exc :: Exc.ErrorCall)

pullOutIO :: IO a -> a
pullOutIO x = unsafePerformIO x