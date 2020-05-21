{-# LANGUAGE QualifiedDo #-}

import qualified Monad.Graded as Graded
import Vector


main = do
  putStrLn "The unqualified do still works."
  print $ toList $ Graded.do
    x <- VCons 1 (VCons 2 VNil)
    y <- VCons 1 (VCons 2 VNil)
    Graded.return (x :: Int, y :: Int)
