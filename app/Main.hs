{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import MiniParsec

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a
