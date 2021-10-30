module CMLIR
    ( translate
    ) where


import Data.List
import Options.Applicative
import System.Environment
import CMLIR.Parser
import CMLIR.Translator (translateToMLIR)

translate :: IO ()
translate =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
     mapM_ (\file -> processFile cppOpts file >>= translateToMLIR >>= putStrLn) files
