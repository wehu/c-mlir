module CMLIR
    ( translate
    ) where


import Data.List
import Options.Applicative
import System.Environment
import CMLIR.Parser
import CMLIR.Translator 

specialOpts = ["-jit", "-llvm", "-loc"]

translate :: IO ()
translate =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
         trOpts = defaultOptions{toLLVM = "-llvm" `elem` cppOpts || "-jit" `elem` cppOpts,
                                 dumpLoc = "-loc" `elem` cppOpts,
                                 jit = "-jit" `elem` cppOpts} 
     mapM_ (\file -> processFile (cppOpts \\ specialOpts) file
       >>= translateToMLIR trOpts >>= putStrLn) files
