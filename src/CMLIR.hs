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
     let (cppOpts', files) = partition (isPrefixOf "-") args
         (jits, cppOpts) = partition (isPrefixOf "-jit=") cppOpts'
         trOpts = defaultOptions{toLLVM = "-llvm" `elem` cppOpts || not (null jits),
                                 dumpLoc = "-loc" `elem` cppOpts,
                                 jits = map (drop 5) jits} 
     mapM_ (\file -> processFile (cppOpts \\ specialOpts) file
       >>= translateToMLIR trOpts >>= putStrLn) files
