module CMLIR
    ( translate
    ) where


import Data.List
import Options.Applicative
import System.Environment
import System.IO
import System.Exit
import CMLIR.Parser
import CMLIR.Translator

specialOpts = ["-jit", "-llvm", "-loc", "-noopt"]

translate :: IO ()
translate =
  do args <- getArgs
     let (cppOpts', files) = partition (isPrefixOf "-") args
         (jits, cppOpts) = partition (isPrefixOf "-jit=") cppOpts'
         hasOpt opt = opt `elem` cppOpts 
         trOpts = defaultOptions{toLLVM = hasOpt "-llvm" || not (null jits),
                                 dumpLoc = hasOpt "-loc",
                                 jits = map (drop 5) jits,
                                 simplize = not $ hasOpt "-noopt"} 
     mapM_ (\file -> do
       tu <- processFile (cppOpts \\ specialOpts) file
       ir <- translateToMLIR trOpts tu
       case ir of
         Left err -> do
           hPutStrLn stderr err
           exitWith (ExitFailure 1)
         Right res -> putStrLn res) files
