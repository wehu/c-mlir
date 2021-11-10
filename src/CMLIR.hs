module CMLIR
    ( translate
    ) where


import Data.List
import Options.Applicative
import System.Environment
import CMLIR.Parser
import CMLIR.Translator
import System.IO
import System.Exit (exitWith, ExitCode (ExitFailure))

specialOpts = ["-jit", "-llvm", "-loc", "-noopt"]

translate :: IO ()
translate =
  do args <- getArgs
     let (cppOpts', files) = partition (isPrefixOf "-") args
         (jits, cppOpts) = partition (isPrefixOf "-jit=") cppOpts'
         trOpts = defaultOptions{toLLVM = "-llvm" `elem` cppOpts || not (null jits),
                                 dumpLoc = "-loc" `elem` cppOpts,
                                 jits = map (drop 5) jits,
                                 simplize = "-noopt" `notElem` cppOpts} 
     mapM_ (\file -> do
       tu <- processFile (cppOpts \\ specialOpts) file
       ir <- translateToMLIR trOpts tu
       case ir of
         Left err -> do
           hPutStrLn stderr err
           exitWith (ExitFailure 1)
         Right res -> putStrLn res) files
