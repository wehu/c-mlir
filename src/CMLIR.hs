module CMLIR
    ( translate
    ) where


import Options.Applicative
import System.Environment
import System.IO
import System.Exit
import CMLIR.Parser
import qualified CMLIR.Translator as T


data CmdOpts = CmdOpts
  { files      :: [String]
  , jits       :: [String]
  , llvm       :: Bool
  , noopt      :: Bool
  , simplize   :: Bool
  , loc        :: Bool}

cmdP :: Parser CmdOpts
cmdP = CmdOpts
      <$> some (argument str (metavar "FILES..."))
      <*> many (strOption
            ( long "jit"
            <> metavar "JIT"
            <> help "Function name to run jit" ))
      <*> switch
          ( long "llvm"
         <> help "Lower to llvm or not" )
      <*> switch
          ( long "nopot"
         <> help "Do not do any IR optimization" )
      <*> switch
          ( long "simplize"
         <> help "Simplize the IR" )
      <*> switch
          ( long "loc"
         <> help "Dump the IR with location" )

opts :: ParserInfo CmdOpts
opts = info (cmdP <**> helper)
  ( fullDesc
  <> progDesc "Compile C to mlir IR"
  <> header "c-mlir - compile c to mlir IR" )

translate :: IO ()
translate =
  do options <- execParser opts
     let trOpts = T.defaultOptions{T.toLLVM = llvm options || not (null $ jits options),
                                   T.dumpLoc = loc options,
                                   T.jits = jits options,
                                   T.simplize = not $ noopt options} 
     mapM_ (\file -> do
       tu <- processFile [] file
       ir <- T.translateToMLIR trOpts tu
       case ir of
         Left err -> do
           hPutStrLn stderr err
           exitWith (ExitFailure 1)
         Right res -> putStrLn res) (files options)

