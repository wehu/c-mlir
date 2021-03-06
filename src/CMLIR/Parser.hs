module CMLIR.Parser (processFile, processString) where

import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Data.Position
import Language.C.System.GCC
import Control.Monad
import System.IO
import System.Exit
import qualified Data.ByteString.UTF8 as BU

analyse :: CTranslUnit -> Either String CTranslUnit
analyse tu =
  case runTrav_ (body tu) of
    Left errs -> Left $ join $ map show errs
    Right (_,errs) -> Right tu
  where body tu =
          do modifyOptions (\opts -> opts {language = GNU99})
             analyseAST tu

processFile :: [String] -> FilePath -> IO CTranslUnit 
processFile cppOpts file =
  do result <- parseCFile (newGCC "gcc") Nothing cppOpts file
     case result of
       Left err -> do
         hPutStrLn stderr ('\n' : show err)
         hPutStrLn stderr "Failed: Parse Error"
         exitWith (ExitFailure 1)
       Right tu -> case analyse tu of
                     Left err -> do
                       hPutStrLn stderr err
                       exitWith (ExitFailure 1)
                     Right tu -> return tu

processString :: String -> Either ParseError CTranslUnit 
processString input = parseC (BU.fromString input) (position 0 "" 0 0 Nothing)
