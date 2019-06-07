#!/usr/bin/env stack

module Main
where
import           Control.Applicative ( liftA2, (<$>) )
import           Control.Monad
import           Data.List ( groupBy, intercalate, uncons )
import           Data.Maybe
import           Data.Semigroup ( (<>) )
import           Data.Text ( strip )
import           GHC.Base ( returnIO )
import           System.Directory
import           System.FilePath.Posix
import           System.IO ( stdin )
import           Text.Regex.Applicative
import           Text.Read ( readEither )
import           TmpRewriteOpts

data CmdPart =   Rest String               -- a non-translated string of the command
               | InputFile String          -- an input file
               | OutputFile String         -- an output file
               | OutputDir String          -- an output directory
               | InvalidRewriteable String -- error state for the command parser
               deriving Show

rest :: CmdPart -> String
rest (Rest s) = s

bracedToken :: RE Char String -> RE Char String
bracedToken f =  sym '{' *> many (psym (/= ':')) <* sym ':' <* f <* sym '}'

rewriteableToken :: String -> RE Char String
rewriteableToken c =  bracedToken $ string c

-- Parse the command into a list of command parts
cmdToken :: RE Char CmdPart
cmdToken = (InputFile <$> rewriteableToken "i")
    <|> (OutputFile <$> rewriteableToken "o")
    <|> (OutputDir <$> rewriteableToken "d")
    <|> (InvalidRewriteable <$> bracedToken (many anySym))

parseCmd :: String -> Either String [CmdPart]
parseCmd s = let getRest r = guard ((not . null) r) >> [Rest r]
                 parseCmd' cs r s@(h:t) =
                  let next = findFirstPrefix cmdToken s in
                        case next of
                          Just (InvalidRewriteable e, _) -> Left $ "Invalid rewriteable:" ++ e ++ "\n" ++ rewriteableDoc
                          Just (cmd, suf) -> parseCmd' (cs ++ getRest r ++ [cmd]) "" suf
                          Nothing -> parseCmd' cs (r ++ [h]) t
                 parseCmd' cs r "" = Right (cs ++ getRest r)
              in parseCmd' [] "" s

-- helper function to filter command parts of certain types
applyFileName :: [CmdPart -> Maybe String] -> CmdPart -> Maybe String
applyFileName fs x =
    let filenames = mapMaybe (\f -> f x) fs
    in (fmap fst . uncons) filenames

-- accessors for input and output file names
inputFileName, outputFileName, outputDirName, rewriteableFileName, outputPath ::
    CmdPart -> Maybe String
inputFileName x = case x of {(InputFile s) -> Just s; _ -> Nothing}
outputFileName x = case x of {(OutputFile s) -> Just s; _ -> Nothing}
outputDirName x = case x of {(OutputDir s) -> Just s; _ -> Nothing}
rewriteableFileName =  applyFileName [inputFileName, outputFileName, outputDirName]
outputPath = applyFileName [outputFileName, outputDirName]

lockedCopy :: TmpSettings -> String -> String
lockedCopy s c = case mode s of
    "lock" -> concat [ "set -e ; (flock -w "
                     , show $ wait s
                     , " 200 ; hostname > "
                     , lockfile s
                     , "-host ; " , c , ") 200> "
                     , lockfile s ]
    _ -> c

rewriteToTmp :: TmpSettings -> String -> String
rewriteToTmp s p =
  case mode s of
    "test" -> p
    _ -> tmploc s </> takeFileName p

mkdirRewrite :: TmpSettings -> [CmdPart] -> Maybe String
mkdirRewrite s cmdParts =
  let rawDirs = mapMaybe outputDirName cmdParts
      dirs = map (rewriteToTmp s) rawDirs
    in case dirs of
        [] -> Nothing
        _ -> Just $ "mkdir -p " ++ unwords dirs

cpRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cpRewrite s cmdParts =
  let inputs = mapMaybe inputFileName cmdParts
      cpRewrite' [] _ = Nothing
      cpRewrite' _ "test" = Nothing
      cpRewrite' fs _ = Just $ lockedCopy s $ unwords ([cpcmd s] ++ fs ++ [tmploc s])
   in cpRewrite' inputs (mode s)

cmdRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cmdRewrite s cmdParts = Just $ concatMap rewritePart cmdParts
  where
    rewritePart part
         | Just p <- rewriteableFileName part = rewriteToTmp s p
         | otherwise = rest part

cpBackRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cpBackRewrite s cmdParts =
  let outputs = mapMaybe outputPath cmdParts
      outputSources = map (rewriteToTmp s) outputs
      outputDests = map takeDirectory outputs
      cpBackCmd src dest = unwords [cpcmd s, src, dest]
      cpBacks =
        intercalate " ; " $ zipWith cpBackCmd outputSources outputDests
    in case mode s of
        "test" -> Nothing
        _ ->
          case cpBacks of
            [] -> Nothing
            _ -> Just cpBacks

reportErrors :: Bool -> String -> IO ()
reportErrors True _ = return ()
reportErrors _ "" = return ()
reportErrors False s = error $ "\n" ++ s ++
  "\nFix these errors or use --ignore-errors to suppress them\n"

checkLocations :: TmpSettings -> [CmdPart] -> IO ()
checkLocations s cmdParts = do
  missingInputs <- filterNotM doesPathExist inputs
  inputErrors <- concat <$> mapM (makeError "File does not exist: ") missingInputs
  missingDirs <- filterNotM doesDirectoryExist dirDests
  dirErrors <- concat <$> mapM (makeError "Directory does not exist: ")  missingDirs
  conflictingPaths <- filterM doesPathExist outPaths
  conflictErrors <- concat <$> mapM (makeError "Destination is an existing filesystem object: ") conflictingPaths
  reportErrors (ignoreErrors s) (inputErrors ++ dirErrors ++ conflictErrors)
  where
    inputs = mapMaybe inputFileName cmdParts
    dirs = mapMaybe outputDirName cmdParts
    outPaths = mapMaybe outputPath cmdParts
    dirDests = map takeDirectory dirs
    filterNotM p = filterM (fmap not . p)
    makeError prefix f = return (prefix ++ f ++ "\n")

rewriteCmdParts :: TmpSettings -> [CmdPart] -> String
rewriteCmdParts settings cmdParts = intercalate " ; " $
    mapMaybe (\f -> f settings cmdParts)
              [ mkdirRewrite
              , cpRewrite
              , cmdRewrite
              , cpBackRewrite
              ]

rewrite :: TmpSettings -> String -> IO String
rewrite settings cmd = do
  parsedCmd <- either error return $ parseCmd cmd
  checkLocations settings parsedCmd
  return $ rewriteCmdParts settings parsedCmd

main :: IO ()
main = do
  settings <- parseTmpSettings
  ls <- case cmd settings of
    Nothing -> fmap lines getContents
    Just template -> returnIO [template]
  out <- mapM (rewrite settings) ls
  putStrLn $ intercalate "\n" out