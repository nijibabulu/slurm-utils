module TmpRewrite (
    rewrite
  , parseCmd
)
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.List ( intercalate, nub, uncons )
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import Text.Regex.Applicative
import TmpRewriteOpts
import Utils

data CmdPart =   Rest {rest :: String}     -- a non-translated string of the command
               | InputFile String          -- an input file
               | IntermediateFile String   -- an intermediate file not to be copied back
               | OutputFile String         -- an output file
               | OutputDir String          -- an output directory
               | InvalidRewriteable String -- error state for the command parser
               deriving Show

bracedToken :: RE Char String -> RE Char String
bracedToken f =  sym '{' *> many (psym (/= ':')) <* sym ':' <* f <* sym '}'

rewriteableToken :: String -> RE Char String
rewriteableToken c =  bracedToken $ string c

-- Parse the command into a list of command parts
cmdToken :: RE Char CmdPart
cmdToken = (InputFile <$> rewriteableToken "i")
    <|> (OutputFile <$> rewriteableToken "o")
    <|> (IntermediateFile <$> rewriteableToken "n")
    <|> (OutputDir <$> rewriteableToken "d")
    <|> (InvalidRewriteable <$> bracedToken (many anySym))

parseCmd :: String -> Either String [CmdPart]
parseCmd s = let getRest r = guard ((not . null) r) >> [Rest r]
                 parseCmd' cs r s@(h:t) =
                  let next = findFirstPrefix cmdToken s in
                        case next of
                          Just (InvalidRewriteable e, _) -> Left $ "Invalid rewriteable:" ++ e ++ "\n" ++ (showDoc rewriteableDoc)
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
intermediateFileName x = case x of {(IntermediateFile s) -> Just s; _ -> Nothing}
outputFileName x = case x of {(OutputFile s) -> Just s; _ -> Nothing}
outputDirName x = case x of {(OutputDir s) -> Just s; _ -> Nothing}
rewriteableFileName =  applyFileName [inputFileName, intermediateFileName, outputFileName, outputDirName]
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


rewriteToRoot :: TmpSettings -> String -> String
rewriteToRoot s p@('/':_) = p
rewriteToRoot s p = root s </> takeFileName p

mkdirRewrite :: TmpSettings -> [CmdPart] -> Maybe String
mkdirRewrite s cmdParts =
  let rawDirs = mapMaybe outputDirName cmdParts
      dirs = map (rewriteToTmp s) rawDirs
    in case dirs of
        [] -> Nothing
        _ -> Just $ "mkdir -p " ++ unwords dirs

cpRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cpRewrite s cmdParts =
  let inputs = nub $ mapMaybe inputFileName cmdParts
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
  let outputs = nub $ mapMaybe outputPath cmdParts
      outputSources = map (rewriteToTmp s) outputs
      outputDests = map (rewriteToRoot s) outputs
      cpBackCmd src dest = unwords [cpcmd s, src, dest]
      cpBacks =
        intercalate " ; " $ zipWith cpBackCmd outputSources outputDests
      in guard(mode s /= "test" && (not . null) cpBacks) >> Just cpBacks

{-|
   @'logFileErrors' cmdParts wantFile validFile errPrefix@ logs errors for
   particular types of files. All @cmdParts@ resulting in @Just@ in
   @wantFile@ are validated with @validFile@. Each invalidated file
   is written to the writer monad with @errPrefix@ as a prefix.
-}
logFileErrors :: [CmdPart]                 -- Parts of the parsed command
              -> (CmdPart -> Maybe String) -- Predicate for parts of the commands to verify
              -> (FilePath -> IO Bool)     -- File validator; a False result will result in a logged error
              -> String                    -- A prefix to append to the error
              -> WriterT String IO ()
logFileErrors cmdParts wantFile validFile errPrefix = do
  badFiles <- filterM (fmap not . lift . validFile) $ mapMaybe wantFile cmdParts
  mapM_ (tell . makeError) badFiles
  where
    makeError f = errPrefix ++ f ++ "\n"

checkLocations :: [[CmdPart]] -> IO ()
checkLocations cmds =
  let checkCmdLocations cmd = do
        logFileErrors cmd inputFileName doesPathExist "File does not exist: "
        logFileErrors cmd (fmap takeDirectory . outputDirName) doesDirectoryExist "Directory does not exist: "
        logFileErrors cmd outputPath (fmap not . doesPathExist) "Destination is an existing filesystem object: "
        logFileErrors cmd (fmap takeDirectory . outputPath) doesDirectoryExist "Output file destination does not exist: "
  in do
    (_,e) <- runWriterT $ mapM checkCmdLocations cmds
    unless (null e) $ errorWithoutStackTrace $
        "\n" ++ e ++ "\nFix these errors or use --ignore-errors to suppress them\n"

rewriteCmdParts :: TmpSettings -> [CmdPart] -> String
rewriteCmdParts settings cmdParts = intercalate " ; " $
    mapMaybe (\f -> f settings cmdParts)
              [ mkdirRewrite
              , cpRewrite
              , cmdRewrite
              , cpBackRewrite
              ]

rewrite :: TmpSettings -> [String] -> IO [String]
rewrite settings cmds = do
  parsedCmds <- mapM (either error return . parseCmd) cmds
  unless (ignoreErrors settings) $ checkLocations parsedCmds
  return $ map (rewriteCmdParts settings) parsedCmds
