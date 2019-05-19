#!/usr/bin/env stack

module Main 
where
import           Control.Applicative ( liftA2, (<**>) )
import           Control.Monad
import           Data.List ( groupBy, intercalate, uncons )
import           Data.Maybe
import           Data.Semigroup ( (<>) )
import           Data.Text ( strip )
import           GHC.Base ( returnIO )
import qualified Options.Applicative as O
import qualified Options.Applicative.Help.Pretty as P
import           System.Directory
import           System.FilePath.Posix
import           System.IO ( stdin )
import           Text.Regex.Applicative
import           Text.Read ( readEither )

data CmdPart =   Rest String               -- a non-translated string of the command
               | InputFile String          -- an input file
               | OutputFile String         -- an output file
               | OutputDir String          -- an output directory
               | InvalidRewriteable String -- error state for the command parser
               deriving Show

rest :: CmdPart -> String
rest (Rest s) = s

-- helper function to filter command parts of certain types
applyFileName :: [CmdPart -> Maybe String] -> CmdPart -> Maybe String
applyFileName fs x =
    let filenames = catMaybes $ map (\f -> f x) fs
    in (fmap fst . uncons) filenames

-- accessors for input and output file names
inputFileName, outputFileName, outputDirName, rewriteableFileName, outputPath :: 
    CmdPart -> Maybe String
inputFileName x = case x of {(InputFile s) -> Just s; otherwise -> Nothing}
outputFileName x = case x of {(OutputFile s) -> Just s; otherwise -> Nothing}
outputDirName x = case x of {(OutputDir s) -> Just s; otherwise -> Nothing}
rewriteableFileName =  applyFileName [inputFileName, outputFileName, outputDirName]
outputPath = applyFileName [outputFileName, outputDirName]

bracedToken :: RE Char [Char] -> RE Char [Char]
bracedToken f =  sym '{' *> many (psym (/= ':')) <* sym ':' <* f <* sym '}'

rewriteableToken :: [Char] -> RE Char [Char]
rewriteableToken c =  bracedToken $ string c 

-- Parse the command into a list of command parts
cmdToken :: RE Char CmdPart
cmdToken = (InputFile <$> rewriteableToken "i") 
    <|> (OutputFile <$> rewriteableToken "o")
    <|> (OutputDir <$> rewriteableToken "d")
    <|> (InvalidRewriteable <$> bracedToken (many anySym))

parseCmd :: String -> Either String [CmdPart]
parseCmd s = let getRest "" = []
                 getRest r = [Rest r]
                 parseCmd' cs r s@(h:t) = 
                  let next = findFirstPrefix cmdToken s in
                        case next of
                          Just (InvalidRewriteable e, _) -> Left $ "Invalid rewriteable:" ++ e ++ "\n" ++ rewriteableInfo
                          Just (cmd, suf) -> parseCmd' (cs ++ (getRest r) ++ [cmd]) "" suf
                          Nothing -> parseCmd' cs (r ++ [h]) t
                 parseCmd' cs r "" = Right (cs ++ (getRest r))
              in parseCmd' [] "" s

choicesReader :: [String] -> O.ReadM String
choicesReader xs = O.eitherReader $ checkInput xs
  where
        checkInput xs input 
            | input `elem` xs = Right input 
            | otherwise = Left $ "Must choose one of " ++ (intercalate ", " xs)

lockedCopy :: TmpSettings -> String -> String
lockedCopy s c = case (mode s) of
    "lock" -> foldl (++) "" [ "set -e ; (flock -w "
                            , (show $ wait s)
                            , " 200 ; hostname > "
                            , (lockfile s)
                            , "-host ; " , c , ") 200> "
                            , (lockfile s) ]
    otherwise -> c

rewriteToTmp :: TmpSettings -> String -> String
rewriteToTmp s p =
  case mode s of
    "test" -> p
    otherwise -> (tmploc s) </> takeFileName p

mkdirRewrite :: TmpSettings -> [CmdPart] -> Maybe String
mkdirRewrite s cmdParts =
  let rawDirs = catMaybes $ map outputDirName $ cmdParts
      dirs = map (rewriteToTmp s) rawDirs
    in case dirs of
        [] -> Nothing
        otherwise -> Just $ "mkdir -p " ++ intercalate " " dirs

cpRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cpRewrite s cmdParts =
  let inputs = catMaybes $ map inputFileName $ cmdParts
      cpRewrite' [] _ = Nothing
      cpRewrite' _ "test" = Nothing
      cpRewrite' fs _ = Just $ lockedCopy s $ intercalate " " ([cpcmd s] ++ fs ++ [tmploc s])
   in cpRewrite' inputs (mode s)

cmdRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cmdRewrite s cmdParts = Just $ foldl (++) "" $ map rewritePart cmdParts
  where
    rewritePart part 
         | Just p <- rewriteableFileName part = (rewriteToTmp s p)
         | otherwise = rest part

cpBackRewrite :: TmpSettings -> [CmdPart] -> Maybe String
cpBackRewrite s cmdParts =
  let outputs = catMaybes $ map outputPath $ cmdParts
      outputSources = map (rewriteToTmp s) outputs
      outputDests = map takeDirectory outputs
      cpBackCmd (src, dest) = intercalate " " [(cpcmd s), src, dest]
      cpBacks =
        intercalate " ; " $ map cpBackCmd $ zip outputSources outputDests
    in case (mode s) of 
        "test" -> Nothing
        otherwise ->
          case cpBacks of
            [] -> Nothing
            otherwise -> Just $ cpBacks

reportErrors :: Bool -> String -> IO ()
reportErrors True _ = return ()
reportErrors _ "" = return ()
reportErrors False s = error $ "\n" ++ s ++ 
  "\nFix these errors or use --ignore-errors to suppress them\n"

checkLocations :: TmpSettings -> [CmdPart] -> IO ()
checkLocations s cmdParts = do
  missingInputs <- filterNotM doesFileExist inputs
  inputErrors <- (liftM concat) $ mapM (makeError "File does not exist: ") missingInputs
  missingDirs <- filterNotM doesDirectoryExist dirDests
  dirErrors <- (liftM concat) $ mapM (makeError "Directory does not exist: ")  missingDirs
  reportErrors (ignoreErrors s) (inputErrors ++ dirErrors)
  where
    inputs = catMaybes $ map inputFileName $ cmdParts
    dirs = catMaybes $ map outputDirName $ cmdParts
    dirDests = map takeDirectory dirs 
    filterNotM p = filterM ((liftM not) . p)
    makeError prefix f = return (prefix ++ f ++ "\n") 

paragraph :: String -> P.Doc
paragraph = P.fillSep . map (P.text) . words

modes = ["lock", "test", "nolock"]
modeHelpDoc =
  P.hang 2 $
  P.vsep $
  map
    paragraph
    [ "Running mode:"
    , "test - create commands rewriting the filenames without copying."
    , "nolock - copy to the temporary directory."
    , "lock - include an flock on copy to the temporary directory."
    ]

data TmpSettings = TmpSettings 
  { mode :: String
  , wait :: Integer
  , tmploc :: String
  , cpcmd :: String
  , lockfile :: String
  , ignoreErrors :: Bool
  , cmd :: [String]
  } deriving (Show)

tmpOptions :: O.Parser TmpSettings
tmpOptions =
  TmpSettings <$>
  O.option
    (choicesReader modes)
    (O.long "mode" <> O.value "lock" <> O.showDefault 
  <> O.helpDoc (Just modeHelpDoc)) <*>
  O.option
    O.auto
    (O.long "wait" <> O.value 7200 <> O.showDefault 
  <> O.help "How long to wait before giving up the lock") <*>
  O.strOption
    (O.long "tmploc" <> O.value "$TMPDIR" <> O.showDefault 
  <> O.help "Location of the temporary directory to copy to") <*>
  O.strOption
    (O.long "cpcmd" <> O.value "cp -Lr" <> O.showDefault 
  <> O.help "Copy command to use") <*>
  O.strOption
    (O.long "lockfile" <> O.value "LOCK" <> O.showDefault 
  <> O.help "Name of the lock file") <*>
  O.switch 
    (O.long "ignore-errors"  
  <> O.help "Do not check for missing files before rewriting") <*>
  O.many (O.argument O.str (O.metavar "TEMPLATE..."))

rewriteableInfo :: String
rewriteableInfo = "Rewriteables use a formatting syntax of {INPUT:i} for input " 
              ++  "file INPUT, {DIR:d} for output directory DIR (DIR is created) " 
              ++  "and {OUTPUT:o} for output file OUTPUT."

progDescDoc = P.vsep $
  [ paragraph ("Automatically wrap a command with copies to and" 
            ++ "from a temporary directory and rewrite the " 
            ++ "command to use files in the temporary directory. "
            ++ rewriteableInfo
            ++ "For example,")
  , P.text ""
  , P.text  "tmp_rewrite \"cat {infile1:i} {infile2:i} > {outfile:o}\""
  , P.text ""
  , P.text "set -e ; (flock -w 7200 200 ; hostname > LOCK-host ; cp -Lr infile1 infile2 $TMPDIR) 200> LOCK ; cat $TMPDIR/infile1 $TMPDIR/infile2 > $TMPDIR/outfile ; cp -Lr $TMPDIR/outfile ."
  ]
parserInfo :: O.ParserInfo TmpSettings
parserInfo =
  O.info
    (tmpOptions <**> O.helper)
    (O.fullDesc <>
     O.progDescDoc (Just progDescDoc) <>
     O.header "tmp_rewrite - Rewrite command(s) in temporary space")

parserPrefs :: O.ParserPrefs
parserPrefs = O.prefs O.showHelpOnEmpty

rewriteCmdParts :: TmpSettings -> [CmdPart] -> String 
rewriteCmdParts settings cmdParts = intercalate " ; " $ 
    catMaybes $ map (\f -> f settings cmdParts) 
              [ mkdirRewrite 
              , cpRewrite 
              , cmdRewrite 
              , cpBackRewrite
              ]

rewrite :: TmpSettings -> String -> IO String
rewrite settings cmd = do 
  parsedCmd <- (either error return) $ parseCmd cmd
  checkLocations settings parsedCmd
  return $ rewriteCmdParts settings parsedCmd

main = do
  settings <- O.customExecParser parserPrefs parserInfo
  ls <- case (length (cmd settings)) of 
    0 -> ((liftM lines) getContents) 
    otherwise -> returnIO (cmd settings) 
  out <- mapM (rewrite settings) ls
  putStrLn $ intercalate "\n" out