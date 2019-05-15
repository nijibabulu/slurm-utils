#!/usr/bin/env stack

module Main 
where
import           Control.Applicative ( liftA2, (<**>) )
import           Control.Monad
import           Control.Monad.Trans.Writer.Strict
import           Data.List ( groupBy, intercalate )
import           Data.Maybe
import           Data.Semigroup ( (<>) )
import           Data.Text ( strip )
import qualified Options.Applicative as O
import qualified Options.Applicative.Help.Pretty as P
import           System.Directory
import           System.FilePath.Posix
import           Text.ParserCombinators.Parsec
import           Text.Read ( readEither )

data CmdPart = RestChar Char        -- a non-translated character of the command
             | Rest String          -- a non-translated string of the command
             | InputFile String     -- an input file
             | OutputFile String    -- an output file
             | OutputDir String     -- an output directory
             deriving Show

rest :: CmdPart -> String
rest (Rest s) = s

applyFileName fs x =
    let fns = catMaybes $ map (\f -> f x) fs
    in case fns of 
        [] -> Nothing
        [f] -> Just f

-- accessors for input and output file names
inputFileName, outputFileName, outputDirName, rewriteableFileName, outputPath :: 
    CmdPart -> Maybe String
inputFileName x = case x of {(InputFile s) -> Just s; otherwise -> Nothing}
outputFileName x = case x of {(OutputFile s) -> Just s; otherwise -> Nothing}
outputDirName x = case x of {(OutputDir s) -> Just s; otherwise -> Nothing}
rewriteableFileName =  applyFileName [inputFileName, outputFileName, outputDirName]
outputPath = applyFileName [outputFileName, outputDirName]

-- flatten many RestChars in to a single Rest
flattenRestChars :: [CmdPart] -> CmdPart
flattenRestChars xs@((RestChar h):t) = Rest $ map (\(RestChar c) -> c) xs
flattenRestChars xs@(h:[]) = h

-- Determine if two CmdParts are both RestChars
matchRestChar :: CmdPart -> CmdPart -> Bool
matchRestChar (RestChar _) (RestChar _) = True
matchRestChar _ _ = False

-- Parse the command into a list of command parts
parseCmd :: String -> Either String [CmdPart]
parseCmd str =
  let parser = many tmpCmdParser
    in case parse parser "" str of
        Left err -> Left $ show err
        Right parts ->
          Right $ map flattenRestChars $ groupBy matchRestChar parts

tmpCmdParser :: Parser CmdPart 
tmpCmdParser =
  try inputParser <|> try outputParser <|> try dirParser <|>
    restParser <?> "Parse error"

restParser :: Parser CmdPart
restParser = RestChar <$> anyChar

-- the parser generator
-- t is the type of the output
-- c is the character expected in the {PATH:c} syntax
gen_parser :: (String -> CmdPart) -> Char -> Parser CmdPart
gen_parser t c = do
    t <$> between (string "{")
                  (string $ ":" ++ [c] ++ "}")
                  (many $ satisfy (\a -> a/= ':'))

inputParser, outputParser, dirParser :: Parser CmdPart
inputParser = gen_parser InputFile 'i'
outputParser = gen_parser OutputFile 'o'
dirParser = gen_parser OutputDir 'd'

data TmpSettings = TmpSettings 
  { mode :: String
    , wait :: Integer
    , tmploc :: String
    , cpcmd :: String
    , lockfile :: String
    , cmdParts :: [CmdPart]
  } deriving (Show)

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
    otherwise -> (tmploc s) </> takeBaseName p

mkdirRewrite :: TmpSettings -> Maybe String
mkdirRewrite s =
  let rawDirs = catMaybes $ map outputDirName $ cmdParts s
      dirs = map (rewriteToTmp s) rawDirs
    in case dirs of
        [] -> Nothing
        otherwise -> Just $ "mkdir -p " ++ intercalate " " dirs

cpRewrite :: TmpSettings -> Maybe String
cpRewrite s =
  let inputs = catMaybes $ map inputFileName $ cmdParts s
      cpRewrite' [] _ = Nothing
      cpRewrite' _ "test" = Nothing
      cpRewrite' fs _ = Just $ lockedCopy s $ intercalate " " ([cpcmd s] ++ fs ++ [tmploc s])
   in cpRewrite' inputs (mode s)

cmdRewrite :: TmpSettings -> Maybe String
cmdRewrite s = Just $ foldl (++) "" $ map rewritePart $ cmdParts s
  where
    rewritePart part 
         | Just p <- rewriteableFileName part = (rewriteToTmp s p)
         | otherwise = rest part

cpBackRewrite :: TmpSettings -> Maybe String
cpBackRewrite s =
  let outputs = catMaybes $ map outputPath $ cmdParts s
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

{-loggedMissing :: (FilePath -> IO Bool) -> String -> FilePath -> Int -> Writer [String] Int
loggedMissing p l f n =
  case p f of
    False -> tell [ l ++ f ] >>= return (n + 1)
    True -> return n--}

-- (\n -> 
--  liftM (logMissing n "Directory does not exist: ") (filterNotM doesDirectoryExist dirDests)

logMissing :: Int -> String -> [String] -> Writer [String] Int
logMissing n prefix (f:fs) = 
  tell [prefix ++ f ++ "\n"] >> logMissing (n+1) prefix fs
logMissing n _ [] = return n

checkLocationsMonadic :: TmpSettings -> IO ()
checkLocationsMonadic s = 
  liftM (logMissing 0 "File does not exist: ") missingInputs >>= (\w -> 
    liftM (logMissing ((fst . runWriter) w) "Directory does not exist: ") missingDirs) >>= (\w -> 
      reportErrors (runWriter w))
  where
    inputs = catMaybes $ map inputFileName $ cmdParts s
    missingInputs = filterNotM doesFileExist inputs
    dirs = catMaybes $ map outputDirName $ cmdParts s
    dirDests = map takeDirectory dirs 
    missingDirs = filterNotM doesDirectoryExist dirDests
    filterNotM p = filterM ((liftM not) . p)
    reportErrors (0, _) = return ()
    reportErrors (_, s) = error (concat s)

-- this could be much better (Writer monad?)
checkLocations :: TmpSettings -> IO ()
checkLocations s = do
    let inputs = catMaybes $ map inputFileName $ cmdParts s
    missingFiles <- filterNotM doesFileExist inputs
    fileErrors <- mapM (makeError "File does not exist:") missingFiles
    let dirs = catMaybes $ map outputDirName $ cmdParts s
    let dirDests = map takeDirectory dirs
    missingDirs <- filterNotM doesDirectoryExist dirDests
    dirErrors <- mapM (makeError "Directory does not exist:") missingDirs
    let error = ((length missingFiles) + (length missingDirs) > 0)
    let errors = concatMap concat [fileErrors, dirErrors]
    reportError error errors
    where 
        makeError prefix f = return (prefix ++ f ++ "\n") 
        reportError True e = error e
        reportError False _ = return ()
        filterNotM p = filterM ((liftM not) . p)

paragraph :: String -> P.Doc
paragraph = P.fillSep . map (P.text) . words

modes = ["lock", "test", "nolock"]
modeHelpDoc =
  P.hang 2 $
  P.vsep $
  map
    paragraph
    [ "Running mode:"
    , "lock - include an flock on copy to the temporary directory."
    , "nolock - copy to the temporary directory."
    , "test - create commands rewriting the filenames without copying."
    ]

tmpOptions :: O.Parser TmpSettings
tmpOptions =
  TmpSettings <$>
  O.option
    (choicesReader modes)
    (O.long "mode" <> O.value "lock" <> O.showDefault <>
     O.helpDoc (Just modeHelpDoc)) <*>
  O.option
    O.auto
    (O.long "wait" <> O.value 7200 <> O.showDefault <>
     O.help "How long to wait before giving up the lock") <*>
  O.strOption
    (O.long "tmploc" <> O.value "$TMPDIR" <> O.showDefault <>
     O.help "Location of the temporary directory to copy to") <*>
  O.strOption
    (O.long "cpcmd" <> O.value "cp -Lr" <> O.showDefault <>
     O.help "Copy command to use") <*>
  O.strOption
    (O.long "lockfile" <> O.value "LOCK" <> O.showDefault <>
     O.help "Name of the lock file") <*>
  O.argument (O.eitherReader parseCmd) (O.metavar "(TEMPLATE|-)")

progDescDoc = P.vsep $
  [ paragraph ("Automatically wrap a command with copies to and" ++
               "from a temporary directory and rewrite the " ++
               "command to use files in the temporary directory. " ++
               "Uses a formatting syntax of {INPUT:i} for input " ++
               "file INPUT, {DIR:d} for output directory DIR (DIR " ++ 
               "is created) and {OUTPUT:o} for output file OUTPUT." ++
               "For example,")
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

rewrite :: TmpSettings -> String 
rewrite settings = intercalate " ; " $ 
    catMaybes [ mkdirRewrite settings
              , cpRewrite settings
              , cmdRewrite settings
              , cpBackRewrite settings
              ]

main = do
  settings <- O.customExecParser parserPrefs parserInfo
  checkLocationsMonadic settings
  putStrLn $ rewrite settings

