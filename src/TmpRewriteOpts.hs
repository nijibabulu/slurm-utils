module TmpRewriteOpts (
      TmpSettings(..)
    , tmpOptions
    , rewriteableDoc
    , parserInfo
    , parserPrefs
    , parseTmpSettings
) where

import Data.Monoid ((<>))
import Data.List (intercalate)
import Options.Applicative 
import Options.Applicative.Help.Pretty
import System.Directory

import Utils

data TmpSettings = TmpSettings
  { mode :: String
  , wait :: Integer
  , tmploc :: String
  , cpcmd :: String
  , lockfile :: String
  , root :: String
  , ignoreErrors :: Bool
  , displayVersion :: Bool
  , cmd :: Maybe String
  } deriving (Show)


tmpOptions :: Parser TmpSettings
tmpOptions =
  TmpSettings
    <$> option
          (choicesReader modes)
          (long "mode" <> value "lock" <> showDefault <> helpDoc
            (Just modeHelpDoc)
          )
    <*> option
          auto
          (long "wait" <> value 7200 <> showDefault <> help
            "How long to wait before giving up the lock"
          )
    <*> strOption
          (long "tmploc" <> value "$TMPDIR" <> showDefault <> help
            "Location of the temporary directory to copy to"
          )
    <*> strOption
          (long "cpcmd" <> value "cp -Lr" <> showDefault <> help
            "Copy command to use"
          )
    <*> strOption
          (long "lockfile" <> value "LOCK" <> showDefault <> help
            "Name of the lock file"
          )
    <*> strOption
          (long "root" <> value "" <> help
           ("Use this directory as the the root to copy back to. Default " ++
            "is to use the current working directory."))
    <*> switch
          (  long "ignore-errors"
          <> help "Do not check for missing files before rewriting"
          )
    <*> switch
          (  long "version" <> short 'v'
          <> help "Display version oand exit")
    <*> optional (argument str (metavar "TASK_TEMPLATE"))


modes = ["lock", "test", "nolock"]
modeHelpDoc =
  hang 2 $
  vsep $
  map
    paragraph
    [ "Running mode:"
    , "test - write commands without rewriting to TMPDIR."
    , "nolock - copy to the temporary directory."
    , "lock - include a flock on copy to the temporary directory."
    ]

choicesReader :: [String] -> ReadM String
choicesReader xs = eitherReader $ checkInput xs
    where
        checkInput xs input
            | input `elem` xs = Right input
            | otherwise = Left $ "Must choose one of " ++ intercalate ", " xs

rewriteableDoc :: Doc
rewriteableDoc = paragraph 
                ("Rewriteables use a formatting syntax of {INPUT:i} for input "
              ++ "file INPUT, {DIR:d} for output directory DIR (DIR is created) "
              ++ "and {OUTPUT:o} for output file OUTPUT. Additionally, intermediate  "
              ++ "files created in the temporary file space and not copied back are "
              ++ "denoted as {INTERMEDIATE:n}.")

tmprewriteDescDoc = vsep
  [ paragraph ("Automatically wrap a command with copies to and"
            ++ "from a temporary directory and rewrite the "
            ++ "command to use files in the temporary directory. ")
 <> rewriteableDoc 
 <> paragraph " For example,"
  , text ""
  , text  "tmprewrite \"cat {infile1:i} {infile2:i} > {outfile:o}\""
  , text ""
  , text ("set -e ; (flock -w 7200 200 ; hostname > LOCK-host ; "
        ++ "cp -Lr infile1 infile2 $TMPDIR) 200> LOCK ; "
        ++ "cat $TMPDIR/infile1 $TMPDIR/infile2 > $TMPDIR/outfile ; "
        ++ "cp -Lr $TMPDIR/outfile .")
  , text ""
  , paragraph ("If no template is supplied in the argument, commands are "
           ++ "read line-by-line from stdin.")
  ]

parserInfo :: ParserInfo TmpSettings
parserInfo =
  info
    (tmpOptions <**> helper)
    (fullDesc <>
     progDescDoc (Just tmprewriteDescDoc) <>
     header "tmprewrite - Rewrite command(s) in temporary space")

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnEmpty

parseTmpSettings :: IO TmpSettings
parseTmpSettings = do
    settings <- customExecParser parserPrefs parserInfo
    cwd <- getCurrentDirectory
    return $ settings { root = (root settings <|> cwd) }
