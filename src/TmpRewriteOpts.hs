module TmpRewriteOpts (
      TmpSettings(..)
    , tmpOptions
    , rewriteableDoc
    , parserInfo
    , parserPrefs
    , parseTmpSettings
) where

import Data.List (intercalate)
import Options.Applicative 
import Options.Applicative.Help.Pretty

data TmpSettings = TmpSettings
  { mode :: String
  , wait :: Integer
  , tmploc :: String
  , cpcmd :: String
  , lockfile :: String
  , ignoreErrors :: Bool
  , cmd :: Maybe String
  } deriving (Show)

paragraph :: String -> Doc
paragraph = fillSep . map text . words

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
    <*> switch
          (  long "ignore-errors"
          <> help "Do not check for missing files before rewriting"
          )
    <*> optional (argument str (metavar "TASK_TEMPLATE"))


modes = ["lock", "test", "nolock"]
modeHelpDoc =
  hang 2 $
  vsep $
  map
    paragraph
    [ "Running mode:"
    , "test - create commands rewriting the filenames without copying."
    , "nolock - copy to the temporary directory."
    , "lock - include an flock on copy to the temporary directory."
    ]

choicesReader :: [String] -> ReadM String
choicesReader xs = eitherReader $ checkInput xs
    where
        checkInput xs input
            | input `elem` xs = Right input
            | otherwise = Left $ "Must choose one of " ++ intercalate ", " xs

rewriteableDoc :: String
rewriteableDoc = "Rewriteables use a formatting syntax of {INPUT:i} for input "
              ++  "file INPUT, {DIR:d} for output directory DIR (DIR is created) "
              ++  "and {OUTPUT:o} for output file OUTPUT."

tmprewriteDescDoc = vsep
  [ paragraph ("Automatically wrap a command with copies to and"
            ++ "from a temporary directory and rewrite the "
            ++ "command to use files in the temporary directory. "
            ++ rewriteableDoc
            ++ "For example,")
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
     header "tmp_rewrite - Rewrite command(s) in temporary space")

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnEmpty

parseTmpSettings :: IO TmpSettings
parseTmpSettings = customExecParser parserPrefs parserInfo