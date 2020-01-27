module SlurmTasksOpts (
      SlurmScriptSettings(..)
    , SlurmScriptProlog(..)
    , parseSlurmTasksOpts
    , verifySlurmTasksOpts
) where

import Control.Monad ( unless, foldM )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Options.Applicative
import Options.Applicative.Common (evalParser)
import Options.Applicative.Help.Pretty
import System.IO.Error
import System.Directory

import SlurmPresets
import Utils

-- TODO: revert name to a maybe and use the following logic:
--          if Nothing and file is used, use the basename of the file for the name
--          if Nothing, set to "job" (or something)
--          if Just, always override.
data SlurmScriptProlog = SlurmScriptProlog
    { shell :: String 
    , logdir :: FilePath
    , cpus :: Int
    , mem :: Int
    , nice :: Int
    , name :: String
    , partition :: Maybe String
    , time :: Maybe String
    , features :: Maybe String
    , workdir :: Maybe FilePath
    , limit :: Maybe Int
    , dependency :: Maybe String
    , license :: Maybe String
    } deriving Show

data SlurmScriptSettings = SlurmScriptSettings
    { prolog :: SlurmScriptProlog
    , groups :: Int
    , ulimit :: Bool
    , ignoreErrors :: Bool
    , shortTasks :: Bool
    , preset :: Maybe String
    , displayVersion :: Bool
    , file :: Maybe String
    }

mkPrologParser :: SlurmScriptProlog -> Parser SlurmScriptProlog
mkPrologParser (SlurmScriptProlog shellVal
                                  logdirVal
                                  cpusVal
                                  memVal
                                  niceVal
                                  nameVal
                                  partitionVal
                                  timeVal
                                  featuresVal
                                  workdirVal
                                  limitVal
                                  dependencyVal
                                  licenseVal ) =
    SlurmScriptProlog
        <$> strOption
                (long "shell" <> metavar "PROG" <> value shellVal <> showDefault
              <> help "Shell string to execute")
        <*> strOption
                (long "logdir" <> short 'o' <> metavar "DIR" <> value logdirVal <> showDefault
              <> help "Directory in which to place output and error files")
        <*> option auto
                (long "cpus" <> short 'c' <> metavar "N" <> value cpusVal <> showDefault
              <> help "How many CPUs to request")
        <*> option auto
                (long "mem" <> short 'm' <> metavar "GB" <> value memVal <> showDefault
              <> help "How much memory (in GB) to request")
        <*> option auto
                (long "nice" <> metavar "N" <> value niceVal <> showDefault
              <> help "The \"nice\" value of the job (higher means lower priority)")
        <*> strOption (long "name" <> short 'n' <> value nameVal <> showDefault
                    <> help "The name of the job")
        <*> option (Just <$> str)
                (long "partition" <> value partitionVal <> help "Which partition to request")
        <*> option (Just <$> str)
                (long "time" <> metavar "T"  <> value timeVal
              <> help "How long the job is alloted by the slurmd to run")
        <*> option (Just <$> str)
                (long "features" <> short 'f'  <> value featuresVal
              <> help ("The required features of the nodes you will be submitting to. "
                    ++ "These can be combined in ways such as array-8core&localmirror. "
                    ++ "See the slurm manual for more information."))
        <*> option (Just <$> str)
                (long "workdir" <> metavar "DIR" <> value workdirVal
            <> help "Specify a working directory for the jobs on the remote node")
        <*> option (Just <$> auto)
                (long "slots" <> short 's' <> metavar "N" <> value limitVal
              <> help "Maximum number of nodes to run the job on.")
        <*> option (Just <$> str)
                (long "dependency" <> short 'd' <> metavar "JOBID" <> value dependencyVal
            <> help "Set a job dependency on JOBID")
        <*> option (Just <$> str)
                (long "license" <> short 'l' <> value licenseVal
              <> help "License to give the job, e.g. \"scratch-highio\".")

defaultSlurmScriptProlog :: SlurmScriptProlog
defaultSlurmScriptProlog =
    SlurmScriptProlog { shell="/bin/bash"
                      , logdir="."
                      , cpus=1
                      , mem=3
                      , nice=0
                      , name="job"
                      , partition=Nothing
                      , time=Nothing
                      , features=Nothing
                      , workdir=Nothing
                      , limit=Nothing
                      , dependency=Nothing
                      , license=Nothing
                      }

 
mkSettingsParser :: SlurmScriptProlog -> PresetInfo -> Parser SlurmScriptSettings
mkSettingsParser prolog pi = SlurmScriptSettings
        <$> mkPrologParser prolog
        <*> option auto (long "group-by" <> short 'g' <> value 1 <> showDefault)
        <*> flag True False (long "no-ulimit" <> short 'u')
        <*> switch (long "ignore-errors")
        <*> switch (long "short-tasks" <> help "make tasks brief (no echo output of the task command)")
        <*> optional (strOption (long "preset" <> short 'p' <> helpDoc (Just (presetDoc pi))))
        <*> switch (long "version" <> short 'v' <> help "Display version and exit")
        <*> optional (strArgument (metavar "TASKFILE"))

presetDoc :: PresetInfo -> Doc
presetDoc pi = paragraph presetText <> availablePresetsDoc pi
    where
        presetText = "Use preset groups of options, builtin or included in "
                  ++ userFilePath pi ++ "."

parserInfo :: SlurmScriptProlog -> PresetInfo -> ParserInfo SlurmScriptSettings
parserInfo prolog pi = info
    ( mkSettingsParser prolog pi <**> helper)
    ( header "Construct a slurm script out of a list of tasks."
    <> progDesc
        (  "Convert a list of tasks into a longer form script "
        ++ "using a case-switch construct and a SLURM header with "
        ++ "with reasonable defaults. Either supply a filename containing a "
        ++ "single task per line or stream tasks via stdin."
        )
    <> fullDesc
    )

-- defaultSettings :: SlurmScriptSettings
-- defaultSettings = fromJust $ evalParser $ mkSettingsParser defaultSlurmScriptProlog emptyPresetInfo

-- TODO: clean up access to the default settings
fetchPreset :: PresetInfo -> SlurmScriptSettings -> String -> IO SlurmScriptSettings
fetchPreset pi s pn = do
    ps <- mapM (findPreset pi) $ splitOn "," pn
    settings <- foldM 
                (\sss p -> parsePresetArgs (parserInfo (prolog sss) pi) p)
                s
                ps
    execParser (parserInfo (prolog settings) pi)

-- baseSettings :: IO SlurmScriptSettings
-- baseSettings = execParser (parserInfo defaultSlurmScriptProlog presetInfo)

-- defaultSettings :: IO SlurmScriptSettings
-- defaultSettings = parsePresetArgs (parserInfo (prolog baseSettings) presetInfo) (defaults presetInfo)

-- TODO: (parserInfo (prolog settings) pi)) is used twice--extracting to and from parser is somewhat annoying
parseSlurmTasksOpts :: IO SlurmScriptSettings
parseSlurmTasksOpts = do
    pi <- presetInfo
    settings <- execParser (parserInfo defaultSlurmScriptProlog pi)
    defaultSettings <- parsePresetArgs (parserInfo (prolog settings) pi) (defaults pi)
    maybe (return defaultSettings) (fetchPreset pi defaultSettings) (preset settings)

verifyDir :: String -> String -> WriterT String IO ()
verifyDir t d = do
    r <- (lift . tryIOError) $ writable <$> getPermissions d
    case r of
        Left e
            | isDoesNotExistError e -> tell $ unwords [t, "directory", d, "does not exist\n"]
            | otherwise -> (lift . ioError) e
        Right False -> tell $ unwords ["Insufficient permissions to write to", t, "directory", d, "\n"]
        Right True -> return ()

verifySlurmTasksOpts :: SlurmScriptSettings -> IO ()
verifySlurmTasksOpts SlurmScriptSettings{prolog=pl, ignoreErrors=skip} =
    unless skip $ do
        (_,e) <- runWriterT $ do
            verifyDir "log" (logdir pl)
            maybe ((lift . return) ()) (verifyDir "working") (workdir pl)
        case e of
            "" -> return ()
            _ -> errorWithoutStackTrace ("\n" ++ e ++ "\nUse --ignore-errors to suppress errors")
