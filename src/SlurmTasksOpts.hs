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
import Options.Applicative
import Options.Applicative.Common (evalParser)
import Options.Applicative.Help.Pretty
import System.IO.Error
import System.Directory

import SlurmPresets
import Utils

data SlurmScriptProlog = SlurmScriptProlog
    { logdir :: FilePath
    , cpus :: Int
    , mem :: Int
    , partition :: String
    , nice :: Int
    , features :: String
    , name :: Maybe String
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

mkSlurmScriptParser :: SlurmScriptProlog -> Parser SlurmScriptProlog
mkSlurmScriptParser (SlurmScriptProlog  logdirVal
                                        cpusVal
                                        memVal
                                        partitionVal
                                        niceVal
                                        featuresVal
                                        nameVal
                                        workdirVal
                                        limitVal
                                        dependencyVal
                                        licenseVal ) =
    SlurmScriptProlog
        <$> strOption
                (long "logdir" <> short 'o' <> metavar "DIR" <> value logdirVal <> showDefault
              <> help "Directory in which to place output and error files")
        <*> option auto
                (long "cpus" <> short 'c' <> metavar "N" <> value cpusVal <> showDefault
              <> help "How many CPUs to request")
        <*> option auto
                (long "mem" <> short 'm' <> metavar "GB" <> value memVal <> showDefault
              <> help "How much memory (in GB) to request")
        <*> strOption
                (long "partition" <> value partitionVal <> showDefault
              <> help "Which partition to request")
        <*> option auto
                (long "nice" <> metavar "N" <> value niceVal <> showDefault
              <> help "The \"nice\" value of the job (higher means lower priority)")
        <*> strOption
                (long "features" <> short 'f' <> value featuresVal <> showDefault
              <> help ("The required features of the nodes you will be submitting to. "
                    ++ "These can be combined in ways such as array-8core&localmirror. "
                    ++ "See the slurm manual for more information."))
        <*> optional (strOption
                (long "name" <> short 'n' <> help "The name of the job"))
        <*> optional (strOption
                (long "workdir" <> metavar "DIR"
            <> help "Specify a working directory for the jobs on the remote node"))
        <*> optional (option auto
                (long "slots" <> short 's' <> metavar "N"
              <> help "Maximum number of nodes to run the job on."))
        <*> optional (strOption
                (long "dependency" <> short 'd' <> metavar "JOBID"
            <> help "Set a job dependency on JOBID"))
        <*> optional (strOption
                (long "license" <> short 'l'
              <> help "License to give the job, e.g. \"scratch-highio\"."))

defaultSlurmScriptProlog :: SlurmScriptProlog
defaultSlurmScriptProlog =
    SlurmScriptProlog { logdir="."
                    , cpus=1
                    , mem=3
                    , partition="basic"
                    , nice=0
                    , features="array-1core"
                    , name=Nothing
                    , workdir=Nothing
                    , limit=Nothing
                    , dependency=Nothing
                    , license=Nothing
                    }

optParser :: SlurmScriptProlog -> PresetInfo -> Parser SlurmScriptSettings
optParser prolog pi = SlurmScriptSettings
        <$> mkSlurmScriptParser prolog
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
    ( optParser prolog pi <**> helper)
    ( header "Construct a slurm script out of a list of tasks."
    <> progDesc
        (  "Convert a list of tasks into a longer form script "
        ++ "using a case-switch construct and a SLURM header with "
        ++ "with reasonable defaults. Either supply a filename containing a "
        ++ "single task per line or stream tasks via stdin."
        )
    <> fullDesc
    )

-- TODO: clean up access to the default settings
fetchPreset :: PresetInfo -> String -> IO SlurmScriptSettings
fetchPreset pi pn = do
    ps <- mapM (findPreset pi) $ splitOn "," pn
    settings <- foldM 
                (\sss p -> parsePresetArgs (parserInfo (prolog sss) pi) p)
                baseSettings
                ps
    execParser (parserInfo (prolog settings) pi)
    where
        baseSettings = fromJust (evalParser (optParser defaultSlurmScriptProlog pi))

parseSlurmTasksOpts :: IO SlurmScriptSettings
parseSlurmTasksOpts = do
    pi <- presetInfo
    settings <- execParser (parserInfo defaultSlurmScriptProlog pi)
    maybe (return settings) (fetchPreset pi) (preset settings)

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