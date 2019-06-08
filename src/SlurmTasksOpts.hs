module SlurmTasksOpts (
      SlurmScriptSettings(..)
    , SlurmScriptProlog(..)
    , parseSlurmTasksOpts
    , verifySlurmTasksOpts
) where

import Control.Monad (unless)
import Control.Monad.Trans
import Control.Monad.Writer.Strict
import Data.List (intercalate)
import Options.Applicative
import System.IO.Error
import System.Directory


data SlurmScriptProlog = SlurmScriptProlog
    { logdir :: FilePath
    , cpus :: Int
    , mem :: Int
    , partition :: String
    , nice :: Int
    , name :: String
    , features :: String
    , workdir :: Maybe FilePath
    , limit :: Maybe Int
    , license :: Maybe String
    } deriving Show

data SlurmScriptSettings = SlurmScriptSettings
    { prolog :: SlurmScriptProlog
    , groups :: Int
    , ulimit :: Bool
    , ignoreErrors :: Bool
    , file :: Maybe String
    }

slurmScriptParser :: Parser SlurmScriptProlog
slurmScriptParser =
    SlurmScriptProlog
        <$> strOption
                (long "logdir" <> short 'o' <> metavar "DIR" <> value "." <> showDefault
              <> help "Directory in which to place output and error files")
        <*> option auto
                (long "cpus" <> short 'c' <> metavar "N" <> value 1 <> showDefault
              <> help "How many CPUs to request")
        <*> option auto
                (long "mem" <> short 'm' <> metavar "GB" <> value 3 <> showDefault
              <> help "How much memory (in GB) to request")
        <*> strOption
                (long "partition" <> value "basic" <> showDefault
              <> help "Which partition to request")
        <*> option auto
                (long "nice" <> metavar "N" <> value 0 <> showDefault
              <> help "The \"nice\" value of the job (higher means lower priority)")
        <*> strOption
                (long "name" <> short 'n' <> value "job" <> showDefault
              <> help "The name of the job")
        <*> strOption
                (long "features" <> short 'f' <> value "array-1core" <> showDefault
                <> help ("The required features of the nodes you will be submitting to. "
                    ++ "These can be combined in ways such as array-8core&localmirror. "
                    ++ "See the slurm manual for more information."))
        <*> optional (strOption
                (long "workdir" <> metavar "DIR"
            <> help "Specify a working directory for the jobs on the remote node"))
        <*> optional (option auto
                (long "slots" <> short 's' <> metavar "N"
              <> help "Maximum number of nodes to run the job on."))
        <*> optional (strOption
                (long "license" <> short 'l'
              <> help "License to give the job, e.g. \"scratch-highio\"."))

optParser :: Parser SlurmScriptSettings
optParser = SlurmScriptSettings
        <$> slurmScriptParser
        <*> option auto (long "group-by" <> short 'g' <> value 1 <> showDefault)
        <*> flag True False (long "no-ulimit" <> short 'u')
        <*> switch (long "ignore-errors")
        <*> optional (argument auto (metavar "TASKFILE"))

parserInfo :: ParserInfo SlurmScriptSettings
parserInfo = info
    ( optParser <**> helper)
    ( header "Construct a slurm script out of a list of tasks."
    <> progDesc
        (  "Convert a list of tasks into a longer form script "
        ++ "using a case-switch construct and a SLURM header with "
        ++ "with reasonable defaults. Either supply a filename containing a "
        ++ "single task per line or stream tasks via stdin."
        )
    <> fullDesc
    )

parseSlurmTasksOpts :: IO SlurmScriptSettings
parseSlurmTasksOpts =  execParser parserInfo

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