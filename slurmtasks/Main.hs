module Main where

import Control.Monad
import Control.Applicative (optional)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Options.Applicative
import System.FilePath.Posix

import ScriptBuilder

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
    , file :: Maybe String
    }

slurmScriptParser :: Parser SlurmScriptProlog
slurmScriptParser =
    SlurmScriptProlog
        <$> strOption
                (long "logdir" <> value "." <> showDefault
              <> help "Directory in which to place output and error files")
        <*> option auto
                (long "cpus" <> short 'c' <> value 1 <> showDefault
              <> help "How many CPUs to request")
        <*> option auto
                (long "mem" <> short 'm' <> value 3 <> showDefault
              <> help "How much memory (in GB) to request")
        <*> strOption
                (long "partition" <> value "basic" <> showDefault
              <> help "Which partition to request")
        <*> option auto
                (long "nice" <> value 0 <> showDefault
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
                (long "workdir"
            <> help "Specify a working directory for the jobs on the remote node"))
        <*> optional (option auto
                (long "slots" <> short 's'
              <> help "Maximum number of nodes to run the job on"))
        <*> optional (strOption
                (long "license" <> short 'l'
              <> help "License to give the job, e.g. scratch-highio."))

optParser :: Parser SlurmScriptSettings
optParser = SlurmScriptSettings
        <$> slurmScriptParser
        <*> option auto (long "group-by" <> short 'g' <> value 1 <> showDefault)
        <*> flag True False (long "no-ulimit" <> short 'u')
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

buildScript :: SlurmScriptProlog -> [String] -> String
buildScript pl tasks = let
     buildTask task = scriptLines
         [ quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on ${HOSTNAME} at $(date)"
         , quotedEchoStmt "  <Command>"
         , hardQuotedEchoStmt (concatMap (\x -> if x == '\'' then "'\"'\"'\"'" else [x]) task)
         , quotedEchoStmt "  </Command>"
         , scriptStmt task
         , ifTestStmt (scriptStmt "$? -ne 0") (quotedEchoStmt "Your program exited with error $?")
         , quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} finished on ${HOSTNAME} at $(date)"
         ]
     buildHeader h tasks = scriptLines $
         [ shebangBash
         , comment ""
         , slurmOutput (logdir h </> name h ++ ".o%A.%a")
         , slurmError (logdir h </> name h ++ ".e%A.%a")
         , (slurmCpu . show) (cpus h)
         , (slurmArray . ("1-"++) . show) (length tasks)
         , (slurmMem . show . (1000*)) (mem h)
         , (slurmNice . show) (nice h)
         , slurmName (name h)
         , slurmConstraint (features h)
         ] ++
         catMaybes [
             slurmNodes . ("1-"++) . show <$> limit h
           , slurmChdir <$> workdir h
           , slurmLicense <$> license h
         ]
     in show $ scriptLines
         [ buildHeader pl tasks
         , scriptStmt ""
         , caseBlock "${SLURM_ARRAY_TASK_ID}" (map show [1..]) (map (show . buildTask) tasks)
         ]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
    where (as,bs) = splitAt n xs

groupTasksBy :: Int -> [String] -> [String]
groupTasksBy n tasks = map (intercalate "\n") (splitEvery n tasks)

processTasks :: SlurmScriptSettings -> [String] -> [String]
processTasks opts tasks =
    let prependUlimit opts task = if ulimit opts
            then "ulimit -v " ++ show (mem (prolog opts) * 1127) ++ "\n" ++ task
            else task
        groupedTasks = groupTasksBy (groups opts) tasks
    in map (prependUlimit opts) groupedTasks

-- TODO: check whether it is possible to write to the output directory

main = do
    opts <- execParser parserInfo
    contents <- case file opts of
            Nothing -> getContents
            (Just fn) -> readFile fn
    guard $ T.length (T.strip (T.pack contents)) > 0
    let tasks = processTasks opts (lines contents)
    putStrLn $ buildScript (prolog opts) tasks

-- fromJust $ getParseResult $ execParserPure defaultPrefs ( info (slurmScriptParser  <**> helper) idm) []
