module Main where

import Control.Monad
import Control.Applicative (optional)
import Data.List
import qualified Data.Text as T
import Options.Applicative hiding (header)
import System.FilePath.Posix

import ScriptBuilder

data SlurmScriptHeader = SlurmScriptHeader
    { logdir :: FilePath
    , cpus :: Int
    , workdir :: FilePath
    , mem :: Int
    , partition :: String
    , nice :: Int
    , name :: String
    , features :: String
    } deriving Show

data SlurmScriptSettings = SlurmScriptSettings
    { header :: SlurmScriptHeader
    , groups :: Int
    , ulimit :: Bool
    , file :: Maybe String
    }

slurmScriptParser :: Parser SlurmScriptHeader
slurmScriptParser =
    SlurmScriptHeader
        <$> strOption
                (long "logdir" <> value "." <> showDefault
              <> help "Directory in which to place output and error files")
        <*> option auto
                (long "cpus" <> short 'c' <> value 1 <> showDefault
              <> help "How many CPUs to request")
        <*> strOption
                (long "workdir" <> value "." <> showDefault
              <> help "The working directory for the jobs on the remote node")
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

optParser :: Parser SlurmScriptSettings
optParser = SlurmScriptSettings
        <$> slurmScriptParser
        <*> option auto (long "group-by" <> short 'g' <> value 1 <> showDefault)
        <*> flag True False (long "no-ulimit" <> short 'u')
        <*> optional (argument auto (metavar "TASKFILE"))

buildScript :: SlurmScriptHeader -> [String] -> String
buildScript h tasks = let
    buildTask task = scriptLines
        [ quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on ${HOSTNAME} at $(date)"
        , quotedEchoStmt "  <Command>"
        , hardQuotedEchoStmt (concatMap (\x -> if x == '\'' then "'\"'\"'\"'" else [x]) task)
        , quotedEchoStmt "  </Command>"
        , scriptStmt task
        , ifTestStmt (scriptStmt "$? -ne 0") (quotedEchoStmt "Your program exited with error $?")
        , quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} finished on ${HOSTNAME} at $(date)"
        ]
    buildHeader h tasks = scriptLines
        [ shebangBash
        , comment ""
        , slurmOutput (logdir h </> name h ++ ".o%A.%a")
        , slurmError (logdir h </> name h ++ ".e%A.%a")
        , slurmCpu (show (cpus h))
        , slurmWd (workdir h)
        , slurmArray ("1-" ++ show (length tasks))
        , slurmMem (show $ mem h * 1000)
        , slurmNice (show (nice h))
        , slurmName (name h)
        , slurmConstraint (features h)
        , scriptStmt ""
        ]
    in show $ scriptLines
        [ buildHeader h tasks
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
            then "ulimit -v " ++ show (mem (header opts) * 1127) ++ "\n" ++ task
            else task
        groupedTasks = groupTasksBy (groups opts) tasks
    in map (prependUlimit opts) groupedTasks

main = do
    opts <- execParser $ info (optParser <**> helper) idm
    contents <- case file opts of
            Nothing -> getContents
            (Just fn) -> readFile fn
    guard $ T.length (T.strip (T.pack contents)) > 0
    let tasks = processTasks opts (lines contents)
    putStrLn $ buildScript (header opts) tasks

-- fromJust $ getParseResult $ execParserPure defaultPrefs ( info (slurmScriptParser  <**> helper) idm) []
