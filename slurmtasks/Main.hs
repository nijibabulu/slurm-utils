module Main where

import Control.Monad
import Control.Applicative (optional)
import Data.List
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
    , limit :: Maybe Int
    } deriving Show

data SlurmScriptSettings = SlurmScriptSettings
    { header :: SlurmScriptHeader
    , groups :: Int
    , ulimit :: Bool
    , file :: Maybe String
    }

slurmScriptParser :: Parser SlurmScriptHeader
slurmScriptParser = SlurmScriptHeader
    <$> strOption (long "logdir" <> value "." <> showDefault)
    <*> option auto (long "cpus" <> short 'c' <> value 1 <> showDefault)
    <*> strOption (long "workdir" <> value "." <> showDefault)
    <*> option auto (long "mem" <> short 'm' <> value 3 <> showDefault)
    <*> strOption (long "partition" <> value "basic" <> showDefault)
    <*> option auto (long "nice" <> value 0 <> showDefault)
    <*> strOption (long "name" <> short 'n' <> value "job" <> showDefault)
    <*> optional (option auto (long "limit" <> short 'l'))

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
        , quotedEchoStmt (concatMap (\x -> if x == '\'' then "'\"'\"'\"'" else [x]) task)
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
        , slurmMem (show $ mem h)
        , slurmNice (show (nice h))
        , slurmName (name h)
        ] 
    in show $ scriptLines
        [ buildHeader h tasks
        , caseBlock "${SLURM_ARRAY_TASK_ID}" (map show [1..]) (map (show . buildTask) tasks)
        ]

splitBy :: Int -> [a] -> [[a]]
splitBy n xs = let
    splitBy' acc xs' n =  case splitAt n xs' of
        (h,[]) -> (acc ++ [h])
        (h,xs'') -> splitBy' (acc ++ [h]) xs'' n
    in splitBy' [] xs n

groupTasksBy :: Int -> [String] -> [String]
groupTasksBy n tasks = map (intercalate "\n") (splitBy n tasks)

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
    let tasks = processTasks opts (lines contents)
    putStrLn $ buildScript (header opts) tasks

-- fromJust $ getParseResult $ execParserPure defaultPrefs ( info (slurmScriptParser  <**> helper) idm) []
