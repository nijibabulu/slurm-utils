module Main where

import Data.List (intercalate)
import Data.Maybe
import Data.Version (showVersion)
import qualified Data.Text as T
import GHC.Base (when)
import System.Exit
import System.FilePath.Posix

import SlurmTasksOpts
import ScriptBuilder

import Paths_slurm_utils (version)

buildScript :: SlurmScriptSettings -> [String] -> String
buildScript SlurmScriptSettings{prolog=pl, shortTasks=short} tasks = let
     buildTask False task = scriptLines $
         [ quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on ${HOSTNAME} at $(date)"
         , quotedEchoStmt "  <Command>"
         ] ++
         map hardQuotedEchoStmt (lines task) ++
         [ quotedEchoStmt "  </Command>"
         , scriptStmt task
         , ifTestStmt (scriptStmt "$? -ne 0") (redirOutErr (quotedEchoStmt "Your program exited with error $?"))
         , quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} finished on ${HOSTNAME} at $(date)"
         ]
     buildTask True task = scriptStmt task
     buildHeader h tasks = scriptLines $
         [ shebang (shell h)
         , comment ""
         , slurmOutput (logdir h </> "%x.o%A.%a")
         , slurmError (logdir h </> "%x.e%A.%a")
         , (slurmCpu . show) (cpus h)
         , (slurmArray . ("1-"++) . show) (length tasks)
         , (slurmMem . show . (1000*)) (mem h)
         , (slurmNice . show) (nice h)
         , slurmName (name h)
         ] ++
         (map slurmDirective (extra h)) ++
         catMaybes [
             slurmConstraint <$> features h
           , slurmPartition <$> partition h
           , slurmTime <$> time h
           , slurmNodes . ("1-"++) . show <$> limit h
           , slurmChdir <$> workdir h
           , slurmDependency <$> dependency h
           , slurmLicense <$> license h
         ]
     in show $ scriptLines
         [ buildHeader pl tasks
         , scriptStmt ""
         , caseBlock "${SLURM_ARRAY_TASK_ID}" (map show [1..]) (map (show . buildTask short) tasks)
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
            then "ulimit -v " ++ show (mem (prolog opts) * 1127000) ++ "\n" ++ task
            else task
        groupedTasks = groupTasksBy (groups opts) tasks
    in map (prependUlimit opts) groupedTasks

main = do
    opts <- parseSlurmTasksOpts
    verifySlurmTasksOpts opts
    when (displayVersion opts) $ do
        putStrLn $ "slurmtasks v" ++ showVersion version
        exitSuccess
    contents <- case file opts of
            Nothing -> getContents
            (Just fn) -> readFile fn
    let emptyStr s = T.length (T.strip (T.pack s)) == 0
    when (emptyStr contents) $ errorWithoutStackTrace "Empty input"
    let tasks = processTasks opts (filter (not . emptyStr) (lines contents))
    putStrLn $ buildScript opts tasks

-- fromJust $ getParseResult $ execParserPure defaultPrefs ( info (slurmScriptParser  <**> helper) idm) []
