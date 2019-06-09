module Main where

import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Text as T
import System.FilePath.Posix

import SlurmTasksOpts
import ScriptBuilder

buildScript :: SlurmScriptProlog -> [String] -> String
buildScript pl tasks = let
     buildTask task = scriptLines $
         [ quotedEchoStmt "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on ${HOSTNAME} at $(date)"
         , quotedEchoStmt "  <Command>"
         ] ++
         map hardQuotedEchoStmt (lines task) ++
         [ quotedEchoStmt "  </Command>"
         , scriptStmt task
         , ifTestStmt (scriptStmt "$? -ne 0") (redirOutErr (quotedEchoStmt "Your program exited with error $?"))
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
         , slurmPartition (partition h)
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
            then "ulimit -v " ++ show (mem (prolog opts) * 1127000) ++ "\n" ++ task
            else task
        groupedTasks = groupTasksBy (groups opts) tasks
    in map (prependUlimit opts) groupedTasks

main = do
    opts <- parseSlurmTasksOpts
    verifySlurmTasksOpts opts
    contents <- case file opts of
            Nothing -> getContents
            (Just fn) -> readFile fn
    guard $ T.length (T.strip (T.pack contents)) > 0
    let tasks = processTasks opts (lines contents)
    putStrLn $ buildScript (prolog opts) tasks

-- fromJust $ getParseResult $ execParserPure defaultPrefs ( info (slurmScriptParser  <**> helper) idm) []
