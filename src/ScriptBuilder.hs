module ScriptBuilder  where

import Prelude hiding ((<$>))
import Data.List
import Text.PrettyPrint.ANSI.Leijen

bump :: Doc -> Doc
bump = indent 4

scriptLines :: [Doc] -> Doc
scriptLines = vsep

scriptStmt :: String -> Doc
scriptStmt = text

comment, shebang, slurmDirective, echoStmt, quotedEchoStmt :: String -> Doc
comment = text . ("#"++)
shebang = comment . ("!"++)
slurmDirective = comment . ("SBATCH "++)
echoStmt arg = text $ "echo " ++ arg
quotedEchoStmt arg = echoStmt ("\"" ++ arg ++ "\"")

ifStmt, ifTestStmt :: Doc -> Doc -> Doc
ifStmt cond body = text "if " <+> cond <+> text "; then" <$> (bump  body) <$> text "fi"
ifTestStmt cond body = ifStmt (text "[ " <+> cond <+> text " ]") body

caseStmt :: String -> String -> Doc
caseStmt pat cmd =
    text (pat ++ ")") <$> (bump $ vsep $ map text $ lines cmd) <$> text ";;"

caseBlock :: String -> [String] -> [String] -> Doc
caseBlock v pats cmds =
        text ("case " ++ v ++ " in")
    <$> bump (vsep (map (uncurry caseStmt) (zip pats cmds)))
    <$> text "esac"

shebangBash :: Doc
shebangBash = shebang "/bin/bash"

slurmOpt :: String -> String -> Doc
slurmOpt o v = slurmDirective $ "--" ++ o ++ "=" ++ v

slurmMem, slurmCpu, slurmWd, slurmArray, slurmPartition, slurmNice, slurmName, slurmOutput, slurmError :: String -> Doc
slurmMem = slurmOpt "mem"
slurmCpu = slurmOpt "cpus-per-task"
slurmWd = slurmOpt "workdir"
slurmArray = slurmOpt "array"
slurmPartition = slurmOpt "partition"
slurmNice = slurmOpt "nice"
slurmName = slurmOpt "job-name"
slurmOutput = slurmOpt "output"
slurmError = slurmOpt "error"

-- slurmArrayCase cmds = caseBlock "${SLURM_ARRAY_TASK_ID}" (map show [0..]) cmds