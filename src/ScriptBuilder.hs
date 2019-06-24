module ScriptBuilder  where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as P ((<$>))

bump :: Doc -> Doc
bump = indent 4

scriptLines :: [Doc] -> Doc
scriptLines = vsep

scriptStmt :: String -> Doc
scriptStmt = text

comment, shebang, slurmDirective, echoStmt, quotedEchoStmt, hardQuotedEchoStmt :: String -> Doc
comment = text . ("#"++)
shebang = comment . ("!"++)
slurmDirective = comment . ("SBATCH "++)
echoStmt_ d = text "echo " <+> d
echoStmt arg = echoStmt_ $ text arg
quotedEchoStmt arg = echoStmt_ $ dquotes $ text arg
hardQuotedEchoStmt arg = echoStmt_ $ squotes $ text $ concatMap (\x -> if x == '\'' then "'\"'\"'\"'" else [x]) arg

redir :: String -> Doc -> Doc
redir s d = d <+> text s
redirOutErr = redir "1>&2"
redirErrOut = redir "2>&1"

ifStmt, ifTestStmt :: Doc -> Doc -> Doc
ifStmt cond body = text "if " <+> cond <+> text "; then" P.<$> bump  body P.<$> text "fi"
ifTestStmt cond  = ifStmt (text "[ " <+> cond <+> text " ]")

caseStmt :: String -> String -> Doc
caseStmt pat cmd =
    text (pat ++ ")") P.<$> bump (vsep $ map text $ lines cmd) P.<$> text ";;"

caseBlock :: String -> [String] -> [String] -> Doc
caseBlock v pats cmds =
        text ("case " ++ v ++ " in")
  P.<$> bump (vsep (zipWith caseStmt pats cmds))
  P.<$> text "esac"

shebangBash :: Doc
shebangBash = shebang "/bin/bash"

slurmOpt :: String -> String -> Doc
slurmOpt o v = slurmDirective $ "--" ++ o ++ "=" ++ v

slurmMem, slurmCpu, slurmChdir, slurmArray, slurmPartition, slurmNice, slurmName, slurmOutput, slurmError, slurmConstraint, slurmNodes, slurmLicense :: String -> Doc
slurmMem = slurmOpt "mem"
slurmCpu = slurmOpt "cpus-per-task"
slurmChdir = slurmOpt "chdir"
slurmArray = slurmOpt "array"
slurmPartition = slurmOpt "partition"
slurmNice = slurmOpt "nice"
slurmName = slurmOpt "job-name"
slurmOutput = slurmOpt "output"
slurmError = slurmOpt "error"
slurmConstraint = slurmOpt "constraint"
slurmNodes = slurmOpt "nodes"
slurmLicense = slurmOpt "license"

-- slurmArrayCase cmds = caseBlock "${SLURM_ARRAY_TASK_ID}" (map show [0..]) cmds
