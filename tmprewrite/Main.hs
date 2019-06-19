module Main
where

import GHC.Base (returnIO, when)
import Data.Version (showVersion)
import System.Exit

import TmpRewrite
import TmpRewriteOpts
import Paths_slurm_utils (version)

main :: IO ()
main = do
  settings <- parseTmpSettings
  when (displayVersion settings) $ do
    putStrLn $ "tmprewrite v" ++ (showVersion version)
    exitSuccess
  ls <- case cmd settings of
    Nothing -> fmap lines getContents
    Just template -> returnIO [template]
  out <- rewrite settings ls
  putStr $ unlines out