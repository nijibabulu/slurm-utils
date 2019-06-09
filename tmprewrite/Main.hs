module Main
where

import GHC.Base (returnIO)

import TmpRewrite
import TmpRewriteOpts

main :: IO ()
main = do
  settings <- parseTmpSettings
  ls <- case cmd settings of
    Nothing -> fmap lines getContents
    Just template -> returnIO [template]
  out <- mapM (rewrite settings) ls
  putStrLn $ unlines out