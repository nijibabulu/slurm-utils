module Utils
(   paragraph
  , showDoc
  , warning
) where

import Control.Monad.IO.Class
import Options.Applicative.Help.Pretty
import System.IO

paragraph :: String -> Doc
paragraph = fillSep . map text . words

showDoc :: Doc -> String
showDoc = (`displayS` "") . renderPretty 1.0 80

warning :: (MonadIO m) => String -> m ()
warning = liftIO . hPutStrLn stderr . ("Warning: " ++)