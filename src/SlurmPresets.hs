{-# LANGUAGE ExistentialQuantification #-}

module SlurmPresets
( Preset(..)
, PresetInfo(..)
, PresetError(..)
, availablePresetsDoc
, catchPresetError
, presetInfo
, parsePresetArgs
, findPreset
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Char (isSpace, isAlphaNum)
import Data.List (find)
import Data.Maybe
import Data.Typeable (Typeable, cast)
import Options.Applicative
import Options.Applicative.Help.Pretty hiding (string)
import Options.Applicative.Help.Types (renderHelp)
import Options.Applicative.Help.Core (parserHelp)
import Text.Regex.Applicative
import System.Directory

import Utils

data Preset = Preset
    { presetName :: String
    , args :: [String]
    }

instance Show Preset where
    show p = presetName p ++ ": " ++ unwords (args p)

data PresetInfo = PresetInfo
    { presets :: [Preset]
    , userFilePath :: FilePath
    }

data SomePresetError = forall e . Exception e => SomePresetError e

instance Show SomePresetError where
    show (SomePresetError e) = show e

instance Exception SomePresetError

data PresetError =
    PresetError
  | PresetsUnparseableError FilePath [String]
  | PresetUserFileNotFoundError FilePath
  | PresetNotFoundError PresetInfo String
  | PresetInvalidOptions String String
    deriving (Typeable)

instance Show PresetError where
    show (PresetsUnparseableError fn ps) = "Could not parse presets in " ++ fn ++ ":\n" ++ unlines ps
    show (PresetUserFileNotFoundError fn) = "Presets file not found: " ++ fn
    show (PresetNotFoundError pi p) = "No such preset: " ++ p ++ "\n\n" ++ showDoc (availablePresetsDoc pi)
    show (PresetInvalidOptions p h) = "Could not parse options in preset: \"" ++ p ++ "\"\n\n" ++ h
    show e = show e

fromPresetError :: Exception e => SomeException -> Maybe e
fromPresetError e = fromException e >>= (\(SomePresetError a) -> cast a)

instance Exception PresetError where
    toException = toException . SomePresetError
    fromException = fromPresetError

catchPresetError :: (MonadCatch m) => m a -> (SomePresetError -> m a) -> m a
catchPresetError = catchJust fromPresetError

builtinPresets :: [String]
builtinPresets = ["8core: --mem 64 --cpus 8 --features array-8core",
                  "himem: --mem 1000 --cpus 40 --features ''"]

availablePresetsDoc :: PresetInfo -> Doc
availablePresetsDoc pi =  text "Available presets:" <> linebreak <>
    indent 2 (vsep $ map ((fillSep . map text . words) . show) (presets pi))

presetParser :: RE Char Preset
presetParser = Preset <$> presetName <* string ":" <*> presetArgs
    where
        presetName = many $ psym isAlphaNum
        presetArg = many $ psym (not . isSpace)
        presetArgs = catMaybes <$> many ((Just <$> presetArg) <|> Nothing <$ psym isSpace)

parsePreset :: String -> Maybe Preset
parsePreset = match presetParser

parsePresetWithLog :: String ->  Writer [String] (Maybe Preset)
parsePresetWithLog l =
     case parsePreset l of
        Just m -> return $ Just m
        Nothing -> tell [l] >> return Nothing

getUserPresetPath :: IO FilePath
getUserPresetPath = getXdgDirectory XdgConfig "slurm-utils/slurmtasks.yml"

-- TODO: split this up to make errors more reasonable to handle
presetInfo :: (MonadIO m, MonadThrow m) => m PresetInfo
presetInfo = do
    path <- liftIO getUserPresetPath
    pathExists <- (liftIO . doesPathExist) path
    users <- if pathExists then (liftIO . parsePresetFile) path else throw $ PresetUserFileNotFoundError path
    let builtins = map (fromJust . match presetParser) builtinPresets
    return $ PresetInfo {presets = users ++ builtins, userFilePath=path}
    where
        parsePresetFile fn = do
            ls <- lines <$> (liftIO . readFile) fn
            let (ps,e) = runWriter $ mapM parsePresetWithLog ls
            unless (null e) $ warning $ show (PresetsUnparseableError fn e)
            return $ catMaybes ps


parsePresetArgs :: (MonadThrow m) => Preset -> ParserInfo a -> m a
parsePresetArgs p parserInfo =
    case execParserPure defaultPrefs parserInfo (args p) of
        Success res -> return res
        Failure f -> throw $ PresetInvalidOptions (show p) help
    where
        help = renderHelp 80 (parserHelp defaultPrefs (infoParser parserInfo))

findPreset :: (MonadThrow m) => PresetInfo -> String -> m Preset
findPreset pi n = maybe (throwM (PresetNotFoundError pi n)) return res
    where res = find ((== n) . presetName) (presets pi)
