{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}


module Main where

import System.Process.Typed (ProcessConfig, readProcess, setStdin, nullStream, proc)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Data.ByteString (putStr)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.String (fromString)
import Data.Text (Text, strip, intercalate, stripPrefix, split, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Text.Read (readMaybe)

putTextUtf8 :: Text -> IO ()
putTextUtf8 = Data.ByteString.putStr . encodeUtf8

tshow :: Show a => a -> Text
tshow = fromString . show

treadMaybe :: Read a => Text -> Maybe a
treadMaybe = readMaybe . unpack

run :: ProcessConfig () () ()
    -> ExceptT String IO (ExitCode, Text)
run p = do
  (exitCode, eStdout, _stderr) <- readProcess (setStdin nullStream p)

  stdout <- case (fmap strip . decodeUtf8' . toStrict) eStdout of
    Left l -> throwE ("Couldn't decode output: " <> show l)
    Right stdout -> pure stdout

  pure (exitCode, stdout)

data Before = At Text | Before Text

before :: String -> ExceptT String IO (Maybe Before)
before gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", gitDir, "describe", "--all", "--long"])

  case exitCode of
    ExitSuccess -> case Prelude.reverse (split (== '-') stdout) of
      _rev:distanceT:refParts ->
        let ref = intercalate (fromString "-") refParts
            shortRef = fromMaybe ref (stripPrefix (fromString "remotes/") ref
                                      <|> stripPrefix (fromString "heads/") ref)

        in case treadMaybe distanceT :: Maybe Int of
          Nothing -> throwE ("Couldn't read distance: " <> unpack distanceT)
          Just 0 -> pure (Just (At shortRef))
          Just _ -> pure (Just (Before (shortRef <> fromString "-" <> distanceT)))
      _ -> pure Nothing
    -- I don't know a way of distinguishing between the various error
    -- conditions
    ExitFailure _ -> pure Nothing

-- Unfortunately:
--
-- $ git describe --all --contains --long
-- remotes/origin/master
after :: String -> ExceptT String IO (Maybe Text)
after gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", gitDir, "describe", "--all", "--contains"])

  case exitCode of
    ExitSuccess -> case Prelude.reverse (split (== '~') stdout) of
      distance:refParts ->
        let ref = intercalate (fromString "~") refParts
            shortRef = fromMaybe ref (stripPrefix (fromString "remotes/") ref
                                      <|> stripPrefix (fromString "heads/") ref)

        in pure (Just (distance <> fromString "-" <> shortRef))
      _ -> pure Nothing
    -- I don't know a way of distinguishing between the various error
    -- conditions
    ExitFailure _ -> pure Nothing

main :: IO ()
main = do
  r <- runExceptT $ do
    gitDir <- lift System.Environment.getArgs >>= \case
      [gitDir] -> pure gitDir
      _ -> throwE "Need exactly one argument"

    beforeStr <- before gitDir
    afterStr  <- after gitDir

    lift $ putTextUtf8 $ case (beforeStr, afterStr) of
      (Nothing, Nothing) -> fromString ""
      (Just (At b), _) -> fromString "ᛘ=" <> b
      (Just (Before b), Nothing) -> b <> fromString "-ᛘ"
      (Nothing, Just a) -> fromString "ᛘ-" <> a
      (Just (Before b), Just a) -> b <> fromString "-ᛘ-" <> a

  case r of
    Left l -> Prelude.putStrLn l
    Right r_ -> pure r_
