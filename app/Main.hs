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
import Data.String (fromString, IsString)
import Data.Text (Text, strip, intercalate, stripPrefix, split, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Text.Read (readMaybe)

putTextUtf8 :: Text -> IO ()
putTextUtf8 = Data.ByteString.putStr . encodeUtf8

reset = "\ESC[0m"

nfgRed = "\ESC[31m"
fgBrightRed = "\ESC[31;1m"
bgRed = "\ESC[41m"
bgBrightRed = "\ESC[41;1m"

fgGreen = "\ESC[32m"
fgBrightGreen = "\ESC[32;1m"
bgGreen = "\ESC[42m"
bgBrightGreen = "\ESC[42;1m"

fgYellow = "\ESC[33m"
fgBrightYellow = "\ESC[33;1m"
bgYellow = "\ESC[43m"
bgBrightYellow = "\ESC[43;1m"

fgBrightCyan = "\ESC[36;1m"

-- This doesn't handle nested colors.  Best to use one of
--
-- * https://hackage.haskell.org/package/rainbow
-- * https://hackage.haskell.org/package/ansi-terminal
-- * https://hackage.haskell.org/package/terminal
putColoredVT100 :: Colored -> IO ()
putColoredVT100 = \case
  Plain text -> putTextUtf8 text
  Colored color colored -> case color of
    Red -> Prelude.putStr fgBrightRed *> putColoredVT100 colored *> Prelude.putStr reset
    Green -> Prelude.putStr fgBrightGreen *> putColoredVT100 colored *> Prelude.putStr reset
    Yellow -> Prelude.putStr fgBrightYellow *> putColoredVT100 colored *> Prelude.putStr reset
    Cyan -> Prelude.putStr fgBrightCyan *> putColoredVT100 colored *> Prelude.putStr reset
  Join c1 c2 -> putColoredVT100 c1 *> putColoredVT100 c2

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

data Before = At Colored | Before Colored

data Color = Green | Red | Yellow | Cyan

data Colored = Plain Text
             | Colored Color Colored
             | Join Colored Colored

instance IsString Colored where
  fromString = Plain . fromString

instance Semigroup Colored where
  (<>) = Join

data RefType = Remote | Head | Tag

refType :: Text -> Maybe (RefType, Text)
refType ref =
      match Remote "remotes/"
  <|> match Head "heads/"
  <|> match Tag "tags/"
  where match rt s = fmap ((,) rt) (stripPrefix (fromString s) ref)

colorRef :: RefType -> (Colored -> Colored)
colorRef = Colored . \case { Head -> Green; Remote -> Red; Tag -> Yellow }
-- Test case:
--
-- git describe --all --long
-- tags/tag-f72b0d2-0-gf72b0d2
before :: String -> ExceptT String IO (Maybe Before)
before gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", gitDir, "describe", "--all", "--long"])

  case exitCode of
    ExitSuccess -> case Prelude.reverse (split (== '-') stdout) of
      _rev:distanceT:refParts ->
        let ref = intercalate (fromString "-") (reverse refParts)
            shortRef = case refType ref of
              Nothing -> Plain ref
              Just (refType_, shortRef_) -> colorRef refType_ (Plain shortRef_)

        in case treadMaybe distanceT :: Maybe Int of
          Nothing -> throwE ("Couldn't read distance: " <> unpack distanceT)
          Just 0 -> pure (Just (At shortRef))
          Just _ -> pure (Just (Before (shortRef <> fromString "-" <> Plain distanceT)))
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
        let ref = intercalate (fromString "~") (reverse refParts)
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

    let branch = Colored Cyan (Plain (fromString "ᛘ"))

    lift $ putColoredVT100 $ case (beforeStr, afterStr) of
      (Nothing, Nothing) -> fromString ""
      (Just (At b), _) -> branch <> fromString "=" <> b
      (Just (Before b), Nothing) -> b <> fromString "-" <> branch
      (Nothing, Just a) -> branch <> fromString "-" <> Plain a
      (Just (Before b), Just a) -> b <> fromString "-" <> branch <> fromString "-" <> Plain a

  case r of
    Left l -> Prelude.putStrLn l
    Right r_ -> pure r_
