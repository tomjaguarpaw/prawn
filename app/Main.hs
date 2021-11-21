{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}


module Main where

import System.Process.Typed (ProcessConfig, readProcess, setStdin, nullStream, proc)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Data.ByteString (putStr)
import Data.ByteString.Lazy (toStrict)
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

-- We ignore the commit hash.  We can get that in other ways.
parseGitDescribe :: Text -> Either String (Maybe RefType, Text, Int)
parseGitDescribe stdout =
  case Prelude.reverse (split (== '-') stdout) of
      _rev:distanceT:refParts -> do
        distance <- case treadMaybe distanceT :: Maybe Int of
                      Nothing -> Left ("Couldn't read distance: " <> unpack distanceT)
                      Just d -> pure d

        let ref = intercalate (fromString "-") (reverse refParts)
            (mRefType, shortRef) = case refType ref of
              Nothing -> (Nothing, ref)
              Just (refType_, shortRef_) -> (Just refType_, shortRef_)

        pure (mRefType, shortRef, distance)

      _ -> Left "Expected three components separated by dashes"

before :: String -> ExceptT String IO (Maybe Before)
before gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", gitDir, "describe", "--all", "--long"])

  case exitCode of
    ExitSuccess -> case parseGitDescribe stdout of
      Left e -> throwE e
      Right (mRefType, shortRef, distance) ->
        let coloredShortRef = case mRefType of
              Nothing -> Plain shortRef
              Just refType_ -> colorRef refType_ (Plain shortRef)

        in (pure . Just) $ case distance of
          0 -> At coloredShortRef
          _ -> Before (coloredShortRef <> fromString "-" <> Plain (tshow distance))

    -- I don't know a way of distinguishing between the various error
    -- conditions
    ExitFailure _ -> pure Nothing

-- If we are on branch "master" then
--
-- $ git describe --all --contains
-- master
--
-- If we are one commit master "master" then
--
-- $ git describe --all --contains
-- master~1
--
-- 1. In the first case we don't get a "~".  That's OK.  It's not
-- possible for that to be in a branch name anyway.  In any case, we
-- will ignore it because "before" we will tell us we are on "master".
--
-- 2. In the second case we don't get "heads/" before the name.  Even
-- --long does not help.  Perhaps we should just assume we are a head
-- if we get nothing there.
parseGitDescribeContains :: Text -> Either String (Maybe RefType, Text, Int)
parseGitDescribeContains stdout = do
  (ref, distance) <- case Prelude.reverse (split (== '~') stdout) of
    [ref] -> pure (ref, 0)
    distanceT:refParts@(_:_) -> do
        distance <- case treadMaybe distanceT :: Maybe Int of
                      Nothing -> Left ("Couldn't read distance: " <> unpack distanceT)
                      Just d -> pure d

        pure (intercalate (fromString "~") (reverse refParts), distance)

    [] -> Left "Expected non-empty output"

  let (mRefType, shortRef) = case refType ref of
        Nothing -> (Nothing, ref)
        Just (refType_, shortRef_) -> (Just refType_, shortRef_)

  pure (mRefType, shortRef, distance)

after :: String -> ExceptT String IO (Maybe Colored)
after gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", gitDir, "describe", "--all", "--contains"])

  case exitCode of
    ExitSuccess -> case parseGitDescribeContains stdout of
      Left e -> throwE e
      Right (mRefType, shortRef, distance) ->
        let coloredShortRef = case mRefType of
              Nothing -> Plain shortRef
              Just refType_ -> colorRef refType_ (Plain shortRef)

        in pure (Just (Plain (tshow distance) <> fromString "-" <> coloredShortRef))

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

    let branch = Colored Cyan (Plain (fromString "HEAD"))

    lift $ putColoredVT100 $ case (beforeStr, afterStr) of
      (Nothing, Nothing) -> fromString ""
      (Just (At b), _) -> branch <> fromString "=" <> b
      (Just (Before b), Nothing) -> b <> fromString "-" <> branch
      (Nothing, Just a) -> branch <> fromString "-" <> a
      (Just (Before b), Just a) -> b <> fromString "-" <> branch <> fromString "-" <> a

  case r of
    Left l -> Prelude.putStrLn l
    Right r_ -> pure r_
