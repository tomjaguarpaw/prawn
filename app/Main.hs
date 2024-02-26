{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Bluefin.Eff (Eff, runEff, runPureEff, (:&), (:>))
import Bluefin.Exception (Exception, throw, try)
import Bluefin.IO (IOE, effIO)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.ByteString (putStr)
import Data.ByteString.Lazy (toStrict)
import Data.String (IsString, fromString)
import Data.Text (Text, intercalate, split, strip, stripPrefix, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (ProcessConfig, nullStream, proc, readProcess, setStdin)
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

tshow :: (Show a) => a -> Text
tshow = fromString . show

treadMaybe :: (Read a) => Text -> Maybe a
treadMaybe = readMaybe . unpack

runExceptTIO ::
  (forall es e1 e2. IOE e1 -> Exception e e2 -> Eff (e2 :& e1 :& es) r) ->
  ExceptT e IO r
runExceptTIO f = ExceptT (runEff $ \io -> try (f io))

runIOEff ::
  (e :> es, ex :> es) =>
  IOE e ->
  Exception String ex ->
  ProcessConfig () () () ->
  Eff es (ExitCode, Text)
runIOEff io ex p = do
  (exitCode, eStdout, _stderr) <- effIO io (readProcess (setStdin nullStream p))

  stdout <- case (fmap strip . decodeUtf8' . toStrict) eStdout of
    Left l -> throw ex ("Couldn't decode output: " <> show l)
    Right stdout -> pure stdout

  pure (exitCode, stdout)

run ::
  ProcessConfig () () () ->
  ExceptT String IO (ExitCode, Text)
run p = runExceptTIO $ \io ex -> runIOEff io ex p

data Before = At Colored | Before Colored

data Color = Green | Red | Yellow | Cyan

data Colored
  = Plain Text
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
  where
    match rt s = fmap ((,) rt) (stripPrefix (fromString s) ref)

colorRef :: RefType -> (Colored -> Colored)
colorRef = Colored . \case Head -> Green; Remote -> Red; Tag -> Yellow

newtype GitDir = UnsafeGitDir {unGitDir :: FilePath}

getGitDir :: FilePath -> ExceptT String IO (Maybe GitDir)
getGitDir filepath = do
  (exitCode, stdout) <- run (proc "git" ["-C", filepath, "rev-parse", "--absolute-git-dir"])

  pure $ case exitCode of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just (UnsafeGitDir (unpack stdout))

-- Test case:
--
-- git describe --all --long
-- tags/tag-f72b0d2-0-gf72b0d2

-- We ignore the commit hash.  We can get that in other ways.
parseGitDescribe :: Text -> Either String (Maybe RefType, Text, Int)
parseGitDescribe stdout =
  case Prelude.reverse (split (== '-') stdout) of
    _rev : distanceT : refParts -> do
      distance <- case treadMaybe distanceT :: Maybe Int of
        Nothing -> Left ("Couldn't read distance: " <> unpack distanceT)
        Just d -> pure d

      let ref = intercalate (fromString "-") (reverse refParts)
          (mRefType, shortRef) = case refType ref of
            Nothing -> (Nothing, ref)
            Just (refType_, shortRef_) -> (Just refType_, shortRef_)

      pure (mRefType, shortRef, distance)
    _ -> Left "Expected three components separated by dashes"

before :: GitDir -> ExceptT String IO (Maybe Before)
before gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", unGitDir gitDir, "describe", "--all", "--long"])

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

-- Test cases:
--

-- $ git describe --all --contains
-- master

-- $ git describe --all --contains
-- master~1

parseGitDescribeContains ::
  (e :> es) =>
  Exception String e ->
  Text ->
  Eff es (Maybe RefType, Text, Int)
parseGitDescribeContains ex stdout = do
  (ref, distance) <- case Prelude.reverse (split (== '~') stdout) of
    [ref] -> pure (ref, 0)
    distanceT : refParts@(_ : _) -> do
      distance <- case treadMaybe distanceT :: Maybe Int of
        Nothing -> throw ex ("Couldn't read distance: " <> unpack distanceT)
        Just d -> pure d

      pure (intercalate (fromString "~") (reverse refParts), distance)
    [] -> throw ex "Expected non-empty output"

  let (mRefType, shortRef) = case refType ref of
        Nothing -> (Nothing, ref)
        Just (refType_, shortRef_) -> (Just refType_, shortRef_)

  pure (mRefType, shortRef, distance)

after :: GitDir -> ExceptT String IO (Maybe Colored)
after gitDir = runExceptTIO $ \io ex -> do
  (exitCode, stdout) <- runIOEff io ex (proc "git" ["-C", unGitDir gitDir, "describe", "--all", "--contains"])

  case exitCode of
    ExitSuccess -> case runPureEff $ try $ \ex -> parseGitDescribeContains ex stdout of
      Left e -> throw ex e
      Right (mRefType, shortRef, distance) ->
        let coloredShortRef = case mRefType of
              -- "git describe --all --contains" does not prefix heads
              -- with "head/".  Therefore if parsing returns an
              -- unknown ref type we assume it was a head and color it
              -- accordingly.
              Nothing -> colorRef Head (Plain shortRef)
              Just refType_ -> colorRef refType_ (Plain shortRef)
         in pure (Just (Plain (tshow distance) <> fromString "-" <> coloredShortRef))
    -- I don't know a way of distinguishing between the various error
    -- conditions
    ExitFailure _ -> pure Nothing

checkedOutBranch :: GitDir -> ExceptT String IO (Maybe Colored)
checkedOutBranch gitDir = do
  (exitCode, stdout) <- run (proc "git" ["-C", unGitDir gitDir, "symbolic-ref", "HEAD"])

  case exitCode of
    ExitSuccess -> case stripPrefix (fromString "refs/heads/") stdout of
      Nothing -> throwE ("Expected to start with refs/heads but got: " <> unpack stdout)
      Just branchName -> pure (Just (Colored Green (Plain branchName)))
    ExitFailure _ -> pure Nothing

main :: IO ()
main = do
  r <- runExceptT $ do
    lift System.Environment.getArgs >>= \case
      [] -> throwE "Need exactly one argument"
      (_ : _ : _) -> throwE "Need exactly one argument"
      [path] ->
        getGitDir path >>= \case
          Nothing -> pure ()
          Just gitDir -> do
            let branch = Colored Cyan (Plain (fromString "HEAD"))

            toDisplay <-
              checkedOutBranch gitDir >>= \case
                Just branchName -> pure $ branch <> fromString "=" <> branchName
                Nothing -> do
                  beforeStr <- before gitDir
                  afterStr <- after gitDir

                  pure $ case (beforeStr, afterStr) of
                    (Nothing, Nothing) -> fromString ""
                    (Just (At b), _) -> branch <> fromString "@" <> b
                    (Just (Before b), Nothing) -> b <> fromString "-" <> branch
                    (Nothing, Just a) -> branch <> fromString "-" <> a
                    (Just (Before b), Just a) -> b <> fromString "-" <> branch <> fromString "-" <> a

            lift (putColoredVT100 toDisplay)

  case r of
    Left l -> Prelude.putStrLn l
    Right r_ -> pure r_
