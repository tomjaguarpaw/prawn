{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Bluefin.Compound (mapHandle, useImplIn)
import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.Exception (Exception, catch, throw)
import Bluefin.IO (IOE, effIO)
import Bluefin.Jump (Jump, jumpTo, withJump)
import Control.Applicative ((<|>))
import Data.ByteString (putStr)
import Data.ByteString.Lazy (toStrict)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString, fromString)
import Data.Text (Text, intercalate, split, strip, stripPrefix, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
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

run ::
  (e :> es, ex :> es) =>
  IOE e ->
  Exception String ex ->
  ProcessConfig () () () ->
  Eff es (ExitCode, Text)
run io ex p = do
  (exitCode, eStdout, _stderr) <- effIO io (readProcess (setStdin nullStream p))

  stdout <- case (fmap strip . decodeUtf8' . toStrict) eStdout of
    Left l -> throw ex ("Couldn't decode output: " <> show l)
    Right stdout -> pure stdout

  pure (exitCode, stdout)

data Git e where
  MkGit ::
    GitDir ->
    IOE e ->
    Exception String e ->
    Git e

runGit ::
  forall e es.
  (e :> es) =>
  Git e ->
  [String] ->
  Eff es (ExitCode, Text)
runGit (MkGit gitDir io ex) args =
  run io ex (proc "git" (["-C", unGitDir gitDir] ++ args))

data Before = At !Colored | Before !Colored

data Color = Green | Red | Yellow | Cyan

data Colored
  = Plain !Text
  | Colored !Color !Colored
  | Join !Colored !Colored

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

getGitDir ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Exception String e2 ->
  FilePath ->
  Eff es (Maybe GitDir)
getGitDir io ex filepath = do
  (exitCode, stdout) <- run io ex (proc "git" ["-C", filepath, "rev-parse", "--absolute-git-dir"])

  pure $ case exitCode of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just (UnsafeGitDir (unpack stdout))

-- Test case:
--
-- git describe --all --long
-- tags/tag-f72b0d2-0-gf72b0d2

-- We ignore the commit hash.  We can get that in other ways.
parseGitDescribe ::
  (e1 :> es) =>
  Exception String e1 ->
  Text ->
  Eff es (Maybe RefType, Text, Int)
parseGitDescribe ex stdout =
  case Prelude.reverse (split (== '-') stdout) of
    _rev : distanceT : refParts -> do
      distance <- case treadMaybe distanceT :: Maybe Int of
        Nothing -> throw ex ("Couldn't read distance: " <> unpack distanceT)
        Just d -> pure d

      let ref = intercalate (fromString "-") (reverse refParts)
          (mRefType, shortRef) = case refType ref of
            Nothing -> (Nothing, ref)
            Just (refType_, shortRef_) -> (Just refType_, shortRef_)

      pure (mRefType, shortRef, distance)
    _ -> throw ex "Expected three components separated by dashes"

before ::
  (e1 :> es, e3 :> es) =>
  Git e1 ->
  Exception String e3 ->
  Eff es (Maybe Before)
before git ex = do
  (exitCode, stdout) <- runGit git ["describe", "--all", "--long"]

  case exitCode of
    ExitSuccess -> do
      (mRefType, shortRef, distance) <- parseGitDescribe ex stdout
      let coloredShortRef = case mRefType of
            Nothing -> Plain shortRef
            Just refType_ -> colorRef refType_ (Plain shortRef)
      (pure . Just) $ case distance of
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

after ::
  (e1 :> es, e3 :> es) =>
  Git e1 ->
  Exception String e3 ->
  Eff es (Maybe Colored)
after git ex = do
  (exitCode, stdout) <- runGit git ["describe", "--all", "--contains"]

  case exitCode of
    ExitSuccess -> do
      (mRefType, shortRef, distance) <- parseGitDescribeContains ex stdout
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

headSymbol :: Colored
headSymbol = Colored Cyan (Plain (fromString "HEAD"))

checkedOutBranch ::
  (e1 :> es, e3 :> es) =>
  Git e1 ->
  Exception String e3 ->
  Eff es (Maybe Colored)
checkedOutBranch git ex = do
  (exitCode, stdout) <- runGit git ["symbolic-ref", "HEAD"]

  case exitCode of
    ExitSuccess -> case stripPrefix (fromString "refs/heads/") stdout of
      Nothing -> throw ex ("Expected to start with refs/heads but got: " <> unpack stdout)
      Just branchName -> pure (Just (Colored Green (Plain branchName)))
    ExitFailure _ -> pure Nothing

type Status = Either Colored (Maybe Before, Maybe Colored)

branchStatus ::
  (e1 :> es, e3 :> es) =>
  Git e1 ->
  Exception String e3 ->
  Eff es Status
branchStatus git ex =
  checkedOutBranch git ex >>= \case
    Just branchName -> pure (Left branchName)
    Nothing -> do
      beforeStr <- before git ex
      afterStr <- after git ex

      pure (Right (beforeStr, afterStr))

renderStatus :: Status -> Colored
renderStatus = \case
  Left branchName -> headSymbol <> fromString "=" <> branchName
  Right x -> case x of
    (Nothing, Nothing) -> fromString ""
    (Just (At b), _) -> headSymbol <> fromString "@" <> b
    (Just (Before b), Nothing) -> b <> fromString "-" <> headSymbol
    (Nothing, Just a) -> headSymbol <> fromString "-" <> a
    (Just (Before b), Just a) -> b <> fromString "-" <> headSymbol <> fromString "-" <> a

runEffOrExitFailure ::
  ( forall e es.
    IOE e ->
    Jump e ->
    Exception String e ->
    Eff (e :& es) ()
  ) ->
  IO r
runEffOrExitFailure f = runEff $ \io -> do
  Proxy :: Proxy (e :& es) <- effType

  r <-
    catch
      ( \ex -> do
          withJump $ \success ->
            useImplIn' @es (f (mapHandle io) (mapHandle success) (mapHandle ex))
          pure ExitSuccess
      )
      ( \l -> effIO io $ do
          Prelude.putStrLn l
          pure (ExitFailure 1)
      )
  effIO io (exitWith r)

-- This should probably be in Bluefin
effType :: Eff es (Proxy es)
effType = pure Proxy

-- This should probably be in Bluefin
useImplIn' :: (e :> es) => Eff (es :& e) r -> Eff es r
useImplIn' = ($ ()) . useImplIn . const

getArg :: (e :> es, ex :> es) => IOE e -> Exception String ex -> Eff es String
getArg io ex =
  effIO io System.Environment.getArgs >>= \case
    [] -> throw ex "Need exactly one argument"
    (_ : _ : _) -> throw ex "Need exactly one argument"
    [one] -> pure one

withGit ::
  forall j e e1 e2 r.
  (j :> e, e1 :> e, e2 :> e) =>
  FilePath ->
  Jump j ->
  IOE e1 ->
  Exception String e2 ->
  (forall eg. Git eg -> Eff (eg :& e) r) ->
  Eff e r
withGit path notGit io ex h = do
  gitDir <-
    getGitDir io ex path >>= \case
      Nothing -> jumpTo notGit
      Just gitDir -> pure gitDir

  useImplIn h (MkGit gitDir (mapHandle io) (mapHandle ex))

main :: IO ()
main = runEffOrExitFailure $ \io success ex -> do
  path <- getArg io ex
  status <- withGit path success io ex $ \git ->
    branchStatus git ex

  effIO io (putColoredVT100 (renderStatus status))
