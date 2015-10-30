{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Servant.API
import           System.Environment
import           System.Exit
import           Text.Read                  (readMaybe)
--------------------------------------------------------------------------------
import           Web.Tracker
--------------------------------------------------------------------------------

main :: IO ()
main = do
  token <- fmap T.pack <$> lookupEnv "TRACKER_API_TOKEN"

  query <- getArgs >>= return . \case
    "unstart" :sid:[] -> doSetState token Unstarted  <$> readMaybe sid
    "start"   :sid:[] -> doSetState token Started    <$> readMaybe sid
    "finish"  :sid:[] -> doSetState token Finished   <$> readMaybe sid
    "deliver" :sid:[] -> doSetState token Delivered  <$> readMaybe sid
    "accept"  :sid:[] -> doSetState token Accepted   <$> readMaybe sid
    "reject"  :sid:[] -> doSetState token Rejected   <$> readMaybe sid
    _                 -> Nothing

  query <- maybe invalidSyntax return query

  runEitherT query >>= \case
    Left err -> print err >> exitWith (ExitFailure 1)
    Right () -> return ()

  return ()

doSetState :: Maybe Text -> StoryState -> StoryId -> EitherT ServantError IO ()
doSetState t s i = void $ updateStory t (SetStoryState s) i

doListStories :: Maybe Text -> ProjectId -> StoryState -> EitherT ServantError IO ()
doListStories t i s = stories t i >>= mapM_ printStory
  where printStory Story{sId=StoryId id, sName}
          = liftIO . putStrLn $ show id <> "\t" <> T.unpack sName

invalidSyntax :: IO a
invalidSyntax = getProgName >>= putStrLn . msg >> exitWith (ExitFailure 1)
  where msg n
          =  "Usage: " <> n <> " <unstart|start|finish|deliver|accept|reject> STORY_ID\n"
          <> "  Use TRACKER_API_TOKEN environment variable if needed"
