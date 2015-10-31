{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.List
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
    "unstart"        :sid:[] -> doSetState token Unstarted <$> readMaybe sid
    "start"          :sid:[] -> doSetState token Started   <$> readMaybe sid
    "finish"         :sid:[] -> doSetState token Finished  <$> readMaybe sid
    "deliver"        :sid:[] -> doSetState token Delivered <$> readMaybe sid
    "accept"         :sid:[] -> doSetState token Accepted  <$> readMaybe sid
    "reject"         :sid:[] -> doSetState token Rejected  <$> readMaybe sid
    "stateOf"        :sid:[] -> doStateOf  token           <$> readMaybe sid
    "list-unstarted" :pid:[] -> doList     token Unstarted <$> readMaybe pid
    "list-started"   :pid:[] -> doList     token Started   <$> readMaybe pid
    "list-finished"  :pid:[] -> doList     token Finished  <$> readMaybe pid
    "list-delivered" :pid:[] -> doList     token Delivered <$> readMaybe pid
    _                        -> Nothing

  query <- maybe invalidSyntax return query

  runEitherT query >>= \case
    Left err -> print err >> exitWith (ExitFailure 1)
    Right () -> return ()

  return ()

doSetState :: Maybe Text -> StoryState -> StoryId -> EitherT ServantError IO ()
doSetState t s i = void $ updateStory t (SetStoryState s) i

printStory :: MonadIO m => Story ->  m ()
printStory Story{sId=StoryId id, sName, sStoryType, sCurrentState}
  = liftIO . putStrLn $ show id
                     <> "\t" <> show sStoryType
                     <> "\t" <> show sCurrentState
                     <> "\t" <> T.unpack sName

doList :: Maybe Text -> StoryState -> ProjectId -> EitherT ServantError IO ()
doList t s i = stories t i (Just s) >>= mapM_ printStory . take 30

doStateOf :: Maybe Text -> StoryId -> EitherT ServantError IO ()
doStateOf t i = story t i >>= liftIO . print . sCurrentState

invalidSyntax :: IO a
invalidSyntax = getProgName >>= putStrLn . msg >> exitWith (ExitFailure 1)
  where msg n
          =  "Usage: " <> n <> " <unstart|start|finish|deliver|accept|reject> STORY_ID\n"
          <> "  Use TRACKER_API_TOKEN environment variable if needed"
