{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<$>))
import           Control.Monad
import           Control.Monad.IO.Class
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
import           Web.Tracker.Story
--------------------------------------------------------------------------------

main :: IO ()
main = do
  token <- T.pack <$> lookupFail "TRACKER_API_TOKEN"
  manager <- mkManager

  query <- getArgs >>= return . \case
    "unstart"        :sid:[] -> doSetState Unstarted <$> readMaybe sid
    "start"          :sid:[] -> doSetState Started   <$> readMaybe sid
    "finish"         :sid:[] -> doSetState Finished  <$> readMaybe sid
    "deliver"        :sid:[] -> doSetState Delivered <$> readMaybe sid
    "accept"         :sid:[] -> doSetState Accepted  <$> readMaybe sid
    "reject"         :sid:[] -> doSetState Rejected  <$> readMaybe sid
    "stateOf"        :sid:[] -> doStateOf            <$> readMaybe sid
    "list-unstarted" :pid:[] -> doList     Unstarted <$> readMaybe pid
    "list-started"   :pid:[] -> doList     Started   <$> readMaybe pid
    "list-finished"  :pid:[] -> doList     Finished  <$> readMaybe pid
    "list-delivered" :pid:[] -> doList     Delivered <$> readMaybe pid
    _                        -> Nothing

  maybe invalidSyntax (runTrackerAPI token manager) query >>= \case
    Left err -> print err >> exitWith (ExitFailure 1)
    Right () -> return ()

lookupFail :: String -> IO String
lookupFail var = maybe (fail $ "Expected environment variable " ++ var) return =<< lookupEnv var

doSetState :: StoryState -> StoryId -> TrackerAPI ()
doSetState s i = void $ updateStory (SetStoryState s) i

printStory :: MonadIO m => Story ->  m ()
printStory Story{sId=StoryId id, sName, sStoryType, sCurrentState}
  = liftIO . putStrLn $ show id
                     <> "\t" <> show sStoryType
                     <> "\t" <> show sCurrentState
                     <> "\t" <> T.unpack sName

doList :: StoryState -> ProjectId -> TrackerAPI ()
doList s i = stories i (Just s) >>= mapM_ printStory . take 30

doStateOf :: StoryId -> TrackerAPI ()
doStateOf i = story i >>= liftIO . print . sCurrentState

invalidSyntax :: IO a
invalidSyntax = getProgName >>= putStrLn . msg >> exitWith (ExitFailure 1)
  where msg n
          =  "Usage: " <> n <> " <unstart|start|finish|deliver|accept|reject> STORY_ID\n"
          <> "  Use TRACKER_API_TOKEN environment variable if needed"
