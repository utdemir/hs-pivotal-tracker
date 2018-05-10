{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Tracker
  ( story
  , updateStory
  , stories

  -- Re-exports
  , ServantError
  , HTTP.Manager
  , mkManager
  , TrackerAPI
  , runTrackerAPI
  ) where

--------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Proxy
import           Data.Text         (Text)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import           Servant.API
import           Servant.Client
--------------------------------------------------------------------------------
import           Web.Tracker.Story
--------------------------------------------------------------------------------

type API = "services" :> "v5" :> "stories"
             :> Capture ":storyId" StoryId
             :> Header "X-TrackerToken" Text
             :> Get '[JSON] Story
      :<|> "services" :> "v5" :> "stories"
             :> ReqBody '[JSON] UpdateStory
             :> Capture ":storyId" StoryId
             :> Header "X-TrackerToken" Text
             :> Put '[JSON] Story
      :<|> "services" :> "v5" :> "projects"
             :> Capture ":projectId" ProjectId
             :> "stories"
             :> QueryParam "with_state" StoryState
             :> Header "X-TrackerToken" Text
             :> Get '[JSON] [Story]

api :: Proxy API
api = Proxy

story' :<|> updateStory' :<|> stories' = client api --

trackerUrl = (BaseUrl Https "www.pivotaltracker.com" 443 "")

type TrackerAPI a = ReaderT (Text, HTTP.Manager) (ExceptT ServantError IO ) a

type TrackerClientM a = Maybe Text -> ClientM a

runTrackerAPI :: Text -> HTTP.Manager -> TrackerAPI a -> IO (Either ServantError a)
runTrackerAPI tok man tApi = runExceptT $ runReaderT tApi (tok,man)

toTrackerAPI :: TrackerClientM a -> TrackerAPI a
toTrackerAPI cli = ReaderT $ \(tok,man) -> ExceptT $ runClientM (cli $ Just tok) (ClientEnv man trackerUrl)

mkManager :: IO HTTP.Manager
mkManager = HTTP.newManager HTTP.tlsManagerSettings

story :: StoryId -> TrackerAPI Story
story = toTrackerAPI . story'

updateStory :: UpdateStory -> StoryId -> TrackerAPI Story
updateStory us sid = toTrackerAPI $ updateStory' us sid

stories :: ProjectId -> Maybe StoryState -> TrackerAPI [Story]
stories pid mbss = toTrackerAPI $ stories' pid mbss