{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Tracker
  ( story
  , updateStory
  , stories
  , project

  -- Re-exports
  , ServantError
  ) where

--------------------------------------------------------------------------------
import           Data.Proxy
import           Data.Text         (Text)
import           Servant.API
import           Servant.Client
--------------------------------------------------------------------------------
import           Web.Tracker.Story
import           Web.Tracker.Project
--------------------------------------------------------------------------------

type API = "services" :> "v5" :> "stories"
             :> Header "X-TrackerToken" Text
             :> Capture ":storyId" StoryId
             :> Get '[JSON] Story
      :<|> "services" :> "v5" :> "stories"
             :> Header "X-TrackerToken" Text
             :> ReqBody '[JSON] UpdateStory
             :> Capture ":storyId" StoryId
             :> Put '[JSON] Story
      :<|> "services" :> "v5" :> "projects"
             :> Header "X-TrackerToken" Text
             :> Capture ":projectId" ProjectId
             :> "stories"
             :> QueryParam "with_state" StoryState
             :> Get '[JSON] [Story]
      :<|> "services" :> "v5" :> "projects"
             :> Header "X-TrackerToken" Text
             :> Capture ":projectId" ProjectId
             :> Get '[JSON] Project

api :: Proxy API
api = Proxy

story :<|> updateStory :<|> stories :<|> project = client api (BaseUrl Https "www.pivotaltracker.com" 443)
