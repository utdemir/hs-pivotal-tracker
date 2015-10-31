{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Tracker
  ( module Web.Tracker.Types
  , story
  , updateStory
  , stories

  -- Re-exports
  , ServantError
  ) where

--------------------------------------------------------------------------------
import           Data.Proxy
import           Data.Text         (Text)
import           Servant.API
import           Servant.Client
--------------------------------------------------------------------------------
import           Web.Tracker.Types
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

api :: Proxy API
api = Proxy

story :<|> updateStory :<|> stories = client api (BaseUrl Https "www.pivotaltracker.com" 443)
