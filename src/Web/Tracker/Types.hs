{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Tracker.Types where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.String
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Time.Clock
import           GHC.Generics
import           Servant.API
import           Text.Read
--------------------------------------------------------------------------------
data StoryId
  = StoryId Integer
  deriving (Show, Eq, Ord)

instance FromJSON StoryId where
  parseJSON = fmap StoryId . parseJSON

instance Read StoryId where
  readPrec = fmap StoryId readPrec

instance ToJSON StoryId where
  toJSON (StoryId t) = toJSON t

instance ToText StoryId where
  toText (StoryId t) = toText t

data ProjectId
  = ProjectId Int
  deriving (Show, Eq, Ord)

instance FromJSON ProjectId where
  parseJSON = fmap ProjectId . parseJSON

instance Read ProjectId where
  readPrec = fmap ProjectId readPrec

instance ToText ProjectId where
  toText (ProjectId t) = toText t

data UserId
  = UserId Int
  deriving (Show, Eq, Ord)

instance FromJSON UserId where
  parseJSON = fmap UserId . parseJSON

data StoryState
  = Accepted | Delivered | Finished | Started
  | Rejected | Planned | Unstarted | Unscheduled
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromJSON StoryState where
  parseJSON "accepted"    = return Accepted
  parseJSON "delivered"   = return Delivered
  parseJSON "finished"    = return Finished
  parseJSON "started"     = return Started
  parseJSON "rejected"    = return Rejected
  parseJSON "planned"     = return Planned
  parseJSON "unstarted"   = return Unstarted
  parseJSON "unscheduled" = return Unscheduled

instance ToJSON StoryState where
  toJSON = toJSON . toText

instance ToText StoryState where
  toText Accepted    = "accepted"
  toText Delivered   = "delivered"
  toText Finished    = "finished"
  toText Started     = "started"
  toText Rejected    = "rejected"
  toText Planned     = "planned"
  toText Unstarted   = "unstarted"
  toText Unscheduled = "unscheduled"

data StoryType
  = Feature | Bug | Chore | Release
  deriving (Show, Eq, Ord)

instance FromJSON StoryType where
  parseJSON "feature" = return Feature
  parseJSON "bug"     = return Bug
  parseJSON "chore"   = return Chore
  parseJSON "release" = return Release

data UpdateStory
  = SetStoryState StoryState

instance ToJSON UpdateStory where
  toJSON (SetStoryState state)
    = object [ "current_state" .= state ]

data Story = Story
  { sId            :: StoryId
  , sProjectId     :: ProjectId
  , sName          :: Text
  , sDescription   :: Maybe Text
  , sStoryType     :: StoryType
  , sCurrentState  :: StoryState
  , sEstimate      :: Maybe Double
  , sAcceptedAt    :: Maybe UTCTime
  -- , sDeadline            :: UTCTime
  -- , sProjectedCompletion :: UTCTime
  , sRequestedById :: UserId
  , sOwnerIds      :: [UserId]
  -- , sLabels        :: [Label]
  -- , sFollowerIds   :: [UserId]
  , sCreatedAt     :: UTCTime
  , sUpdatedAt     :: UTCTime
  -- , sBeforeId :: UTCTime
  -- , sAfterId :: UTCtime
  -- , sIntegrationId
  -- , sExternalId
  , sUrl           :: Text
  } deriving (Show, Generic)

instance FromJSON Story where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
