{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Tracker.Story where

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

instance ToHttpApiData StoryId where
  toUrlPiece (StoryId t) = toUrlPiece t

data ProjectId
  = ProjectId Int
  deriving (Show, Eq, Ord)

instance FromJSON ProjectId where
  parseJSON = fmap ProjectId . parseJSON

instance Read ProjectId where
  readPrec = fmap ProjectId readPrec

instance ToHttpApiData ProjectId where
  toUrlPiece (ProjectId t) = toUrlPiece t

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
  parseJSON = withText "StoryState" $ \case
   "accepted"    -> return Accepted
   "delivered"   -> return Delivered
   "finished"    -> return Finished
   "started"     -> return Started
   "rejected"    -> return Rejected
   "planned"     -> return Planned
   "unstarted"   -> return Unstarted
   "unscheduled" -> return Unscheduled
   other -> fail $ "Invalid story state: " ++ T.unpack other

instance ToJSON StoryState where
  toJSON = toJSON . toUrlPiece

instance ToHttpApiData StoryState where
  toUrlPiece = \case
    Accepted    -> "accepted"
    Delivered   -> "delivered"
    Finished    -> "finished"
    Started     -> "started"
    Rejected    -> "rejected"
    Planned     -> "planned"
    Unstarted   -> "unstarted"
    Unscheduled -> "unscheduled"

data StoryType
  = Feature | Bug | Chore | Release
  deriving (Show, Eq, Ord)

instance FromJSON StoryType where
  parseJSON = withText "StoryType" $ \case
   "feature" -> return Feature
   "bug"     -> return Bug
   "chore"   -> return Chore
   "release" -> return Release
   other -> fail $ "Invalid StoryType: " ++ T.unpack other

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
