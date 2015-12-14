{-# LANGUAGE DeriveGeneric     #-}

module Web.Tracker.Project where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time.Clock
import           GHC.Generics
import           Servant.API
import           Text.Read
--------------------------------------------------------------------------------

data Timezone = Timezone
                { tKind :: Text
                , tOlsonName :: Text
                , tOffset :: Text
                } deriving (Show, Generic)

data Project = Project
               { pAccountId :: Int
               , pAtomEnabled :: Bool
               , pAutomaticPlanning :: Bool
               , pBugsAndChoresAreEstimatable :: Bool
               , pCreatedAt :: UTCTime
               , pCurrentIterationNumber :: Int
               , pDescription :: Text
               , pEnableFollowing :: Bool
               , pEnableIncoming_emails :: Bool
               , pEnableTasks :: Bool
               , pHasGoogleDomain :: Bool
               , pId :: Int
               , pInitialVelocity :: Int
               , pIterationLength :: Int
               , pKind :: Text
               , pName :: Text
               , pNumberOfDoneIterationsToShow :: Int
               , pPointScale :: Text
               , pPointScaleIsCustom :: Bool
               , pProfileContent :: Text
               , pProjectType :: Text
               , pPublic :: Bool
               , pStartDate :: Maybe Text
               , pStartTime :: Maybe UTCTime
               , pTimeZone :: Timezone
               , pUpdatedAt :: UTCTime
               , pVelocityAveragedOver :: Double
               , pVersion :: Int
               , pWeekStartDay :: Text
               } deriving (Show, Generic)

instance FromJSON Timezone where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
