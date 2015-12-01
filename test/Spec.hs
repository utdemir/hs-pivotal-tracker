import Test.Hspec

{-
import Control.Lens ( (^.)
                    , (^?!)
                    , ix
                    )
-}

import           Control.Monad.Trans.Either

import           Web.Tracker
--import qualified Web.Tracker.Story as S

main :: IO ()
main = hspec $
  describe "PivotalTrackerApi.getStory" $
    it "should return story data" $ do
      s <- runEitherT $ story authentication (StoryId 104591424)
      putStrLn $ either show show s
    where
        authentication = Just "7f3f76bc6ae8c48e7b528369c999c8d8"
      {-
      story ^. S.storyType `shouldBe` "feature"
      story ^. S.kind `shouldBe` "story"
      story ^. S.url `shouldBe` "https://www.pivotaltracker.com/story/show/104591424"
      story ^. S.requested_by_id `shouldBe` 1810650
      story ^. S.current_state `shouldBe` "delivered"
      story ^. S.name `shouldBe` "Shopper should be able to view contents of shopping cart"
      story ^. S.project_id `shouldBe` 1440520
      story ^. S.created_at `shouldBe` "2015-09-21T07:00:00Z"
      story ^. S.id `shouldBe` 104591424
      story ^. S.labels ^?! ix 0 . L.kind `shouldBe` "label"
      story ^. S.owner_ids ^?! ix 0 `shouldBe` 1810650
      story ^?! S.estimate `shouldBe` Just 2
      story ^?! S.description `shouldBe` Just "Cart icon in top right corner, with a number indicating how many items in cart"
      -}

  {-
  describe "PivotalTrackerApi.getProject" $
    it "should return project data" $ do
      project <- getProject authentication 1440520
      project ^. P.name `shouldBe` "My Sample Project"

  describe "PivotalTrackerApi.getTask" $
    it "should return task data" $ do
      task <- getTask authentication 1440520 104591424 37657208
      task ^. T.description `shouldBe` "Implement view cart link"

  describe "PivotalTrackerApi.getEpic" $
    it "should return epic data" $ do
      epic <- getEpic authentication 1440520 2106574
      epic ^. E.name `shouldBe` "Admin Users"

  describe "PivotalTrackerApi.getComment" $
    it "should return epic data" $ do
      comment <- getComment authentication 1440520 104591424 118080782
      comment ^. C.text `shouldBe` "testing comment api."

  describe "PivotalTrackerApi.getIterations" $
    it "should return epic data" $ do
      iterations <- getIterations authentication 1440520
      iterations ^?! ix 0 . I.number `shouldBe` 1
    where
      authentication = withToken "7f3f76bc6ae8c48e7b528369c999c8d8" defaults
  -}
