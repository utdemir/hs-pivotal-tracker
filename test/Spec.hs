import Test.Hspec

--------------------------------------------------------------------------------
import           Control.Monad.Trans.Either
import           Data.Functor((<$>))
--------------------------------------------------------------------------------
import           Web.Tracker
import           Web.Tracker.Story
import           Web.Tracker.Project
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Web.Tracker story" $
    it "should return story data" $ do
      s <- runEitherT $ story authentication $ StoryId 104591424
      show (sStoryType <$> s) `shouldBe` "Right Feature"
      show (sName <$> s) `shouldBe` "Right \"Shopper should be able to view contents of shopping cart\""
  describe "Web.Tracker project" $
    it "should return project data" $ do
      p <- runEitherT $ project authentication $ ProjectId 1440520
      show (pName <$> p) `shouldBe` "Right \"My Sample Project\""
  where
        authentication = Just "7f3f76bc6ae8c48e7b528369c999c8d8"
