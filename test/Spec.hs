import Test.Hspec

--------------------------------------------------------------------------------
import           Control.Monad.Trans.Either
import           Data.Functor((<$>))
--------------------------------------------------------------------------------
import           Web.Tracker
import           Web.Tracker.Story
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $
  describe "Web.Tracker story" $
    it "should return story data" $ do
      s <- runEitherT $ story authentication (StoryId 104591424)
      show (sStoryType <$> s) `shouldBe` "Right Feature"
      show (sName <$> s) `shouldBe` "Right \"Shopper should be able to view contents of shopping cart\""
    where
        authentication = Just "7f3f76bc6ae8c48e7b528369c999c8d8"
