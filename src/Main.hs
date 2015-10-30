{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Monad.Trans.Either
import qualified Data.Text                  as T
import           System.Environment
import           System.Exit
import           Text.Read                  (readMaybe)
--------------------------------------------------------------------------------
import           Web.Tracker
--------------------------------------------------------------------------------

main :: IO ()
main = do
  token <- fmap T.pack <$> lookupEnv "TRACKER_API_TOKEN"
  query <- getArgs >>= \case
    "deliver":sid:[] -> case readMaybe sid of Just sid -> return $ updateStory token (SetStoryState Delivered) sid
                                                   Nothing  -> fail "Invalid story id"
    _ -> putStrLn "Usage text here" >> exitWith (ExitFailure 1)

  runEitherT query >>= \case
    Right _  -> putStrLn "Done"
    Left err -> print err >> exitWith (ExitFailure 1)

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
