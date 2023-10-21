{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Time as Time

data Visitor
  = Member Profile
  | NonMember (Maybe T.Text)
  deriving Show

data Profile =
  Profile
    { name :: T.Text
    , birthday :: Time.Day
    } deriving Show

main :: IO ()
main = do
  let haskell = Member Profile
        { name = "Mario Camarena"
        , birthday = read "1998-08-14"
        }
  greeting <- makeGreeting haskell
  putStrLn $ T.unpack greeting

makeGreeting :: Visitor -> IO T.Text
makeGreeting visitor =
  case visitor of
    NonMember maybeName ->
      pure $ case maybeName of
        Just name -> "Verifying, " <> name <> "!"
        Nothing   -> "Cannot verify user!"
    Member profile -> do
      today <- Time.utctDay <$> Time.getCurrentTime
      let monthAndDay = (\(_y, m, d) -> (m, d)) . Time.toGregorian
      if monthAndDay today == monthAndDay (birthday profile)
      then pure $ "Match | birth date..., " <> name profile <> "!"
      else pure $ "Welcome back, " <> name profile <> "!"