{-# OPTIONS_GHC -Wno-orphans #-}
module Models where

import Telegram.Bot.API hiding (UserId)
import Data.Text (Text)
import Database.SQLite.Simple.ToField (ToField)

deriving instance Read ChatId
deriving instance Read MessageThreadId
deriving instance ToField MessageThreadId
data UserId = UserId ChatId (Maybe MessageThreadId)
  deriving (Show, Read)

data User = User
  { userTasks :: [Text]
  , userMainMessage :: MessageId
  , userVisualConfig :: VisualConfig
  } deriving Show

data VisualConfig = VisualConfig
  { visualNoTasksText :: Text
  , visualTasksHeader :: Text
  } deriving Show

