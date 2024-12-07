module Models where

import Telegram.Bot.API
import Data.Text (Text)

data UserId = UserId ChatId (Maybe MessageThreadId)
  deriving Show

data User = User
  { userTasks :: [Text]
  , userMainMessage :: MessageId
  , userVisualConfig :: VisualConfig
  } deriving Show

data VisualConfig = VisualConfig
  { visualNoTasksText :: Text
  , visualTasksHeader :: Text
  } deriving Show

