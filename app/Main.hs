{-# LANGUAGE OverloadedStrings #-}

module Main where

import Telegram.Bot.Simple
import Telegram.Bot.API as Telegram
import Data.Text (Text)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text, command)
import Control.Applicative ((<|>))

import qualified Data.Text as Text

data Model = Model
  { modelTasks :: [Text]
  , modelMainMessageId :: Maybe Telegram.MessageId
  }

data Action
  = AddTask Text
  | CompleteTask Text
  | ShowTasks

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model [] Nothing
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate $
  CompleteTask <$> (command "done" <|> command "check" <|> command "complete") <|>
  ShowTasks <$ command "show" <|>
  AddTask <$> text

addTask :: Text -> Model -> Model
addTask task model = model{modelTasks = task : modelTasks model}

completeTask :: Text -> Model -> Model
completeTask task model = model{modelTasks = filter (/= task) (modelTasks model)}

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  AddTask task ->
    addTask task model <# return()
  CompleteTask task ->
    completeTask task model <# do
      editUpdateMessageText $ task <> " should be complited"
  ShowTasks ->
    model <# do
      replyText $ Text.unlines $ "Your tasks: " : modelTasks model

run :: Telegram.Token -> IO ()
run token =
  Telegram.defaultTelegramClientEnv token >>= startBot_ bot

main :: IO ()
main = do
  putStrLn "The bot is running"
  run "7946973775:AAECaTwKRYC4D5YSZ8hD9wI2pT_HoTlOcwU"
