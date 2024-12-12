{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import TelegramUtils
import Actions
import Persistence
import Models

import Telegram.Bot.API hiding (editMessageReplyMarkup, User, UserId)
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (parseUpdate, command, callbackQueryDataRead, mkParser, plainText)
import Database.SQLite.Simple
import Configuration.Dotenv (loadFile, defaultConfig)

import GHC.Base (join)
import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Control.Monad ((<=<), (>=>), forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Read
import System.Environment (getEnv)

readMaybe :: Read a => Text -> Maybe a
readMaybe = Text.Read.readMaybe . Text.unpack

newtype Model = Model Connection

bot :: Connection -> BotApp Model Action
bot conn = BotApp
  { botInitialModel = Model conn
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ = parseUpdate $
  AddTasks . Text.lines <$> plainText <|>
  callbackQueryDataRead <|>
  CompleteTask <$> command "done" <|>
  DeleteMessage <$ pinMessageMessage <|>
  CreateNewMainMessage <$ command "reload" <|>
  SetTasksHeader <$> command "setheader" <|>
  SetNoTasksText <$> command "setnotaskstext" <|>
  ShowHelp <$ command "help"
  where
    pinMessageMessage = mkParser (fmap messageMessageId . messagePinnedMessage <=< updateMessage)

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model

handleAction DeleteMessage model = model <# deleteUpdateMessage

handleAction (UpdateMainMessage userId@(UserId chatId _)) (Model conn) = Model conn <# do
  user <- getOrCreateUser conn userId
  let mainMsg = userMainMessage user
  let someChatId = SomeChatId chatId
  editMessage
    (EditChatMessageId someChatId mainMsg)
    (editMessageData user)
  where
    replyMarkup tasks = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup $
      map (\task -> [actionButton task (CompleteTask task)]) tasks

    editMessageData user =
      let tasks = user & userTasks
      in case tasks of
        [] -> toEditMessage $ user & userVisualConfig & visualNoTasksText
        _ -> (toEditMessage $ user & userVisualConfig & visualTasksHeader)
               { editMessageReplyMarkup = replyMarkup tasks }

handleAction (AddTasks tasks) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    liftIO $ forM_ tasks $ addTask conn userId
    deleteUpdateMessage
    return $ UpdateMainMessage userId

handleAction (CompleteTask task) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    liftIO $ deleteTask conn userId task
    deleteUpdateMessage
    return $ UpdateMainMessage userId

handleAction (SetTasksHeader header) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    liftIO $ updateTasksHeader conn userId header
    deleteUpdateMessage
    return $ UpdateMainMessage userId

handleAction (SetNoTasksText txt) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    liftIO $ updateNoTasksText conn userId txt
    deleteUpdateMessage
    return $ UpdateMainMessage userId

handleAction CreateNewMainMessage (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    mainMsgId <- sendMainMessage userId initialText
    liftIO $ updateMainMessageId conn userId mainMsgId
    deleteUpdateMessage
    return $ UpdateMainMessage userId
  where
    initialText = "Welcome! This text will be updated soon"

handleAction ShowHelp (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId@(UserId chatId _) -> do
    deleteUpdateMessage
    mainMsgId <- userMainMessage <$> getOrCreateUser conn userId
    editMessage
      (EditChatMessageId (SomeChatId chatId) mainMsgId)
      (toEditMessage helpText){ editMessageReplyMarkup = replyMarkup userId }
    return NoAction
  where
    helpText = Text.unlines
      [ "Here is my the list of commands:"
      , "- Just type any text and I will add it to your tasks"
      , "- Use /done <taskname> to mark task as done"
      , "- Use /reload to create new main message"
      , "- Use /setheader <header> to set header for the list"
      , "- Use /setnotaskstext <text> to set text that will be displayed when the list is empty"
      , "- Use /help to show this message"
      ]

    replyMarkup userId = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
      [[actionButton "Show Tasks" (UpdateMainMessage userId)]]

currentUserId :: BotM (Maybe UserId)
currentUserId =
  currentChatId >>= traverse \chatId -> do
    mThreadId <- asks $ botContextUpdate >=> updateMessage >=> messageMessageThreadId
    return $ UserId chatId mThreadId

getOrCreateUser :: Connection -> UserId -> BotM User
getOrCreateUser conn userId =
  currentUser >>= maybe initDefaultUser pure
  where
    currentUser =
      fmap join $ traverse (liftIO . getUserWithId conn) =<< currentUserId

    initDefaultUser =
      let visualConfig = VisualConfig
            "No tasks available! Take a break or add some tasks. 😊"
            "Tasks:"
          initialText = "welcome to the ratatasker"
      in initUser conn userId initialText visualConfig

initUser :: Connection -> UserId -> Text -> VisualConfig -> BotM User
initUser conn userId initialText visualConfig = do
  mainMsgId <- sendMainMessage userId initialText
  liftIO $ createEmptyUser conn userId mainMsgId visualConfig

sendMainMessage :: UserId -> Text -> BotM MessageId
sendMainMessage userId@(UserId chatId _) initialText = do
  mainMsgId <- messageMessageId <$> sendTextMessageTo userId initialText
  pinMessageUniquely chatId mainMsgId
  return mainMsgId

run :: Connection -> Token -> IO ()
run conn =
  startBot_ (bot conn) <=< defaultTelegramClientEnv

main :: IO ()
main = do
  loadFile defaultConfig

  conn <- open =<< getEnv "DB_CONNECTION_STRING"
  initDb conn

  putStrLn "The bot is running"
  run conn =<< getEnvToken "TELEGRAM_BOT_TOKEN"

