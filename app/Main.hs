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
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text, command, callbackQueryDataRead, mkParser)
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

renderTasks :: User -> Text
renderTasks user =
  let tasks = userTasks user
      noTasksText = visualNoTasksText $ userVisualConfig user
      header = visualTasksHeader $ userVisualConfig user
  in case tasks of
    [] -> noTasksText
    _ -> tasks
      & zipWith (<>) (map ((<> ". ") . Text.pack . show) [1..])
      & (header:)
      & Text.unlines

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
  DeletePinMessageMessage <$ mkParser (fmap messageMessageId . messagePinnedMessage <=< updateMessage) <|>
  CompleteTask <$> command "done" <|>
  SetTasksHeader <$> command "setheader" <|>
  SetNoTasksText <$> command "setnotaskstext" <|>
  CreateNewMainMessage <$ command "reload" <|>
  ShowHelp <$ command "help" <|>
  callbackQueryDataRead <|>
  AddTasks . Text.lines <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model

handleAction DeletePinMessageMessage model = model <#
  deleteUpdateMessage

handleAction (UpdateMainMessage userId) (Model conn) = Model conn <# do
  user <- getOrCreateUser conn userId
  updateMainMessage userId user

handleAction (AddTasks tasks) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    forM_ tasks $ liftIO . addTask conn userId
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
    user <- getOrCreateUser conn userId
    actUnlessF (null $ userTasks user) $
      updateMainMessage userId user

handleAction (SetNoTasksText txt) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    liftIO $ updateNoTasksText conn userId txt
    deleteUpdateMessage
    user <- getOrCreateUser conn userId
    actWhenF (null $ userTasks user) $
      updateMainMessage userId user

handleAction CreateNewMainMessage (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId@(UserId chatId _) -> do
    txt <- renderTasks <$> getOrCreateUser conn userId
    mainMsgId <- messageMessageId <$> sendTextMessageTo userId txt
    pinMessageUniquely chatId mainMsgId
    liftIO $ updateMainMessageId conn userId mainMsgId
    deleteUpdateMessage
    return NoAction

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

updateMainMessage :: UserId -> User -> BotM Action
updateMainMessage (UserId chatId _) user =
  edit $ userMainMessage user
  where
    msgText = renderTasks user

    edit msgId = do
      let someChatId = SomeChatId chatId
      editMessage
        (EditChatMessageId someChatId msgId)
        (toEditMessage msgText)
      return NoAction

currentUserId :: BotM (Maybe UserId)
currentUserId =
  currentChatId >>= traverse \chatId -> do
    mThreadId <- asks $ botContextUpdate >=> updateMessage >=> messageMessageThreadId
    return $ UserId chatId mThreadId

currentUser :: Connection -> BotM (Maybe User)
currentUser conn =
  fmap join $ currentUserId >>= traverse (liftIO . getUserWithId conn)

getOrCreateUser :: Connection -> UserId -> BotM User
getOrCreateUser conn userId =
  maybe initDefaultUser pure =<< currentUser conn
  where
    initDefaultUser =
      let visualConfig = VisualConfig
            "No tasks available! Take a break or add some tasks. 😊"
            "Tasks:"
          initialText = "welcome to the ratatasker"
      in initUser conn userId initialText visualConfig

initUser :: Connection -> UserId -> Text -> VisualConfig -> BotM User
initUser conn userId initialText visualConfig = do
  mainMsgId <- messageMessageId <$> sendTextMessageTo userId initialText
  liftIO $ createEmptyUser conn userId mainMsgId visualConfig

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

