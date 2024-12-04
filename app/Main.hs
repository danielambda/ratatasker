{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Actions
import Persistence
import Models

import Telegram.Bot.API hiding (User, UserId)
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text, command)
import Database.SQLite.Simple
import Configuration.Dotenv (loadFile, defaultConfig)

import GHC.Base (join)
import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Control.Monad ((<=<), (>=>), forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Read
import System.Environment (getEnv)
import Data.Functor (void)

readMaybe :: Read a => Text -> Maybe a
readMaybe = Text.Read.readMaybe . Text.unpack

sendTextMessageTo :: UserId -> Text -> BotM Message
sendTextMessageTo (UserId chatId mThreadId) msgText =
  let someChatId = SomeChatId chatId in
  fmap responseResult $
    runTG $ sendMessage $ SendMessageRequest Nothing
      someChatId
      mThreadId
      msgText Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

deleteUpdateMessage :: BotM ()
deleteUpdateMessage =
  currentChatId >>= traverse_ \chatId -> do
    mMsgId <- asks $ fmap messageMessageId . updateMessage <=< botContextUpdate
    for_ mMsgId $ runTG . deleteMessage chatId

pinMessage :: ChatId -> MessageId -> BotM ()
pinMessage chatId msgId =
  void $ runTG $ PinChatMessageRequest (SomeChatId chatId) msgId Nothing

pinMessageUniquely :: ChatId -> MessageId -> BotM ()
pinMessageUniquely chatId msgId = do
  void $ runTG $ unpinAllChatMessages (SomeChatId chatId)
  pinMessage chatId msgId

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
  CompleteTask <$> command "done" <|>
  SetTasksHeader <$> command "setheader" <|>
  SetNoTasksText <$> command "setnotaskstext" <|>
  CreateNewMainMessage <$ command "reload" <|>
  AddTasks . reverse . Text.lines <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = model <# pure ()

handleAction (AddTasks tasks) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    forM_ tasks $ liftIO . addTask conn userId
    deleteUpdateMessage
    user <- getOrCreateUser conn userId
    updateMainMessage userId user

handleAction (CompleteTask task) (Model conn) = Model conn <# do
  currentUserId >>= actOnJustM \userId -> do
    liftIO $ deleteTask conn userId task
    deleteUpdateMessage
    user <- getOrCreateUser conn userId
    updateMainMessage userId user

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

initUser :: Connection -> UserId -> Text -> VisualConfig -> BotM User
initUser conn userId initialText visualConfig = do
  mainMsgId <- messageMessageId <$> sendTextMessageTo userId initialText
  liftIO $ createEmptyUser conn userId mainMsgId visualConfig

initUser_ :: Connection -> UserId -> BotM User
initUser_ conn userId =
  let visualConfig = VisualConfig
        "No tasks available! Take a break or add some tasks. 😊"
        "Tasks:"
      initialText = "welcome to the ratatasker"
  in initUser conn userId initialText visualConfig

currentUserId :: BotM (Maybe UserId)
currentUserId =
  currentChatId >>= traverse \chatId -> do
    mThreadId <- asks $ botContextUpdate >=> updateMessage >=> messageMessageThreadId
    return $ UserId chatId mThreadId

currentUser :: Connection -> BotM (Maybe User)
currentUser conn =
  fmap join $ currentUserId >>= traverse (liftIO . getUserWithId conn)

getOrCreateUser :: Connection -> UserId -> BotM User
getOrCreateUser conn userId = do
  mUser <- currentUser conn
  maybe (initUser_ conn userId) pure mUser

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

