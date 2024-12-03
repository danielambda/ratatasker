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

import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Control.Monad ((<=<), (>=>), forM_)
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Read
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import GHC.Base (join)

readMaybe :: Read a => Text -> Maybe a
readMaybe = Text.Read.readMaybe . Text.unpack

tryEditMessage :: EditMessageId -> EditMessage -> BotM Bool
tryEditMessage editMsgId editMsg = do
  let request = editMessageToEditMessageTextRequest editMsgId editMsg
  responseOk <$> runTG request

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
  CompleteTask <$> (command "done" <|> command "check" <|> command "complete") <|>
  SetTasksHeader <$> (command "setHeader" <|> command "set_header") <|>
  SetNoTasksText <$> (command "setNoTasksText" <|> command "set_no_tasks_text") <|>
  AddTasks . reverse . Text.lines <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = model <# pure ()

handleAction (AddTasks tasks) (Model conn) = Model conn <# do
  userId <- fromJust <$> currentUserId

  forM_ tasks $ liftIO . addTask conn userId

  deleteUpdateMessage
  mUser <- currentUser conn
  user <- maybe (initUser_ conn userId) pure mUser
  updateMainMessage user

handleAction (CompleteTask task) (Model conn) = Model conn <# do
  userId <- fromJust <$> currentUserId

  liftIO $ deleteTask conn userId task

  deleteUpdateMessage
  mUser <- currentUser conn
  user <- maybe (initUser_ conn userId) pure mUser
  updateMainMessage user

handleAction (SetTasksHeader header) (Model conn) = Model conn <# do
  userId <- fromJust <$> currentUserId

  liftIO $ updateTasksHeader conn userId header

  deleteUpdateMessage
  mUser <- currentUser conn
  user <- maybe (initUser_ conn userId) pure mUser
  actUnlessF (null $ userTasks user) $
    updateMainMessage user

handleAction (SetNoTasksText txt) (Model conn) = Model conn <# do
  userId <- fromJust <$> currentUserId

  liftIO $ updateNoTasksText conn userId txt

  deleteUpdateMessage
  mUser <- currentUser conn
  user <- maybe (initUser_ conn userId) pure mUser
  actWhenF (null $ userTasks user) $
    updateMainMessage user

updateMainMessage :: User -> BotM Action
updateMainMessage user =
  edit $ userMainMessage user
  where
    msgText = renderTasks user

    edit msgId = do
      someChatId <- SomeChatId . fromJust <$> currentChatId
      edited <- tryEditMessage
        (EditChatMessageId someChatId msgId)
        (toEditMessage msgText)
      return if edited then NoAction else undefined

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

run :: Token -> Connection -> IO ()
run token conn =
  defaultTelegramClientEnv token >>=
    startBot_ (bot conn)

main :: IO ()
main = do
  conn <- open "db.db"
  initDb conn
  putStrLn "The bot is running"
  run "7946973775:AAECaTwKRYC4D5YSZ8hD9wI2pT_HoTlOcwU" conn

