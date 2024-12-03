{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Persistence where

import Models

import Database.SQLite.Simple
import Data.Text (Text)
import Telegram.Bot.API.Types hiding (User, UserId)

initDb :: Connection -> IO ()
initDb conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS tasks (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \userChatId INTEGER NOT NULL,\
    \userMessageThreadId INTEGER,\
    \text TEXT NOT NULL,\
    \FOREIGN KEY (userChatId, userMessageThreadId)\
    \REFERENCES users(chatId, messageThreadId) ON DELETE CASCADE\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS users (\
    \chatId INTEGER NOT NULL,\
    \messageThreadId INTEGER,\
    \mainMessageId INTEGER NOT NULL,\
    \visualNoTasksText VARCHAR NOT NULL,\
    \visualTasksHeaderText VARCHAR NOT NULL,\
    \PRIMARY KEY (chatId, messageThreadId)\
    \)"

addTask :: Connection -> UserId -> Text -> IO ()
addTask conn (UserId (ChatId chatIdInt) mThreadId) task =
  let mThreadIdInt = fmap (\(MessageThreadId m) -> m) mThreadId
  in execute conn
    "INSERT INTO tasks\
    \(userChatId, userMessageThreadId, text) VALUES (?, ?, ?)"
    (chatIdInt, mThreadIdInt, task)

deleteTask :: Connection -> UserId -> Text -> IO ()
deleteTask conn (UserId (ChatId chatIdInt) mThreadId) task =
  let mThreadIdInt = fmap (\(MessageThreadId m) -> m) mThreadId
  in execute conn
    "DELETE FROM tasks \
    \WHERE (userChatId = ?, userMessageThreadId = ?, text = ?)"
    (chatIdInt, mThreadIdInt, task)

updateTasksHeader :: Connection -> UserId -> Text -> IO ()
updateTasksHeader conn (UserId (ChatId chatIdInt) Nothing) header =
  execute conn
    "UPDATE users SET visualTasksHeader = ? WHERE chatId = ? AND messageThreadId IS NULL"
    (header, chatIdInt)
updateTasksHeader conn (UserId (ChatId chatIdInt) (Just (MessageThreadId threadIdInt))) header =
  execute conn
    "UPDATE users SET visualTasksHeader = ? WHERE chatId = ? AND messageThreadId = ?"
    (header, chatIdInt, threadIdInt)

updateNoTasksText :: Connection -> UserId -> Text -> IO ()
updateNoTasksText conn (UserId (ChatId chatIdInt) Nothing) txt =
  execute conn
    "UPDATE users SET visualNoTasksText = ? WHERE chatId = ? AND messageThreadId IS NULL"
    (txt, chatIdInt)
updateNoTasksText conn (UserId (ChatId chatId) (Just (MessageThreadId threadId))) txt =
  execute conn
    "UPDATE users SET visualNoTasksText = ? WHERE chatId = ? AND messageThreadId = ?"
    (txt, chatId, threadId)

getTasks :: Connection -> UserId -> IO [Text]
getTasks conn (UserId (ChatId chatId) Nothing) =
  fmap fromOnly <$> query conn
    "SELECT text FROM tasks WHERE userChatId = ? AND userMessageThreadId IS NULL"
    (Only chatId)
getTasks conn (UserId (ChatId chatId) (Just (MessageThreadId threadId))) =
  fmap fromOnly <$> query conn
    "SELECT text FROM tasks WHERE userChatId = ? AND userMessageThreadId = ?"
    (chatId, threadId)

getUserWithId :: Connection -> UserId -> IO (Maybe User)
getUserWithId conn userId@(UserId (ChatId chatId) mThreadId) =
  maybe
    queryUserWithoutThreadId
    queryUserWithThreadId
    mThreadId >>=
  \case
    [(mainMsgId, noTasksText, tasksHeader)] -> do
      tasks <- getTasks conn userId
      return $ Just $
        User tasks (MessageId mainMsgId) (VisualConfig noTasksText tasksHeader)

    _ -> return Nothing
  where
    queryUserWithoutThreadId = query conn
      "SELECT \
      \mainMessageId, visualNoTasksText, visualTasksHeaderText \
      \FROM users WHERE chatid = ? AND messageThreadId IS NULL"
      (Only chatId)

    queryUserWithThreadId (MessageThreadId threadId) = query conn
      "SELECT \
      \mainMessageId, visualNoTasksText, visualTasksHeaderText \
      \FROM users WHERE chatid = ? AND messageThreadId = ?"
      (chatId, threadId)

createEmptyUser :: Connection -> UserId -> MessageId -> VisualConfig -> IO User
createEmptyUser conn
  (UserId (ChatId chatId) mThreadIdWrapped)
  (MessageId mainMsgId) (VisualConfig noTasksText header) = do
  let mThreadId = (\(MessageThreadId x) -> x) <$> mThreadIdWrapped
  execute conn
    "INSERT INTO users \
    \(chatId, messageThreadId, \
    \mainMessageId, visualNoTasksText, visualTasksHeaderText) \
    \VALUES (?, ?, ?, ?, ?)"
    (chatId, mThreadId, mainMsgId, noTasksText, header)

  return $ User [] (MessageId mainMsgId) (VisualConfig noTasksText header)
