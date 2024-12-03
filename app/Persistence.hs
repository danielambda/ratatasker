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
addTask conn (UserId (ChatId chatId) mThreadIdWrapped) task =
  let mThreadId = fmap (\(MessageThreadId m) -> m) mThreadIdWrapped
  in execute conn
    "INSERT INTO tasks\
    \(userChatId, userMessageThreadId, text) VALUES (?, ?, ?)"
    (chatId, mThreadId, task)

deleteTask :: Connection -> UserId -> Text -> IO ()
deleteTask conn (UserId (ChatId chatId) (Just (MessageThreadId threadId))) task =
  execute conn
    "DELETE FROM tasks \
    \WHERE userChatId = ? AND userMessageThreadId = ? AND text = ?"
    (chatId, threadId, task)
deleteTask conn (UserId (ChatId chatId) Nothing) task =
  execute conn
    "DELETE FROM tasks \
    \WHERE userChatId = ? AND userMessageThreadId IS NULL AND text = ?"
    (chatId, task)

updateTasksHeader :: Connection -> UserId -> Text -> IO ()
updateTasksHeader conn (UserId (ChatId chatId) Nothing) header =
  execute conn
    "UPDATE users SET visualTasksHeader = ? WHERE chatId = ? AND messageThreadId IS NULL"
    (header, chatId)
updateTasksHeader conn (UserId (ChatId chatId) (Just (MessageThreadId threadId))) header =
  execute conn
    "UPDATE users SET visualTasksHeader = ? WHERE chatId = ? AND messageThreadId = ?"
    (header, chatId, threadId)

updateNoTasksText :: Connection -> UserId -> Text -> IO ()
updateNoTasksText conn (UserId (ChatId chatId) Nothing) txt =
  execute conn
    "UPDATE users SET visualNoTasksText = ? WHERE chatId = ? AND messageThreadId IS NULL"
    (txt, chatId)
updateNoTasksText conn (UserId (ChatId chatId) (Just (MessageThreadId threadId))) txt =
  execute conn
    "UPDATE users SET visualNoTasksText = ? WHERE chatId = ? AND messageThreadId = ?"
    (txt, chatId, threadId)

updateMainMessageId :: Connection -> UserId -> MessageId -> IO ()
updateMainMessageId conn (UserId (ChatId chatId) Nothing) (MessageId msgId) =
  execute conn
    "UPDATE users SET mainMessageId = ? WHERE chatId = ? AND messageThreadId IS NULL"
    (msgId, chatId)
updateMainMessageId conn (UserId (ChatId chatId) (Just (MessageThreadId threadId))) (MessageId msgId) =
  execute conn
    "UPDATE users SET mainMessageId = ? WHERE chatId = ? AND messageThreadId = ?"
    (msgId, chatId, threadId)

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
