{-# LANGUAGE OverloadedStrings #-}

module Persistence where

import Models

import Telegram.Bot.API.Types hiding (User, UserId)
import Database.SQLite.Simple

import Data.Text (Text)
import Data.Maybe (listToMaybe)

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
    \noTasksText VARCHAR NOT NULL,\
    \tasksHeader VARCHAR NOT NULL,\
    \PRIMARY KEY (chatId, messageThreadId)\
    \)"

addTask :: Connection -> UserId -> Text -> IO ()
addTask conn (UserId (ChatId chatId) mThreadId) task =
  execute conn
    "INSERT INTO tasks (userChatId, userMessageThreadId, text) VALUES (?, ?, ?)"
    (chatId, mThreadId, task)

deleteTask :: Connection -> UserId -> Text -> IO ()
deleteTask conn (UserId (ChatId chatId) mThreadId) task =
  case mThreadId of
    Just (MessageThreadId threadId) -> execute conn
      "DELETE FROM tasks WHERE userChatId = ? AND userMessageThreadId = ? AND text = ?"
      (chatId, threadId, task)
    Nothing -> execute conn
      "DELETE FROM tasks WHERE userChatId = ? AND userMessageThreadId IS NULL AND text = ?"
      (chatId, task)

updateTasksHeader :: Connection -> UserId -> Text -> IO ()
updateTasksHeader conn (UserId (ChatId chatId) mThreadId) header =
  case mThreadId of
    Just (MessageThreadId threadId) -> execute conn
      "UPDATE users SET tasksHeader = ? WHERE chatId = ? AND messageThreadId = ?"
      (header, chatId, threadId)
    Nothing -> execute conn
      "UPDATE users SET tasksHeader = ? WHERE chatId = ? AND messageThreadId IS NULL"
      (header, chatId)

updateNoTasksText :: Connection -> UserId -> Text -> IO ()
updateNoTasksText conn (UserId (ChatId chatId) mThreadId) txt =
  case mThreadId of
    Just (MessageThreadId threadId) -> execute conn
      "UPDATE users SET noTasksText = ? WHERE chatId = ? AND messageThreadId = ?"
      (txt, chatId, threadId)
    Nothing -> execute conn
      "UPDATE users SET noTasksText = ? WHERE chatId = ? AND messageThreadId IS NULL"
      (txt, chatId)

updateMainMessageId :: Connection -> UserId -> MessageId -> IO ()
updateMainMessageId conn (UserId (ChatId chatId) mThreadId) (MessageId msgId) =
  case mThreadId of
    Just (MessageThreadId threadId) -> execute conn
      "UPDATE users SET mainMessageId = ? WHERE chatId = ? AND messageThreadId = ?"
      (msgId, chatId, threadId)
    Nothing -> execute conn
      "UPDATE users SET mainMessageId = ? WHERE chatId = ? AND messageThreadId IS NULL"
      (msgId, chatId)

getTasks :: Connection -> UserId -> IO [Text]
getTasks conn (UserId (ChatId chatId) mThreadId) =
  map fromOnly <$> case mThreadId of
    Just (MessageThreadId threadId) -> query conn
      "SELECT text FROM tasks WHERE userChatId = ? AND userMessageThreadId = ?"
      (chatId, threadId)
    Nothing -> query conn
      "SELECT text FROM tasks WHERE userChatId = ? AND userMessageThreadId IS NULL"
      (Only chatId)

getMainMessageId :: Connection -> UserId -> IO (Maybe MessageId)
getMainMessageId conn (UserId (ChatId chatId) mThreadId) =
  fmap listToMaybe $ map (MessageId . fromOnly) <$> case mThreadId of
    Just (MessageThreadId threadId) -> query conn
      "SELECT mainMessageId FROM users WHERE chatId = ? AND userMessageThreadId = ?"
      (chatId, threadId)
    Nothing -> query conn
      "SELECT mainMessageId FROM users WHERE chatId = ? AND userMessageThreadId IS NULL"
      (Only chatId)

getUserWithId :: Connection -> UserId -> IO (Maybe User)
getUserWithId conn userId@(UserId (ChatId chatId) mThreadId) = do
  result <- maybe
    queryUserWithoutThreadId
    queryUserWithThreadId
    mThreadId

  case result of
    [(mainMsgId, noTasksText, tasksHeader)] -> do
      tasks <- getTasks conn userId
      return $ Just $
        User tasks (MessageId mainMsgId) (VisualConfig noTasksText tasksHeader)

    _ -> return Nothing
  where
    queryUserWithoutThreadId = query conn
      "SELECT \
      \mainMessageId, noTasksText, tasksHeader \
      \FROM users WHERE chatId = ? AND messageThreadId IS NULL"
      (Only chatId)

    queryUserWithThreadId (MessageThreadId threadId) = query conn
      "SELECT \
      \mainMessageId, noTasksText, tasksHeader \
      \FROM users WHERE chatId = ? AND messageThreadId = ?"
      (chatId, threadId)

createEmptyUser :: Connection -> UserId -> MessageId -> VisualConfig -> IO User
createEmptyUser
  conn
  (UserId (ChatId chatId) mThreadId)
  (MessageId mainMsgId)
  (VisualConfig noTasksText header)
  = do
  execute conn
    "INSERT INTO users \
    \(chatId, messageThreadId, mainMessageId, noTasksText, tasksHeader) \
    \VALUES (?, ?, ?, ?, ?)"
    (chatId, mThreadId, mainMsgId, noTasksText, header)

  return $ User [] (MessageId mainMsgId) (VisualConfig noTasksText header)
