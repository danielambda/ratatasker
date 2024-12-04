{-# LANGUAGE BlockArguments #-}
module TelegramUtils where

import Models

import Telegram.Bot.Simple

import Data.Text (Text)
import Data.Functor (void)
import Data.Foldable (for_, traverse_)
import Telegram.Bot.API hiding (UserId, User)

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

