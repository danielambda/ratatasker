{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text, command)

import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Control.Monad (void, (<=<), (>=>))
import Data.Functor ((<&>))
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import qualified Text.Read
import Data.Text (Text)
import qualified Data.Text as Text

readMaybe :: Read a => Text -> Maybe a
readMaybe = Text.Read.readMaybe . Text.unpack

actWhen :: Bool -> Action -> Action
actWhen False = const NoAction
actWhen True = id

actWhenF :: Functor f => Bool -> f Action -> f Action
actWhenF b = fmap $ actWhen b

actUnlessF :: Functor f => Bool -> f Action -> f Action
actUnlessF b = fmap $ actWhen $ not b

actOnJust :: (a -> Action) -> Maybe a -> Action
actOnJust f (Just a) = f a
actOnJust _ Nothing = NoAction

actOnJustM :: Monad m => (a -> m Action) -> Maybe a -> m Action
actOnJustM f (Just a) = f a
actOnJustM _ Nothing = pure NoAction

sendTextMessageTo :: ChatId -> Maybe MessageThreadId -> Text -> BotM Message
sendTextMessageTo chatId mThreadId msgText =
  let someChatId = SomeChatId chatId in
  fmap responseResult $
    runTG $ Telegram.sendMessage $ SendMessageRequest Nothing
      someChatId
      mThreadId
      msgText Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

deleteUpdateMessage :: ChatId -> BotM () -- TODO why take chatId here?
deleteUpdateMessage chatId = do
  mMsgId <- asks $ fmap messageMessageId . updateMessage <=< botContextUpdate
  for_ mMsgId (void . runTG . deleteMessage chatId)

data Model = Model
  { modelTasks :: [Text]
  , modelMainMessage :: Maybe CompoundMessageId
  , modelVisualConfig :: VisualConfig
  }

data CompoundMessageId = CompoundMessageId ChatId MessageId

data VisualConfig = VisualConfig
  { visualNoTasksText :: Text
  , visualTasksHeader :: Text
  }

renderTasks :: Model -> Text
renderTasks model =
  let tasks = modelTasks model
      noTasksText = visualNoTasksText $ modelVisualConfig model
      header = visualTasksHeader $ modelVisualConfig model
  in case tasks of
    [] -> noTasksText
    _ -> tasks
      & reverse
      & zipWith (<>) (map ((<> ". ") . Text.pack . show) [1..])
      & (header:)
      & Text.unlines

data Action
  = NoAction
  | SetMainMessageId CompoundMessageId
  | AddTasks [Text]
  | CompleteTask Text
  | SetTasksHeader Text
  | SetNoTasksText Text

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model [] Nothing initialVisual
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }
  where
    initialVisual = VisualConfig
      { visualNoTasksText = "No tasks available! Take a break or add some tasks. 😊"
      , visualTasksHeader = "Tasks:"
      }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate $
  CompleteTask <$> (command "done" <|> command "check" <|> command "complete") <|>
  SetTasksHeader <$> (command "setHeader" <|> command "set_header") <|>
  SetNoTasksText <$> (command "setNoTasksText" <|> command "set_no_tasks_text") <|>
  AddTasks . reverse . Text.lines <$> text

addTasks :: [Text] -> Model -> Model
addTasks tasks model = model{modelTasks = tasks ++ modelTasks model}

completeTask :: Text -> Model -> Model
completeTask task model = model{modelTasks = filter (/= task) (modelTasks model)}

setMainMessage :: CompoundMessageId -> Model -> Model
setMainMessage mainMsg model =
  model{modelMainMessage = Just mainMsg}

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = model <# pure ()

handleAction (AddTasks tasks) model =
  let model' = addTasks tasks model
  in model' <# do
    currentChatId >>= traverse_ deleteUpdateMessage
    createOrEditMainMessage model'

handleAction (CompleteTask task) model =
  let model' = completeTask task model
  in model' <# do
    currentChatId >>= traverse_ deleteUpdateMessage
    createOrEditMainMessage model'

handleAction (SetMainMessageId msgId) model =
  setMainMessage msgId model <# pure ()

handleAction (SetTasksHeader header) model =
  let model' = model{modelVisualConfig = (modelVisualConfig model){visualTasksHeader = header}}
  in model' <# do
    currentChatId >>= traverse_ deleteUpdateMessage
    actUnlessF (null $ modelTasks model') $
      createOrEditMainMessage model'

handleAction (SetNoTasksText txt) model =
  let model' = model{modelVisualConfig = (modelVisualConfig model){visualNoTasksText = txt}}
  in model' <# do
    currentChatId >>= traverse_ deleteUpdateMessage
    actWhenF (null $ modelTasks model') $
      createOrEditMainMessage model'

createOrEditMainMessage :: Model -> BotM Action
createOrEditMainMessage model =
  maybe createNew edit
    $ modelMainMessage model
  where
    msgText = renderTasks model

    edit (CompoundMessageId chatId msgId) = do
      let someChatId = SomeChatId chatId
      editMessage
        (EditChatMessageId someChatId msgId)
        (toEditMessage msgText)
      return NoAction

    createNew = do
      mChatId <- currentChatId
      mThreadId <- asks $ messageMessageThreadId <=< updateMessage <=< botContextUpdate
      actOnJustM
        (\chatId -> sendTextMessageTo chatId mThreadId msgText
                <&> SetMainMessageId . compoundIdOfMessage
        ) mChatId

compoundIdOfMessage :: Message -> CompoundMessageId
compoundIdOfMessage =
  CompoundMessageId <$> (chatId . messageChat) <*> messageMessageId

run :: Telegram.Token -> IO ()
run token =
  Telegram.defaultTelegramClientEnv token >>=
    startBot_ (conversationBot keySelector bot)
  where
    keySelector :: Update -> Maybe (Maybe ChatId, Maybe MessageThreadId)
    keySelector = (Just .) $ (,) <$> updateChatId <*> (updateMessage >=> messageMessageThreadId)

main :: IO ()
main = do
  putStrLn "The bot is running"
  run "7946973775:AAECaTwKRYC4D5YSZ8hD9wI2pT_HoTlOcwU"
