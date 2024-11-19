{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Telegram.Bot.Simple
import Telegram.Bot.API as Telegram
import Data.Text (Text)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text, command)
import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import qualified Data.Text as Text
import Control.Monad (void, (<=<))
import Data.Functor ((<&>))
import Data.Foldable (for_)
import Data.Function ((&))

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

sendTextMessageTo :: ChatId -> Text -> BotM MessageId
sendTextMessageTo chatId msgText =
  let someChatId = SomeChatId chatId in
  fmap (messageMessageId . responseResult) $
    runTG $ Telegram.sendMessage $ SendMessageRequest Nothing
      someChatId Nothing
      msgText Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Model = Model
  { modelTasks :: [Text]
  , modelMainMessageId :: Maybe Telegram.MessageId
  , modelVisualConfig :: VisualConfig
  }

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
  | AddTasks [Text]
  | CompleteTask Text
  | SetMainMessageId Telegram.MessageId
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

setMainMessageId :: Telegram.MessageId -> Model -> Model
setMainMessageId msgId model = model{modelMainMessageId = Just msgId}

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = model <# pure ()

handleAction (AddTasks tasks) model =
  model' <# do
    currentChatId >>= actOnJustM handleAddTask
  where
    model' = addTasks tasks model
    msgText = renderTasks model'

    handleAddTask chatId = do
      mUserMsgId <- asks $ fmap messageMessageId . updateMessage <=< botContextUpdate
      for_ mUserMsgId (void . runTG . deleteMessage chatId)
      createOrEditMainMessage (modelMainMessageId model') chatId msgText

handleAction (CompleteTask task) model =
  model' <# do
    currentChatId >>= actOnJustM handleCompleteTask
  where
    model' = completeTask task model
    msgText = renderTasks model'

    handleCompleteTask chatId = do
      mUserMsgId <- asks $ fmap messageMessageId . updateMessage <=< botContextUpdate
      for_ mUserMsgId (void . runTG . deleteMessage chatId)
      createOrEditMainMessage (modelMainMessageId model') chatId msgText

handleAction (SetMainMessageId msgId) model =
  setMainMessageId msgId model <# pure ()

handleAction (SetTasksHeader header) model =
  model' <# do
    currentChatId >>= actOnJustM handleSetTasksHeader
  where
    model' = model{modelVisualConfig = (modelVisualConfig model){visualTasksHeader = header}}
    msgText = renderTasks model'

    handleSetTasksHeader chatId = do
      mUserMsgId <- asks $ fmap messageMessageId . updateMessage <=< botContextUpdate
      for_ mUserMsgId (void . runTG . deleteMessage chatId)
      actUnlessF (null $ modelTasks model') $
        createOrEditMainMessage (modelMainMessageId model') chatId msgText

handleAction (SetNoTasksText txt) model =
  model' <# do
    currentChatId >>= actOnJustM handleSetTasksHeader
  where
    model' = model{modelVisualConfig = (modelVisualConfig model){visualNoTasksText = txt}}
    msgText = renderTasks model'

    handleSetTasksHeader chatId = do
      mUserMsgId <- asks $ fmap messageMessageId . updateMessage <=< botContextUpdate
      for_ mUserMsgId (void . runTG . deleteMessage chatId)
      actWhenF (null $ modelTasks model') $
        createOrEditMainMessage (modelMainMessageId model') chatId msgText

createOrEditMainMessage :: Maybe MessageId -> ChatId -> Text -> BotM Action
createOrEditMainMessage (Just mainMsgId) chatId msgText = do
  let someChatId = SomeChatId chatId
  editMessage
    (EditChatMessageId someChatId mainMsgId)
    (toEditMessage msgText)
  return NoAction

createOrEditMainMessage Nothing chatId msgText =
  sendTextMessageTo chatId msgText <&> SetMainMessageId

run :: Telegram.Token -> IO ()
run token =
  Telegram.defaultTelegramClientEnv token >>= startBot_ bot

main :: IO ()
main = do
  putStrLn "The bot is running"
  run "7946973775:AAECaTwKRYC4D5YSZ8hD9wI2pT_HoTlOcwU"
