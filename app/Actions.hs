module Actions where

import Data.Text (Text)

actWhen :: Bool -> Action -> Action
actWhen False = const NoAction
actWhen True = id

actWhenF :: Functor f => Bool -> f Action -> f Action
actWhenF b = fmap $ actWhen b

actUnlessF :: Functor f => Bool -> f Action -> f Action
actUnlessF b = fmap $ actWhen $ not b

actOnJustM :: Monad m => (a -> m Action) -> Maybe a -> m Action
actOnJustM f (Just a) = f a
actOnJustM _ Nothing = pure NoAction

data Action
  = NoAction
  | AddTasks [Text]
  | CompleteTask Text
  | SetTasksHeader Text
  | SetNoTasksText Text
  | CreateNewMainMessage

