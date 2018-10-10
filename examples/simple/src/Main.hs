{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Main where

import Protolude

import Miso
import qualified Miso as Miso
import Miso.String
import Flume ( Flume, el, el_, DomEvent
             , FlumeEventId
             , runFlume
             , FlumeEvent
             )
import qualified Flume as Flume

type Model = Int

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

main' :: IO ()
main' = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  print ("Hello World" :: MisoString) >> pure NoOp

viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]

data FlumeModel event = FlumeModel
  { flumeEventId :: FlumeEventId
  , flumeView :: View DomEvent
  , flumeCont :: Flume IO event ()
  }

instance Eq (FlumeModel event) where
  m1 == m2 = False

startFlume :: Flume IO event () -> IO ()
startFlume p = startApp App {..}
  where
    initialAction = FlumeInit
    model  = FlumeModel
      { flumeEventId = 0
      , flumeView = text "initializing..."
      , flumeCont = p }
    update = flumeUpdateModel
    view   = flumeViewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

data FlumeAction event = FlumeEventAction (FlumeEvent event)
                       | FlumeNoOp
                       | FlumeInit


flumeUpdateModel :: FlumeAction event -> FlumeModel event
                 -> Effect (FlumeAction event) (FlumeModel event)
flumeUpdateModel action model = case action of
  FlumeNoOp -> noEff model
  FlumeInit ->  updateFlume Nothing
  FlumeEventAction fe -> updateFlume (Just fe)
  where
    updateFlume mfe =
      let ((_, effects, er), eid) = runFlume (flumeCont model) (flumeEventId model) mfe in
        case er of
          --should run effs even when finished
          (Right a) -> noEff $ model { flumeView = Miso.text "finished." }
          (Left (cont, vs)) -> batchEff
            (model { flumeEventId = eid
                   , flumeView = div_ [] vs
                   , flumeCont = cont } )
            $ fmap (fmap FlumeEventAction) effects
      
flumeViewModel :: FlumeModel event -> View (FlumeAction event)
flumeViewModel m = FlumeEventAction . Flume.DomEvent <$> flumeView m

main :: IO ()
main = startFlume simple

simple :: forall event. Flume IO event ()
simple = void $ el div_ [] $ do
--  ((Flume.cmd $ print ("Hello Flume Console" :: MisoString)) <|> (Flume.cmd $ print ("So async!" :: MisoString)))
  Flume.button "Click here to load the counter"
  (el div_ [] $ Flume.text "three") <|> (Flume.cmd $ threadDelay 1000000)
  (el div_ [] $ Flume.text "two") <|> (Flume.cmd $ threadDelay 1000000)
  (el div_ [] $ Flume.text "one") <|> (Flume.cmd $ threadDelay 1000000)
  counter 0
  where
    counter :: Int -> Flume IO event a
    counter n = do
      d <- displayCounter
      counter (n + d)
      where
        displayCounter = (Flume.button "-" >> return (-1))
                     <|> Flume.text (ms n)
                     <|> (Flume.button "+" >> return 1)

