{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flume where

import Miso.String ( MisoString )
import qualified Miso as Miso
import Miso.Html ( View
                 , div_
                 , Attribute
                 , button_
                 )
import Miso.Event ( KeyCode(KeyCode)
                  , Checked(Checked)
                  , Options
                  , AllowDrop
                  )
import qualified Miso.Html.Event as MisoHE
import Protolude

import           Control.Monad.Trans                         ( MonadTrans )

import Flume.Types.FlumeT (FlumeT(FlumeT), runFlumeT)
import qualified Flume.Types.FlumeT as F
import qualified Data.Text                      as Text
import Control.Concurrent.STM.TQueue (TQueue)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)

data DomEvent = DomNull FlumeEventId
              | DomString FlumeEventId MisoString
              | DomKeyCode FlumeEventId KeyCode
              | DomChecked FlumeEventId Checked
  deriving (Eq, Show)

newtype FlumeEventId = FlumeEventId Int
  deriving (Eq, Ord, Show, Read, Num)

data FlumeEvent event = CmdEvent FlumeEventId Dynamic
                      | DomEvent DomEvent
                      | GlobalDomEvent DomEvent
                      | GlobalEvent event

-- data FlumeState m' event = FlumeState
--   { nextEventId :: FlumeEventId
--   , actions     :: [m' (FlumeEvent event)]
--   , dom         :: [View JsEvent]
--   }


type Flume m' event a = FlumeT m' (FlumeEvent event) [View DomEvent] (State FlumeEventId) a

runFlume :: Flume m' event a -> FlumeEventId -> Maybe (FlumeEvent event)
         -> ( ( Bool, [m' (FlumeEvent event)]
              , Either (Flume m' event a, [View DomEvent]) a
              )
            , FlumeEventId )
runFlume m eid me = flip runState eid $ runFlumeT m me

type MisoHtmlElement = [Attribute DomEvent] -> [View DomEvent] -> View DomEvent

el_ :: MisoHtmlElement -> [Attribute DomEvent] -> Flume m' event a
el_ v atts = el (\atts' _ -> v atts' []) atts empty 

el :: MisoHtmlElement -> [Attribute DomEvent] -> Flume m' event a -> Flume m' event a
el v atts (FlumeT g) = FlumeT $ \mevent -> do
  (consumed, actions, er) <- g mevent
  case er of
    Right a -> return (consumed, actions, Right a)
    Left (cont, vs) -> return ( consumed
                              , actions
                              , Left (cont, [v atts vs]))

text :: MisoString -> Flume m' event a
text t = FlumeT $ \_ -> return ( False, [], Left (text t, [Miso.text t]) )

button :: MisoString -> Flume m' event ()
button t = do
  eid <- lift get
  lift . put $ eid + 1
  el button_ [ MisoHE.onClick $ DomNull eid ]
    $ (F.listen $ f eid)
  where
    f eid (DomEvent (DomNull eid'))
      | eid == eid' = return ()
      | otherwise = Nothing

cmd :: (Monad m', Typeable a) => m' a -> Flume m' event a
cmd action = do
  eid <- lift get
  F.cmd (CmdEvent eid . toDyn <$> action)
  lift . put $ eid + 1
  F.listen $ cmdListen eid
    where
      cmdListen eid (CmdEvent eid' x)
        | eid == eid' = fromDynamic x
        | otherwise = Nothing
      cmdListen _ _ = Nothing

emitGlobalEvent :: Monad m' => m' event -> Flume m' event ()
emitGlobalEvent action = F.cmd $ GlobalEvent <$> action


-- should use F.peek?
-- but need to stop propogating global in children after consumed
listen :: (event -> Maybe a) -> Flume m' event a
listen mf = F.listen f where
  f (GlobalEvent e) = mf e
  f _ = Nothing
