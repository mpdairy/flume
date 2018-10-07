{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flume where

import Miso.Html (View)
import Protolude

import           Control.Monad.Trans                         ( MonadTrans )

import Flume.Types.FlumeT (FlumeT(FlumeT), runFlumeT)
import qualified Flume.Types.FlumeT as F
import qualified Data.Text                      as Text
import Control.Concurrent.STM.TQueue (TQueue)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)

data DomEvent
  = Click
  | KeyDown Char
  deriving (Eq, Read, Show)


newtype FlumeEventId = FlumeEventId Int
  deriving (Eq, Ord, Show, Read, Num)

data FlumeEvent event = CmdEvent FlumeEventId Dynamic
                      | DomEvent FlumeEventId DomEvent
                      | GlobalDomEvent DomEvent
                      | GlobalEvent event

-- data FlumeState m' event = FlumeState
--   { nextEventId :: FlumeEventId
--   , actions     :: [m' (FlumeEvent event)]
--   , dom         :: [View JsEvent]
--   }


type Flume m' event a = FlumeT m' (FlumeEvent event) [View DomEvent] (State FlumeEventId) a

el_ :: View DomEvent -> Flume m' event a
el_ v = el (const v) empty 

el :: ([View DomEvent] -> View DomEvent) -> Flume m' event a -> Flume m' event a
el v (FlumeT g) = FlumeT $ \mevent -> do
  (consumed, actions, er) <- g mevent
  case er of
    Right a -> return (consumed, actions, Right a)
    Left (cont, vs) -> return ( consumed
                              , actions
                              , Left (cont, [v vs]))


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

listen :: (event -> Maybe a) -> Flume m' event a
listen mf = F.listen f where
  f (GlobalEvent e) = mf e
  f _ = Nothing
