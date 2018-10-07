{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flume.Types.FlumeT where

import Flume.Prelude

import           Control.Monad.Trans                         ( MonadTrans )
import qualified Data.Text                      as Text
import Control.Concurrent.STM.TQueue (TQueue)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)

data DomEvent
  = Click
  | KeyDown Char
  deriving (Eq, Read, Show)


newtype FlumeT m' event r m a =
  FlumeT { runFlumeT :: Maybe event
           -> m (Bool
                , [m' event]
                , Either (FlumeT m' event r m a, r) a) }

instance MonadTrans (FlumeT m' event r) where
  lift ma = FlumeT $ \_ -> (False,[],) . Right <$> ma

instance Monad m => Functor (FlumeT m' event r m) where
  fmap f (FlumeT g) = FlumeT $ \event -> do
    (consumed, actions, er) <- g event
    case er of
      Right a -> return (consumed, actions, Right $ f a)
      Left (cont, r) -> return (consumed, actions, Left (f <$> cont, r))

instance Monad m => Applicative (FlumeT m' event r m) where
  pure a = FlumeT $ \_ -> return (False, [], Right a)
  (FlumeT mfab) <*> (FlumeT ma) = FlumeT $ \mevent -> do
    (consumed, actions, efab) <- mfab mevent
    case efab of
      Left (cont, r) -> return (consumed, actions, Left (cont <*> (FlumeT ma), r))
      Right fab -> do
        (consumed', actions', ea) <- ma $ bool mevent Nothing consumed
        case ea of
          Left (cont, r) -> return ( consumed || consumed'
                                   , actions <> actions'
                                   , Left (fab <$> cont, r))
          Right a -> return ( consumed || consumed'
                            , actions <> actions'
                            , Right $ fab a)

instance Monad m => Monad (FlumeT m' event r m) where
  return = pure
  (FlumeT ma) >>= famb = FlumeT $ \mevent -> do
    (consumed, actions, er) <- ma mevent
    case er of
      Left (errma, r) -> return (consumed, actions, Left (errma >>= famb, r))
      Right a -> do
        let (FlumeT mb) = famb a
        (consumed', actions', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Left (errmb, r) -> return ( consumed || consumed'
                                    , actions <> actions'
                                    , Left (errmb, r))
          Right b -> return ( consumed || consumed'
                            , actions <> actions'
                            , Right b)

instance (Monoid r, Monad m) => Alternative (FlumeT m' event r m) where
  empty = FlumeT $ \_ -> return (False, [], Left (empty, mempty))
  (FlumeT ma) <|> (FlumeT mb) = FlumeT $ \ mevent -> do
    (consumed, actions, er) <- ma mevent
    case er of
      Right a -> return (consumed, actions, Right a)
      Left (conta, ra) -> do
        (consumed', actions', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Right b -> return ( consumed' || consumed
                            , actions <> actions'
                            , Right b )
          Left (contb, rb) -> return ( consumed' || consumed
                                     , actions <> actions'
                                     , Left (conta <|> contb, ra <> rb))


listen :: (Monad m, Monoid r) => (event -> Maybe a) -> FlumeT m' event r m a
listen p = FlumeT $ \mevent -> case join (p <$> mevent) of
  Nothing -> return (False, [], Left (listen p, mempty))
  (Just a) -> return (True, [], Right a)

peek :: (Monad m, Monoid r) => (event -> Maybe a) -> FlumeT m' event r m a
peek p = FlumeT $ \mevent -> case join (p <$> mevent) of
  Nothing -> return (False, [], Left (listen p, mempty))
  (Just a) -> return (False, [], Right a)

cmd :: Monad m =>  m' event -> FlumeT m' event r m ()
cmd action = FlumeT $ \_ -> return (False, [action], Right ())

consumeEvent :: Monad m => FlumeT m' event r m ()
consumeEvent = FlumeT $ \_ -> return (True, [], Right ())


-- newtype EventListenT event m a =
--   EventListenT { runEventListenT ::
--              Maybe event -> m ( Bool
--                               , Either (EventListenT event m a) a)
--          }

-- instance MonadTrans (EventListenT event) where
--   lift ma = EventListenT $ \_ -> (False,) . Right <$> ma

-- instance Monad m => Functor (EventListenT event m) where
--   fmap f (EventListenT g) = EventListenT $ \event -> do
--     (consumed, er) <- g event
--     case er of
--       Right a -> return (consumed, Right $ f a)
--       Left cont -> return (consumed, Left $ f <$> cont)

-- instance Monad m => Applicative (EventListenT event m) where
--   pure a = EventListenT $ \_ -> return (False, Right a)
--   (EventListenT mfab) <*> (EventListenT ma) = EventListenT $ \mevent -> do
--     (consumed, efab) <- mfab mevent
--     case efab of
--       Left cont -> return (consumed, Left $ cont <*> (EventListenT ma))
--       Right fab -> do
--         (consumed', ea) <- ma $ bool mevent Nothing consumed
--         case ea of
--           Left cont -> return (consumed || consumed', Left $ fab <$> cont)
--           Right a -> return (consumed || consumed', Right $ fab a)

-- instance Monad m => Monad (EventListenT event m) where
--   return = pure
--   (EventListenT ma) >>= famb = EventListenT $ \mevent -> do
--     (consumed, er) <- ma mevent
--     case er of
--       Left errma -> return (consumed, Left $ errma >>= famb)
--       Right a -> do
--         let (EventListenT mb) = famb a
--         (consumed', er') <- mb $ bool mevent Nothing consumed
--         case er' of
--           Left errmb -> return (consumed || consumed', Left errmb)
--           Right b -> return (consumed || consumed', Right b)

-- instance Monad m => Alternative (EventListenT event m) where
--   empty = EventListenT $ \_ -> return (False, Left empty)
--   (EventListenT ma) <|> (EventListenT mb) = EventListenT $ \ mevent -> do
--     (consumed, er) <- ma mevent
--     case er of
--       Right a -> return (consumed, Right a)
--       Left conta -> do
--         (consumed', er') <- mb $ bool mevent Nothing consumed
--         case er' of
--           Right b -> return (consumed' || consumed, Right b)
--           Left contb -> return (consumed' || consumed, Left $ conta <|> contb)

-- listen :: Monad m => (event -> Maybe a) -> EventListenT event m a
-- listen p = EventListenT $ \mevent -> case join (p <$> mevent) of
--   Nothing -> return (False, Left $ listen p)
--   (Just a) -> return (True, Right a)

-- peek :: Monad m => (event -> Maybe a) -> EventListenT event m a
-- peek p = EventListenT $ \mevent -> case join (p <$> mevent) of
--   Nothing -> return (False, Left $ listen p)
--   (Just a) -> return (False, Right a)

-- -- cmd :: Monad m => m' event -> EventListenT m' event m ()
-- -- cmd action = EventListenT $ \_ -> return (False, [action], Right ())

-- consumeEvent :: Monad m => EventListenT event m ()
-- consumeEvent = EventListenT $ \_ -> return (True, Right ())

----------------------

