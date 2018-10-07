{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flam where

import Miso.Html (View)
import Protolude

data DomEvent
  = Click
  | KeyDown Char
  deriving (Eq, Read, Show)

newtype FlamT event r m a =
  FlamT { runFlamT :: Maybe event -> m (Bool, Either (FlamT event r m a, r) a) }

instance Monad m => Functor (FlamT event r m) where
  fmap f (FlamT g) = FlamT $ \event -> do
    (consumed, er) <- g event
    case er of
      Right a -> return (consumed, Right $ f a)
      Left (cont, r) -> return (consumed, Left (f <$> cont, r))

instance Monad m => Applicative (FlamT event r m) where
  pure a = FlamT $ \_ -> return (False, Right a)
  (FlamT mfab) <*> (FlamT ma) = FlamT $ \mevent -> do
    (consumed, efab) <- mfab mevent
    case efab of
      Left (cont, r) -> return (consumed, Left (cont <*> (FlamT ma), r))
      Right fab -> do
        (consumed', ea) <- ma $ bool mevent Nothing consumed
        case ea of
          Left (cont, r) -> return (consumed || consumed', Left (fab <$> cont, r))
          Right a -> return (consumed || consumed', Right $ fab a)

instance Monad m => Monad (FlamT event r m) where
  return = pure
  (FlamT ma) >>= famb = FlamT $ \mevent -> do
    (consumed, er) <- ma mevent
    case er of
      Left (errma, r) -> return (consumed, Left (errma >>= famb, r))
      Right a -> do
        let (FlamT mb) = famb a
        (consumed', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Left (errmb, r) -> return (consumed || consumed', Left (errmb, r))
          Right b -> return (consumed || consumed', Right b)

instance (Monoid r, Monad m) => Alternative (FlamT event r m) where
  empty = FlamT $ \_ -> return (False, Left (empty, mempty))
  (FlamT ma) <|> (FlamT mb) = FlamT $ \ mevent -> do
    (consumed, er) <- ma mevent
    case er of
      Right a -> return (consumed, Right a)
      Left (conta, ra) -> do
        (consumed', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Right b -> return (consumed' || consumed, Right b)
          Left (contb, rb) -> return (consumed' || consumed
                                     , Left (conta <|> contb, ra <> rb))

-- data SimpleTree a = SimpleTreeNode [SimpleTree a]
--                   | SimpleTreeLeaf a

data HtmlTag = Div
             | Span
             | TextTag Text
             | Button Text
  deriving (Show, Read, Eq)

data SimpleView = ViewNode HtmlTag [SimpleView]
  deriving (Show, Eq)

type VFlam a = FlamT DomEvent [SimpleView] Identity a

runVFlam :: VFlam a -> Maybe DomEvent -> (Bool, Either (VFlam a, [SimpleView]) a)
runVFlam vf me = runIdentity $ runFlamT vf me

el_ :: HtmlTag -> VFlam a
el_ tag = el tag empty

el :: HtmlTag -> VFlam a -> VFlam a
el tag (FlamT g) = FlamT $ \event -> do
  (consumed, er) <- g event
  case er of
    Right a -> return (consumed, Right a)
    Left (cont, rs) -> return (consumed, Left (cont, [ViewNode tag rs]))

listen :: (Monad m, Monoid r) => (event -> Maybe a) -> FlamT event r m a
listen f = FlamT $ \mevent -> case join (f <$> mevent) of
  Nothing -> return (False, Left (listen f, mempty))
  Just a -> return (True, Right a)


main :: IO ()
main = putText "hello big boy"

printFlam :: forall event m r a. (Show r, Show a) => (Bool, Either (FlamT event r m a, r) a) -> IO ()
printFlam (b, Left (_, r)) = print $ (b, (Left r :: Either r ()))
printFlam (b, Right a) = print $ (b, (Right a :: Either () a))


----------------
button :: Text -> VFlam ()
button label = el (Button label) $ listen isClick
  where
    isClick Click = Just ()
    isClick _ = Nothing

demoVFlam :: VFlam Int
demoVFlam = intMenu <|> intMenu

intMenu :: VFlam Int
intMenu = el Div $
  instructions <|> can'tChoose <|> b 5 <|> b 12
  where
    b n = button ("Return " <> show n) >> return n
    can'tChoose = el Div $ do
      el_ (TextTag "I don't know what to choose!") <|> button "Help me decide!"
      button "Click again if you just want me to pick!"
      return 5
    instructions = el_ $ TextTag "Click one of the buttons if you want to have some fun."
