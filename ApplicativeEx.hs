{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

{-
An exercise in Traversable / Applicative functors from Conor McBride
https://stackoverflow.com/questions/10239630/where-to-find-programming-exercises-for-applicative-functors/10242673

The following has some reference material: http://strictlypositive.org/Idiom.pdf
-}

module ApplicativeEx where

import           Control.Applicative
import           Data.Char
import           Data.Semigroup                 ( Semigroup
                                                , (<>)
                                                )

--import           Data.Traversable

-- (1) --

data Triple a =
  Tr a
     a
     a
  deriving (Show)

instance Functor Triple where
  fmap f (Tr a1 a2 a3) = Tr (f a1) (f a2) (f a3)
  (<$) z Tr {} = Tr z z z

instance Applicative Triple where
  pure a = Tr a a a
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Foldable Triple where
  foldr f z (Tr a1 a2 a3) = f a1 (f a2 (f a3 z))
  foldMap f (Tr a1 a2 a3) = mappend (f a1) (mappend (f a2) (f a3))
  -- null (Tr _ _ _) = False

instance Traversable Triple where
  traverse f (Tr a1 a2 a3) = Tr <$> f a1 <*> f a2 <*> f a3
  --traverse f (Tr a1 a2 a3) = liftA2 Tr (f a1) (f a2) <*> f a3

-- Writing fmap in terms of pure and (<*>)
--fmap' :: Applicative f => (a -> b) -> f a -> f b
--fmap' f x = pure f <*> x

newtype I x = I
  { unI :: x
  } deriving (Show, Eq)


--- (2) ---

-- TODO:: use he standrd Composable type instead

newtype (:.) f g x = Comp
  { comp :: f (g x)
  } deriving (Show)

instance (Functor f, Functor g) => Functor (f :. g) where
  fmap f (Comp x) = Comp $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (f :. g) where
  pure = Comp . pure . pure
  Comp f <*> Comp x = Comp $ fmap (<*>) f <*> x

instance (Foldable f, Foldable g) => Foldable (f :. g) where
  foldMap f (Comp x) = foldMap (foldMap f) x

instance (Traversable f, Traversable g) => Traversable (f :. g) where
  traverse f (Comp x) = pure Comp <*> traverse (traverse f) x

type Zone = Triple :. Triple

type Board = Zone :. Zone

rows :: Board a -> Board a
rows = id

columns :: Board a -> Board a
columns = Comp . sequenceA . comp

boxes :: Board a -> Board a
boxes = Comp . Comp . fmap (fmap Comp . traverse comp) . comp . comp


--- (3) ---

newtype Parse x = Parser
  { parse :: String -> [(x, String)]
  }

instance Functor Parse where
  fmap f (Parser p) = Parser $ \s -> [(f a, b) | (a, b) <- p s]

instance Applicative Parse where
  pure a = Parser $ \s -> [(a, s)]
  (Parser left) <*> (Parser right) =
    Parser $ \s -> [(f a, s2) | (f, s1) <- left s, (a, s2) <- right s1]

instance Monad Parse where
  return a = Parser $ \s -> [(a, s)]
  p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance Alternative Parse where
  empty = Parser $ const []
  p <|> q =
    Parser $ \s ->
      case parse p s of
        [] -> parse q s
        res -> res

ch :: (Char -> Bool) -> Parse Char
ch p = Parser $ \case
  (c : cs) | p c -> [(c, cs)]
  _              -> []

oneOf :: String -> Parse Char
oneOf s = ch (`elem` s)

whitespace :: Parse Char
whitespace = oneOf " \t\n"

digit :: Parse Char
digit = oneOf ['0' .. '9']


--- (4) ---

square :: Parse Int
square = digitToInt <$> (many whitespace *> digit)

emptyBoard :: Board Int
emptyBoard = pure 0

parseSquare :: Int -> Parse Int
parseSquare = const square

board :: Parse (Board Int)
board = traverse parseSquare emptyBoard


--- (5) ---

newtype K a x = K
  { unK :: a
  } deriving (Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance Monoid a => Applicative (K a) where
  pure _ = K mempty
  (K a) <*> (K b) = K $ mappend a b

crush :: (Traversable f, Monoid b) => (a -> b) -> f a -> b
crush f = unK . traverse (K . f)

newtype Any = Any
  { unAny :: Bool
  } deriving (Show)

newtype All = All
  { unAll :: Bool
  } deriving (Show)

instance Semigroup Any where
  (Any x) <> (Any y) = Any $ x || y

instance Monoid Any where
  mempty = Any False
  mappend = (<>)

instance Semigroup All where
  (All x) <> (All y) = All $ x && y

instance Monoid All where
  mempty = All True
  mappend = (<>)

-- avoid name conflicts with imports

anyany :: Traversable f => (a -> Bool) -> f a -> Bool
anyany p = unAny . crush (Any . p)

allall :: Traversable f => (a -> Bool) -> f a -> Bool
allall p = unAll . crush (All . p)

anyelem :: (Traversable f, Eq a) => a -> f a -> Bool
anyelem x = anyany (== x)


--- (6) ---

-- TODO :: optimize to not add already seen items to seen
duplicates :: (Traversable f, Eq a) => f a -> [a]
duplicates =
  snd
    . foldr
        (\item (seen, dups) ->
          ( item : seen
          , if anyelem item seen && not (anyelem item dups)
            then item : dups
            else dups
          )
        )
        mempty


--- (7) ---

complete :: Board Int -> Bool
complete = allall (`anyelem` [1 .. 9])

ok :: Board Int -> Bool
ok b = allall (\f -> null $ duplicates $ f b) [rows, columns, boxes]

bd :: Board Int
bd = fst $ head $ parse board $ unlines
  [ "050060001"
  , "004800070"
  , "800000052"
  , "200057030"
  , "000000000"
  , "030690005"
  , "790000008"
  , "010006500"
  , "500030060"
  ]

