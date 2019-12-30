{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S
import Data.Char

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State f) start = snd $ f start

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State f) start = fst $ f start

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State $ \x -> (x, x)

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State (\_ -> ((), s))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f g = State $ (\(a,s) -> (f a,s)) <$> (runState g)

instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure x = State (\s -> (x, s))
  (<*>) ::
    State s (a -> b)  -- s -> ((a -> b), s)
    -> State s a      -- s -> (a, s)
    -> State s b      -- s -> (b, s)
  (<*>) (State f) (State g) = State $ \s -> let (r, s') = f s
                                                go f' (a, c) = (f' a, c)
                                            in go r <$> g $ s'



-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  -- (=<<) k (State g) = State h -- h :: s -> (b, s)
  --                              -- g :: s -> (a, s)
  --                              -- g s :: (a, s)
  --                              -- f :: a -> State s b
  --                              -- f $ fst $ g s :: State s b
  --                              -- h s :: (b, s)
  --                              -- runState (f $ fst $ g s) s :: (s, b)
  --                              where h s = runState (k x) y
  --                                          where (x, y) = g s
  k =<< p = State $ \ s0 ->
     let (x, s1) = runState p s0  -- Running the first processor on s0.
     in runState (k x) s1         -- Running the second processor on s1.

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
-- foldr :: Foldable t => (a -> f (Optional a) -> f (Optional a)) -> f (Optional a) -> t a -> f (Optional a)
-- reduceEffects :: f (Optional a) -> a -> f (Optional a)
-- next :: Optional a -> Bool -> Optional a -> Optional a
-- lift2 :: (Bool -> Optional a -> Optional a) -> f Bool -> f (Optional a) -> f (Optional a)
-- lift :: a -> f Bool -> f a -> f (f Bool)
-- _h :: Bool -> Optional a -> Optional a
-- (p a) :: f Bool
-- bool :: a -> a -> Bool -> a
-- next :: a -> Bool -> a -> a
-- 从 next 到 _h
-- next :: Optional a -> Bool -> Optional a -> Optional a
-- next (some Optional a) :: Bool -> Optional a -> Optional a
-- liftA2 f x y = f <$> x <*> y

-- work but can not deal with infinit list
-- findM pred = foldLeft reducer (pure Empty)
--              where reducer prev curr = join $ next <$> prev
--                                        where next (Full x) = pure $ const (Full x) curr
--                                              next Empty = (\tOrf -> if tOrf then Full curr else Empty) <$> pred curr
-- findM = error "not implemented"

findM _ Nil = pure Empty
findM pred (x :. y) = do
      b <- pred x
      if b then return $ Full x else findM pred y

-- (<|>) :: Optional a -> Optional a -> Optional a
-- (<|>) (Full x) = const $ Full x
-- (<|>) Empty = id
-- g1 :: Bool -> a -> Optional a
-- g1 = bool (const Empty) pure
-- this version also not work for infinite list
-- findM pred = foldLeft reducer (pure Empty)
--     where reducer prev curr = lift2 (<|>) prev $ (flip g1 curr) <$> pred curr

--   (=<<) ::    (a -> Optional b)    -> Optional a    -> Optional b
-- (=<<) :: (a -> f Bool) -> f a -> f Bool
-- flip ((=<<) pred) :: f Bool -> f a
-- (*>) ::   f Bool  -> f a  -> f a

-- g :: Bool -> a -> Optional a
-- g = bool (const Empty) pure
-- (flip g a) <$> (pred a) :: f (Optional a)

-- findM pred = foldLeft reducer (pure Empty)
--       where reducer prev curr = (\p -> case p of
--                                          Full x -> const (Full x)
--                                          Empty  -> flip g curr) <$> prev <*> (pred curr)
--             g = bool (const Empty) pure

-- findM p = foldLeft reduce (pure Empty)
--           where reduce x a = lift2 h t _x

-- =<< pure


-- reduce a x = if a is Full _ then Full _ else if (p x) then Full x Else Empty

-- (<=<) ::  Monad f =>  (b -> f Bool)  -> (a -> f b)  -> a  -> f Bool

-- (>>=) ::  Monad f =>  f a -> (a -> f Bool) -> f Bool

-- lift2 ::  Applicative f =>  (a -> b -> c)  -> f a  -> f b  -> f c

-- lift3 ::  Applicative f =>  (a -> b -> c -> d)  -> f a  -> f b  -> f c  -> f d

--                h b a c = bool a c b
-- _h :: Bool -> Optional a -> Optional a
-- _h :: b -> a -> c
-- _h : if a is Full _ then Full _ else if b is True then Full ? else Empty
--              _h b o = bool const ?? if b then
-- (??) :: Optional a -> a -> a
-- (<*)  f b  -> f a  -> f b
-- _h1 :: Optional a
-- _h b o = o ??
--                          next = bool id . (??)
-- reduceEffects :: f (Optional a) -> a -> f (Optional a)

-- findM pred la = error "todo: Course.State#findM"

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat la = eval (findM pred la) S.empty
-- member :: Ord a => a -> Set a -> Bool
-- pred :: a -> State (S.Set a0) Bool
    where pred a = State $ \s -> (S.member a s, S.insert a s)

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct la = eval (filtering pred la) S.empty
  where pred a = State $ \s -> (not $ S.member a s, S.insert a s)

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
-- produce :: (a -> a) -> a -> List a
-- firstRepeat :: Ord a => List a -> Optional a
-- join :: Monad f => f (f a) -> f a
-- contains :: Eq a => a -> Optional a -> Bool

-- isHappy = contains 1 . firstRepeat . produce sumSquareDigits . fromInteger
--        where square = join (*)
--              sumSquareDigits = sum . map (square . digitToInt) . reverse . toRevListChar
--              toRevListChar n = if n < 10
--                                     then intToDigit n :. Nil
--                                     else let (q, r) = quotRem n 10 in intToDigit r :. toRevListChar q

square :: Int -> Int
square = join (*)
sumSquareDigits :: Int -> Int
sumSquareDigits = sum . map (square . digitToInt) . reverse . toRevListChar
toRevListChar :: Int -> List Char
toRevListChar n = if n < 10
                  then intToDigit n :. Nil
                  else let (q, r) = quotRem n 10 in intToDigit r :. toRevListChar q
isHappy = contains 1 . firstRepeat . produce sumSquareDigits . fromInteger
-- isHappy = error "1"
