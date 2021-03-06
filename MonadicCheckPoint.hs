{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
undefined = undefined

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--Laws:
--    forall f :: (a -> b), pac1 :: (a -> f a), pac2 :: (b -> f b) .
--        fmap f . pac1 == pac2 . f
-- Law of the commuting square:
--       f
-- a  -------> b
-- |           |
-- | p         | p
-- | a         | a
-- | c         | c
-- | 1         | 2
-- V   fmap f  V
-- F a -----> F b

{-
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

--Laws:
--    fmap == (<*>) . pure

--------------------------------------
-- or else:

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

{- -- This is a nice pattern to comment out/uncomment something.
instance Applicative f => Functor f where -- This means
    fmap = (<*>) . pure                   -- I can deduce `Functor f` if
                                          -- given `Applicative f`.
-}
--------------------------------------

class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    id = \x -> x
    (.) f g x = f (g x)

--Laws:
--    ?

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
a >>> b = b . a

class Monad1 m where
    return1 :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad1 m => m x -> m y -> m y
-- a :: m x
-- (\_ -> b) :: (x -> m y)
a >> b = a >>= (\_ -> b)

class Functor m => Monad2 m where -- This means `fmap` is avaible
    return2 :: a -> m a            -- in `Monad2` too.
    join :: m (m a) -> m a

class Monad3 m where
    return3 :: a -> m a
    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

--Laws:
--  x >=> y = join . (fmap y) x

-- Show the relation between Monad3 and Category:
--   ?

-- Instances from Monad1 to anything are allowed to use do-syntax in
-- right-hand side of the equations.

instance Monad1 m => Monad2 m where
    return2 = return1

    join mma = mma >>= id

instance Monad1 m => Functor m where
    fmap f ma = ma >>= (return1 . f)

instance Monad1 m => Applicative m where
    pure = return1

    mf <*> ma = do
                  f <- mf
		  a <- ma
		  return1 (f a)
--  mf <*> ma = mf >>=
--              \f -> (ma >>= (\a -> return1 (f a)))

instance Monad2 m => Monad1 m where
    return1 = return2
    ma >>= f = join (fmap f ma)

instance Monad3 m => Monad1 m where
    return1 = return3

    ma >>= f = undefined

instance Monad1 m => Monad3 m where
    return3 = return1

    f >=> g = \x -> do
                      y <- f x
		      g y
--            (\x -> ((f x) >>= (\y -> g y)))
