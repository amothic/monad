import Control.Monad
import Data.Maybe
{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 11:58:21 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 2 - Do notation

Usage: Compile the code and execute the resulting program.
       It will print Dolly's maternal grandfather.
-}

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- the Maybe type is already declared as an instance of the Monad class
-- in the standard prelude, so we don't actually need to define it here.
-- just remember that it looks something like this:
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just

-- we can use do-notation to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do m <- mother s
                           father m
                            
maternalGrandfather' :: Sheep -> Maybe Sheep
maternalGrandfather' s = mother s >>= father 

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
                                  mother gm

fathersMaternalGrandmother' :: Sheep -> Maybe Sheep
fathersMaternalGrandmother' s = father s >>= \f -> mother f >>= \gm -> mother gm

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
                                  father gf

mothersPaternalGrandfather' :: Sheep -> Maybe Sheep
mothersPaternalGrandfather' s = mother s >>= \m -> father m >>= \gf -> father gf

parent :: Sheep -> Maybe Sheep
parent s = (father s) `mplus` (mother s)

grandparent :: Sheep -> Maybe Sheep
grandparent s = (mother s >>= parent) `mplus` (father s >>= parent )

parentlist :: Sheep -> [Sheep]
parentlist s = (maybeToList $ father s) `mplus` (maybeToList $ mother s)

grandparentlist :: Sheep -> [Sheep]
grandparentlist s = (maybeToList (mother s) >>= parentlist) `mplus` (maybeToList (father s) >>= parentlist)

grandparentlist2 :: Sheep -> [Sheep]
grandparentlist2 s = parentlist s >>= parentlist

maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad Nothing  = mzero
maybeToMonad (Just a) = return a

mparent :: (MonadPlus m) => Sheep -> m Sheep
mparent s = (maybeToMonad $ father s) `mplus` (maybeToMonad $ mother s)

mgrandparent :: (MonadPlus m) => Sheep -> m Sheep
mgrandparent s = (maybeToMonad (mother s) >>= mparent) `mplus` (maybeToMonad (father s) >>= mparent )

-- this builds our sheep family tree
breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing Nothing
                 uranus = Sheep "Uranus" Nothing Nothing
                 gaea   = Sheep "Gaea" Nothing Nothing
                 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
                 roger  = Sheep "Roger" (Just eve) (Just kronos)
                 molly  = Sheep "Molly" (Just holly) (Just roger)
             in Sheep "Dolly" (Just molly) Nothing

-- print Dolly's maternal grandfather
main :: IO ()
main = let dolly = breedSheep
       in do print (maternalGrandfather dolly)
             print (mparent dolly :: Maybe Sheep) 
             print (mparent dolly :: [Sheep]) 
             print (grandparent dolly) 
             print (grandparentlist dolly) 
             print (mgrandparent dolly :: Maybe Sheep) 
             print (mgrandparent dolly :: [Sheep]) 
  
-- END OF FILE
