{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Regex where

import Control.Monad
import Data.List
import Test.QuickCheck

data Regex
  = Empty
  | Lam
  | Chr Char
  | Regex :@: Regex
  | Regex :+: Regex
  | Star Regex
  deriving (Eq, Ord, Show)

class Accept a where
  accept :: a -> String -> Bool

instance Accept Regex where
  accept Empty _
    = False
  accept Lam s
    = null s
  accept (Chr c) s
    = s == [ c ]
  accept (e :@: e') s
    = any (\(x,y) -> accept e x && accept e' y) xs
    where
      xs = [(x,drop (length x) s) | x <- inits s]
  accept (e :+: e') s
    = accept e s || accept e' s
  accept (Star (Star e)) s
    = null s || accept (e :@: (Star e)) s
  accept (Star e) s
    = null s || accept (e :@: (Star e)) s

-- star normal form

data StarNF
  = SEmpty
  | SLam
  | SChr Char
  | StarNF :@@: StarNF
  | SStar StarNF
  deriving (Eq, Ord, Show)

instance Accept StarNF where
  accept SEmpty _
    = False
  accept SLam s
    = null s
  accept (SChr c) s
    = s == [ c ]
  accept (e :@@: e') s
    = any (\(x,y) -> accept e x && accept e' y) xs
      where
         xs = [(x,drop (length x) s) | x <- inits s]
  accept (SStar (SStar e)) s
    = null s || accept (e :@@: (SStar e)) s 
  accept (SStar e) s
     = null s || accept (e :@@: (SStar e)) s

-- disjuntive normal form

type DNF = [StarNF]

instance Accept DNF where
  accept dnf s = any (flip accept s) dnf

-- conversion algorithm

convert :: Regex -> DNF
convert Empty
  = [ SEmpty ]
convert Lam
  = [ SLam ]
convert (Chr c)
  = [ SChr c ]
convert (e :@: e')
  = [s :@@: s' | s <- convert e, s' <- convert e']
convert (e :+: e')
  = convert e ++ convert e'
convert (Star Lam)
  = [ SLam ]
convert (Star (Star e))
  = map SStar (convert e)
convert (Star e)
  = map SStar (convert e)

-- correctness properties

--1) generator of strings is correct

genStringOk :: Regex -> Property
genStringOk e = forAll (genStringOf e) (\s -> accept e s)

-- 2) correctness of conversion

convertOk :: Regex -> Property
convertOk e = forAll (genStringOf e) (\ s -> accept e s ==> accept (convert e) s)

-- quickcheck arbitrary instances


sizedRegex :: Int -> Gen Regex
sizedRegex n
  | n <= 1
     = frequency
          [
            (5, return Lam)
          , (20, Chr <$> oneof (map return ['a'.. 'z']))
          ]
  | otherwise
     = frequency
          [
            (60, liftM2 (:@:) (sizedRegex n2) (sizedRegex n2))
          , (40, liftM2 (:+:) (sizedRegex n2) (sizedRegex n2))
          , (30, liftM star (sizedRegex n1))
          ]
       where
         n2 = n `div` 2
         n1 = n - 1
         -- smart constructor for optimize star operator
         star (Star (Star e)) = e
         star (Star Lam) = Lam
         star c    = c

genStringOf :: Regex -> Gen String
genStringOf Empty
  = return [] -- will not happen
genStringOf Lam
  = return []
genStringOf (Chr c)
  = return [c]
genStringOf (e :@: e')
  = liftM2 (++) (genStringOf e) (genStringOf e')
genStringOf (e :+: e')
  = do
       m1 <- genStringOf e
       m2 <- genStringOf e'
       elements [m1,m2]
genStringOf (Star e)
  = do
      s <- genStringOf e
      elements [[] , s]

instance Arbitrary Regex where
  arbitrary
    = choose (1,4) >>= sizedRegex

test :: Regex
test = Star ((Star (Chr 'z') :+: (Chr 'd' :@: Chr 's')))
