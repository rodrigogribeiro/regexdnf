module RegexDNF where

data Regex
  = Empty
  | Eps
  | Chr Char
  | Regex :@: Regex
  | Regex :+: Regex
  | Star Regex
  deriving (Eq, Ord, Show)

data StarNF
  = StarEmpty
  | StarEps
  | StarChr Char
  | StarCat StarNF StarNF
  | StarStar StarNF
  deriving (Eq, Ord, Show)

type DNF = [StarNF]

convert :: Regex -> DNF
convert Empty = [StarEmpty]
convert Eps   = [StarEps]
convert (Chr c) = [StarChr c]
convert (e :@: e')
  = 
