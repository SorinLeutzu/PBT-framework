module Assertions
(Matcher(..),EqMatcher(..),GtMatcher(..),ContainsMatcher(..),
And(..),Or(..),Not(..),
eq,gt,contains,
assertThat, expectThat) where



--Checks if a string contains a particular string
containsString _ [] = False
containsString string text@(t:ts) = if (startsWithString string text) then True else (containsString string ts)

--Checks if a string starts with a particular string
startsWithString [] _ = True
startsWithString _ [] = False
startsWithString string@(s:ss) text@(t:ts) | t==s = startsWithString ss ts
                                           | t/=s = False

-- Mathcer typeclass
class Matcher m a where
    match:: m->a-> Either String()

--Basic matchers:

--Equality matcher
newtype EqMatcher a = EqMatcher a
instance (Eq a,Show a) => Matcher (EqMatcher a) a where
    match (EqMatcher exp) act 
     | exp == act = Right()
     | otherwise = Left $ "Expected  "++ show exp++ "but got "++show act

--Comparison Mathcer
newtype GtMatcher a = GtMatcher a
instance (Ord a,Show a) => Matcher (GtMatcher a) a where
    match (GtMatcher higher) lower
     |higher>lower = Right()
     |otherwise = Left $ show higher ++ "is not greater than"++show lower


--Contains matcher
newtype ContainsMatcher = ContainsMatcher String
instance Matcher ContainsMatcher String where
    match (ContainsMatcher string) text
     | containsString string text= Right()
     | otherwise = Left $ "String "++ show text ++" does not contain string " ++show string

eq = EqMatcher
gt = GtMatcher
contains = ContainsMatcher


data And m1 m2 a where
    And::(Matcher m1 a,Matcher m2 a) => m1->m2->And m1 m2 a
instance (Matcher m1 a,Matcher m2 a) => Matcher (And m1 m2 a) a where
    match (And m1 m2) x = do
        match m1 x
        match m2 x

data Or m1 m2 a where
    Or::(Matcher m1 a, Matcher m2 a) => m1->m2-> Or m1 m2 a
instance (Matcher m1 a,Matcher m2 a) => Matcher (Or m1 m2 a) a where
    match (Or m1 m2) x=
        case match m1 x of
            Right() -> Right()
            Left e1 -> case match m2 x of
                Right() -> Right()
                Left e2 -> Left(e1++" or  "++e2)

data Not m a where
    Not :: Matcher m a=> m-> Not m a
instance Matcher m a => Matcher (Not m a) a where
    match (Not m) x = case match m x of
        Right() -> Left $ "Negation failed"
        Left _ -> Right()

               


assertThat :: Matcher m a=> m->a->IO()
assertThat m x = case match m x of
    Right() -> return()
    Left err ->  error("Assertion failed " ++ err)

expectThat :: Matcher m a => m->a->IO (Either String())
expectThat m x = return(match m x)
