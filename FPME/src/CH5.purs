module CH5 where

import Prelude (Unit, (+), (==), (/=), (<), (>) ,(>=), discard, negate, otherwise, show)

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

-- NOTE: Constant function not value.
const ∷ ∀ a b. a → b → a
const x _ = x

-- Silly but useful function
flip ∷ ∀ a b c. (a → b → c) → b → a → c
flip f x y = f y x

-- Defining operators with precedence!
apply ∷ ∀ a b. (a → b) → a → b
apply f x = f x

infixr 0 apply as $

applyFlipped ∷ ∀ a b. a → (a → b) → b
applyFlipped = flip apply

infixl 1 applyFlipped as #

-- Lists to study recursive functions
singleton ∷ ∀ a. a → List a
singleton x = x : Nil

-- Check for empty values (alt. end of the list)
null ∷ ∀ a. List a → Boolean
null Nil = true
null _ = false

-- Does the inverse of cons (adds to the end of List)
snoc ∷ ∀ a. List a → a → List a
snoc Nil x = singleton x
snoc (x : xs) y = x : snoc xs y

-- Self-explanatory
length ∷ ∀ a. List a → Int
length Nil = 0
length (_ : xs) = length xs + 1

-- Tail recursive version
length' ∷ ∀ a. List a → Int
length' l = go 0 l
  where
  go ∷ Int → List a → Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

-- Maybe adds type safety even if it "takes" more "work"
head ∷ ∀ a. List a → Maybe a
head Nil = Nothing
head (x : _) = Just x

tail ∷ ∀ a. List a → Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last ∷ ∀ a. List a → Maybe a
last Nil = Nothing
last (_ : xs) = if length xs == 1 then head xs else last xs

last' ∷ ∀ a. List a → Maybe a
last' Nil = Nothing
last' (x : Nil) = Just x
last' (_ : xs) = last' xs

init ∷ ∀ a. List a → Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go ∷ List a → List a
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons ∷ ∀ a. List a  → Maybe { head ∷ a, tail ∷ List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index ∷ ∀ a. List a → Int → Maybe a
index Nil _ = Nothing
index l v = go l 0
  where
  go ∷ List a → Int → Maybe a 
  go Nil _ = Nothing
  go (x : xs) vi
    | v < 0 = Nothing
    | vi == v = Just x
    | otherwise = go xs (vi + 1)

infixl 8 index as !!

findIndex ∷ ∀ a. (a → Boolean) → List a → Maybe Int
findIndex _ Nil = Nothing
findIndex pred l = go 0 l
  where
  go ∷ Int → List a → Maybe Int 
  go _ Nil = Nothing
  go v (x : xs) = if pred x then Just v else go (v + 1) xs

findLastIndex ∷ ∀ a. (a → Boolean) → List a → Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l = ?what

test ∷ Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : Nil
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ length' $ 1 : 2 : 3 : Nil
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show (tail Nil :: (Maybe (List Unit)))
  log $ show $ tail ("abc" : "123" : "sfdsf" : Nil)
  log $ show (last Nil :: (Maybe Unit))
  log $ show $ last ("first" : "second" : "third" : Nil)
  log $ show $ last' ("first" : "second" : "third" : "fourth" : Nil)
  log $ show $ init (Nil :: (List (Maybe Unit)))
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-1)
  (1 : 2 : 3 : Nil) !! 2 # show # log
  findIndex (_ > 2) (1 : 2 : 3 : 4 : Nil) # show # log  
  findIndex (_ >= 99) (1 : 2 : 3 : Nil) # show # log  
  findIndex (10 /= _) (Nil :: List Int) # show # log  
  -- findLastIndex (_ == 10) (Nil :: List Int) # show # log
  -- findLastIndex (_ == 10) (10 : 5 : -1 : 2 : 10 : Nil) # show # log
  -- findLastIndex (_ == 10) (11 : 12 : Nil) # show # log 
