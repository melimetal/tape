module CH5 where

import Prelude (Unit, (+), (-), (==), (/=), (<), (>) ,(>=), (<<<), (>>>), discard, max, negate, otherwise, show, type (~>))

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
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
init l = Just $ go l
  where
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
findLastIndex pred l = go Nothing 0 l
  where
  go ∷ Maybe Int → Int → List a → Maybe Int 
  go fv _ Nil = fv
  go fv v (x : xs) = go (if pred x then Just v else fv) (v + 1) xs

reverse ∷ List ~> List
        -- ^^ Equivalent to: List a ~> List a
        -- NOTE: (~>): means Natural Transformation.
reverse Nil = Nil
reverse orig = go Nil orig
  where
  go ∷ ∀ a. List a → List a → List a
  go reversed Nil = reversed
  go reversed (x : xs) = go (x : reversed) xs

concat ∷ ∀ a. List (List a) → List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter ∷ ∀ a. (a → Boolean) → List a → List a
filter _ Nil = Nil
filter pred (x : xs) = filter'
  where
  recur = filter pred xs
  filter' = if pred x then (x : recur) else recur

-- case ... of from
-- https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/01-Basic-Syntax/src/04-Various-Keywords/03-Keywords--Case-expression-of-ps.html?highlight=case%20expression#03-keywords--case-expression-ofpurs

catMaybes ∷ ∀ a. List (Maybe a) → List a
catMaybes Nil = Nil
catMaybes (x : xs) =
  case x of
  Just y → y : catMaybes xs
  Nothing → catMaybes xs
  
range ∷ Int → Int → List Int
range start end = 
  let steps = if start < end then 1 else -1
  in
  if start == end then singleton start
  else start : range (start + steps) end  

take ∷ ∀ a. Int → List a → List a
take _ Nil = Nil
take 0 _ = Nil
take v (x : xs) = x : take (v - 1) xs

drop ∷ ∀ a. Int → List a → List a
drop _ Nil = Nil
drop 0 l = l 
drop v (_ : xs) = drop (v - 1) xs  

takeWhile ∷ ∀ a. (a → Boolean) → List a → List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

dropWhile ∷ ∀ a. (a → Boolean) → List a → List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd ∷ ∀ a. Int → List a → List a
takeEnd v = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c vl) → Tuple (c + 1) $ if c < v then x : vl else vl
 
dropEnd ∷ ∀ a. Int → List a → List a
dropEnd v = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c vl) → Tuple (c + 1) $ if c < v then vl else x : vl

zip ∷ ∀ a b. List a → List b → List (Tuple a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip ∷ ∀ a b. List (Tuple a b) → Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) = unzip ts # \(Tuple xs ys) → Tuple (x : xs) (y : ys) 

test ∷ Effect Unit
test = do
  flip const 1 2 # show # log
  flip const 1 2 # show # log
  singleton "xyz" # show # log
  null Nil # show # log
  null ("abc" : Nil) # show # log
  snoc (1 : 2 : Nil) 3 # show # log
  length (1 : Nil) # show # log
  length (1 : 2 : 3 : Nil) # show # log
  length' (1 : 2 : 3 : Nil) # show # log
  (head Nil ∷ Maybe Unit) # show # log
  head ("abc" : "123" : Nil) # show # log
  (tail Nil ∷ (Maybe (List Unit))) # show # log
  tail ("abc" : "123" : "sfdsf" : Nil) # show # log
  (last Nil ∷ (Maybe Unit)) # show # log
  last ("first" : "second" : "third" : Nil) # show # log
  last' ("first" : "second" : "third" : "fourth" : Nil) # show # log
  init (Nil ∷ (List (Maybe Unit))) # show # log
  init (1 : Nil) # show # log
  init (1 : 2 : Nil) # show # log
  init (1 : 2 : 3 : Nil) # show # log
  uncons (1 : 2 : 3 : Nil) # show # log
  index (1 : Nil) 4 # show # log
  index (1 : 2 : 3 : Nil) 1 # show # log
  index (Nil ∷ List Unit) 0 # show # log
  index (1 : 2 : 3 : Nil) (-1) # show # log
  (1 : 2 : 3 : Nil) !! 2 # show # log
  findIndex (_ > 2) (1 : 2 : 3 : 4 : Nil) # show # log  
  findIndex (_ >= 99) (1 : 2 : 3 : Nil) # show # log  
  findIndex (10 /= _) (Nil ∷ List Int) # show # log  
  findLastIndex (_ == 10) (Nil ∷ List Int) # show # log
  findLastIndex (_ == 10) (10 : 5 : -1 : 2 : 10 : Nil) # show # log
  findLastIndex (_ == 10) (11 : 12 : Nil) # show # log 
  reverse (1 : 2 : 3 : Nil) # show # log
  concat ((1 : 2 : 3 : Nil)
           : (4 : 5 : Nil)
           : (6 : Nil)
           : (Nil) : Nil)
           # show # log
  filter (4 > _) (1 : 2 : 3 : 4 : 5 : 6 : Nil) # show # log
  catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil) # show # log
  range 1 10   # show # log
  range 3 (-3) # show # log
  take 5 (range 12 14) # show # log
  take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 :  Nil) # show # log
  drop 10 (Nil ∷ List Unit) # show # log
  drop 2 (range 1 7) # show # log
  takeWhile (_ > 3) (range 1 3) # show # log
  takeWhile (_ == -17) (range 1 3) # show # log
  dropWhile (_ > 3) (range 5 8) # show # log
  dropWhile (_ == -17) (range 1 3) # show # log
  takeEnd 3 (range 1 6) # show # log
  takeEnd 10 (1 : Nil) # show # log
  dropEnd 3 (range 1 6) # show # log
  dropEnd 10 (1 : Nil) # show # log
  zip (range 1 3) ("a" : "b" : "c" : "d" : "e" : Nil) # show # log
  zip ("a" : "b" : "c" : "d" : "e" : Nil) (range 1 3) # show # log
  zip (Nil ∷ List Unit) (1 : 2 : Nil) # show # log
  unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil) # show # log
  unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil) # show # log
  unzip (Nil ∷ List (Tuple Unit Unit)) # show # log
