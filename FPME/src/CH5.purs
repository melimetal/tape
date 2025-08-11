module CH5 where

import Prelude (Unit, show, discard, (+))

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
