module CH5 where

import Prelude (Unit, show, discard)

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

-- NOTE: Constant function not value.
const :: forall a b. a -> b -> a
const x _ = x

-- Silly but useful function
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- Defining operators with precedence!
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x 

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

-- Lists to study recursive functions
singleton :: forall a. a -> List a
singleton x = x : Nil

-- Check for empty values (alt. end of the list)
null :: forall a. List a -> Boolean
null Nil = true
null _   = false

-- Does the inverse of cons (adds to the end of List)
snoc :: forall a. List a -> a -> List a
snoc Nil x        = singleton x
snoc (x : xs) y   = x : snoc xs y 

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
