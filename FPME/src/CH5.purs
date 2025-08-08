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

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
