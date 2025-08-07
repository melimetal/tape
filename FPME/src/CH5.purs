module CH5 where

import Prelude (Unit, show)
import Effect (Effect)
import Effect.Console (log)

-- Defining operators with precedence!
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x 

infixr 0 apply as $

-- NOTE: Constant function not value.
const :: forall a b. a -> b -> a
const x _ = x

-- Silly but useful function
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

test :: Effect Unit
test = do
  log $ show $ flip const 1 2

