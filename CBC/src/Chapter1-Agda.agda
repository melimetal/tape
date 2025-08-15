module CBC.src.Chapter1-Agda where

open import Agda.Primitive renaming (Set to Type ; Setω to Typeω)

module Booleans where
  data Bool : Type where
    false : Bool
    true : Bool

open Booleans
not : Bool → Bool
not true  = false
not false = true

_∨_ : Bool → Bool → Bool
false ∨ other = other
true  ∨ other = true

-- Exercise : Write the analogous function _∧_ (AND).
_∧_ : Bool → Bool → Bool
true  ∧ other = other 
false ∧ other = false 
