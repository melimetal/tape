module CBC.src.Chapter1-Agda where

module Booleans where
  data Bool : Set where
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
