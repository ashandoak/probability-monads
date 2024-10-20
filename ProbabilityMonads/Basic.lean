/- 
\ This code based on the tutorial found at https://dennybritz.com/posts/probability-monads-from-scratch/
-/

import Std.Data.HashMap

open Std

set_option diagnostics true

-- Use Float as a representation for probabilities 
-- def Prob : Type := Float didn't work
-- abbrev is always unfoled
abbrev Prob := Float 

-- A distribution is a list of possible values and their probabilities
-- TODO Is this better as an inductive?
structure Dist (α : Type) where
  data : List (α × Prob) 

-- Helper function to access the inner list wrapped by the distribution
def unpackDist (d : Dist α) : List (α × Prob) := d.data

-- Helped function to collapse outcomes that occur multiple times
def squishD [Ord α] (d : Dist α) : Dist α := Dist $ HashMap.toList $ Lean.HashMap.ofListWith (· + ·) d.data


#check HashMap.toList

open HashMap
def ofListWith {α : Type} [BEq α] [Hashable α] (l : List (α × β)) (f : β → β → β) : HashMap α β :=
  l.foldl (init := HashMap.empty)
    (fun m p =>
      match m.find? p.fst with
        | none   => m.insert p.fst p.snd
        | some v => m.insert p.fst $ f v p.snd)
