/- 
\ This code based on the tutorial found at https://dennybritz.com/posts/probability-monads-from-scratch/
-/
import Batteries

open Std

set_option diagnostics true
set_option diagnostics.threshold 48
-- Use Float as a representation for probabilities 
-- def Prob : Type := Float didn't work
-- abbrev is always unfolded
abbrev Prob := Float 

-- A distribution is a list of possible values and their probabilities
-- TODO Is this better as an inductive?
structure Dist (α : Type) where
  data : List (α × Prob)
deriving Repr

-- Examples
def data  := [("a", 0.2), ("b", 0.2), ("c", 0.1), ("a", 0.1), ("c", 0.4)]
def dist := Dist.mk data
#check dist

-- Helper function to access the inner list wrapped by the distribution
def unpackDist (d : Dist α) : List (α × Prob) := d.data

-- Examples
#eval unpackDist dist
#check unpackDist dist

-- Helper function to collapse outcomes that occur multiple times
def squishD [Ord α] [BEq α] [Hashable α] (d : Dist α) : Dist α := Dist.mk $ Batteries.HashMap.toList $ (Batteries.HashMap.ofListWith d.data (· + ·))

def squishD' [BEq α] [Hashable α] (d : Dist α) : Dist α :=
  Dist.mk $ HashMap.toList $ List.foldl (fun acc (k, v) =>
    acc.insert k (v + acc.getD k 0)) HashMap.empty $ unpackDist d 

-- Examples
#eval squishD dist
#eval squishD' dist


-- Helper function to sum all probabilities in a list
def sumP (l : List (α × Prob)) : Prob := l.map Prod.snd |>.foldr (· + ·) 0

-- Helper function to normalize probabilities to 1.0
def normP (l : List (α × Prob)) : List (α × Prob) := 
  let q := sumP l
  l.map (fun (a, p) => (a, p/q))

