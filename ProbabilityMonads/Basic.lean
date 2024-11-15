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


-- An Event maps an outcome to a truth value
abbrev Event α := α → Bool

-- Evaluate the probability of the given Event
-- I guess the intention is to use this in a compositional way, so that the user will pass in an event and a distribution
def evalD (p : Event α) (d : Dist α) : Prob := 
  sumP $ (unpackDist d).filter (fun x => p x.fst) 

-- Create a uniform distribution
def uniform (l : List α) : Dist α :=
  Dist.mk $ normP $ l.map (fun x => (x, 1.0)) 

-- A fair n-sided die
def die (n : Nat) : Dist Nat := uniform $ List.range' 1 n 

/-
-- A coin that lands on x with probability f and y with probability 1-f
def coin (f : Prob) (x y : α) : Dist a :=
  match f with
  | f < 0.0 || f > 1.0 => error "f must be between 0 and 1"
  | _ => Dist [(x, f), (y, 1 - f)]
-/
def isEven (n : Nat) : Bool :=
  n % 2 == 0

#eval die 6
#eval die 5
#eval evalD isEven $ die 5
