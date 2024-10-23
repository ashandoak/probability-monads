import Lake
open Lake DSL

require batteries from git "https://github.com/leanprover-community/batteries" @ "main"

package "probability-monads" where
  -- add package configuration options here

lean_lib «ProbabilityMonads» where
  -- add library configuration options here

@[default_target]
lean_exe "probability-monads" where
  root := `Main
