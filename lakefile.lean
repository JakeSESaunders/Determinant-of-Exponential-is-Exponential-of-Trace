import Lake
open Lake DSL

package «det_exp_eq_exp_tr» {
  -- add any package configuration options here
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

@[default_target]
lean_lib «DetExpEqExpTr» {
  -- add any library configuration options here
}
