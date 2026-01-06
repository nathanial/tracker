import Lake
open Lake DSL

package tracker where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]
  precompileModules := true

require terminus from "../../graphics/terminus"
require parlance from "../../util/parlance"
require chronos from "../../util/chronos"

@[default_target]
lean_lib Tracker where
  roots := #[`Tracker]

lean_exe tracker where
  root := `Main
