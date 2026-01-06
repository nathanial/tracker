import Lake
open Lake DSL

package tracker where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]
  precompileModules := true

require terminus from git "https://github.com/nathanial/terminus" @ "v0.0.1"
require parlance from git "https://github.com/nathanial/parlance" @ "v0.0.1"
require chronos from git "https://github.com/nathanial/chronos-lean" @ "v0.0.1"

@[default_target]
lean_lib Tracker where
  roots := #[`Tracker]

lean_exe tracker where
  root := `Main
