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
require sift from "../../util/sift"
require raster from "../../graphics/raster"
require crucible from "../../testing/crucible"

@[default_target]
lean_lib Tracker where
  roots := #[`Tracker]

lean_exe tracker where
  root := `Main

@[test_driver]
lean_exe tests where
  root := `TrackerTests.Main
