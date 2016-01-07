## 2015-01-06

  * Bugfix: mean item outcomes could be calculated incorrectly, inflating
    success counts toward trial counts.

## 2015-12-30

Version bump to 0.0.10.

  * Functionality:
    * Apply more descriptive names to `dgirt()` results using the variable names
      originally passed to `wrangle()` and the levels of factors.
    * `poststratify()` is safer and more flexible. It takes new arguments
      `strata`, `groups`, and `check_proportions`; see the documentation.
    * Specify the algorithm for CmdStan to use. `dgirt()` passes new argument
      `optimize_algorithm` to CmdStan if `method = "optimize"`; one of
      `"lbfgs"` (the default), `"bfgs"` and `"newton"`.
  * Documentation:
    * Switch to a README.Rmd that includes the "Getting Started" vignette
      content and drop the vignette.
  * Functions renamed:
    * `run_dgirt()` -> `dgirt()`
    * `format_dgirt()` -> `wrangle()`
  * New and renamed datasets:
    * `rstan_output`: example of `dgirt()` output for `method = "rstan"`
    * `optimize_output`: example of `dgirt()` output for `method = "optimize"`
    * `states` -> `state_opinion`
    * `state_targets` and `targets` -> `state_demographics`
  * Under the hood:
    * Switch to [assertthat](https://github.com/hadley/assertthat) package from
    ad-hoc `stop()` calls
    * Speed up `wrangle()`
    * Bugfixes
