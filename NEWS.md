# dgirt 0.2.5

* `group_names` is no longer required. If omitted, only the geographic variable
  given by `geo_name` will define groups.
* `raking` argument of `shape()` replaces `strata_names`. It takes a formula or
  list of formulas and allows more complicated preweighting.
* New: `rhats` and `plot_rhats` for model checking.
