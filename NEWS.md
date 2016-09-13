# dgirt 0.2.5

* `group_names` is no longer required. If omitted, the geographic variable given
  by `geo_name` will define groups.
* `aggregate_item_names` is no longer required. It defaults to the observed
  values of the `item` column in `aggregate_data`.
* `raking` argument to `shape()` replaces `strata_names`. It takes a formula or
  list of formulas and allows more complicated preweighting.
* `id_vars` argument to `shape()` specifies variables to be kept in `item_data`.
* `aggregate_data` may include geographic areas, demographics, or time periods
  that don't appear in `item_data`.
* Fix: use a smaller epsilon than the default in survey::rake() for convergence
  with non-frequency weights.
* New `dgirtfit` methods `rhats()` and `plot_rhats()` for model checking.
* New `dgirtfit` method `get_time_elapsed` gives model run times. These also
  appear in `summary` output.
