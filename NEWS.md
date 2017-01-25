# 0.2.8

* Improve Stan models for shorter run times
* Add `dgmrp()` for fitting single-issue MRP models with hierarchical covariates
* Add class `dgmrp_fit` for models fitted with `dgmrp()`, inheriting from a new
  virtual class `dgo_fit` 
* `dgirt()` now returns a `dgirt_fit`-class object that also inherits from
  `dgo_fit` class
* Bugfixes

# 0.2.7

* Package renamed dgo: Dynamic Estimation of Group-level Opinion
* Tweaks to pass CRAN checks: clean up examples and docs
* Use roxygen2 for classes, methods, and `NAMESPACE`
* Fix checks on `P`, `S` related to `group_names` change in 0.2.5
* Fix Rcpp module issue from 0.2.6 (`Error in .doLoadActions(where, attach)`)
* Export `expand_rownames()`
 
# 0.2.6

* Fix error in `dgirt_plot`
* Fix path in `tools/make_cpp.R`

# 0.2.5

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
