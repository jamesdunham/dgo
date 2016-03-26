setClass("ItemVar", contains = "character")
setMethod("initialize", "ItemVar", function(.Object, .Data) {
  if (!missing(.Data)) {
    if (!all(nchar(.Data) > 0)) stop("variable name is not a positive-length character")
    if (!length(.Data) > 0) stop("variable name is NULL")
    if (any(duplicated(.Data))) stop("duplicate variable name")
    .Object@.Data <- .Data
  }
  .Object
})

setClass("TimeFilter", contains = "numeric")
setMethod("initialize", "TimeFilter", function(.Object, .Data) {
  if (!missing(.Data)) {
    if (!inherits(.Data, "integer")) {
      warning("item_filter times should probably be integers")
      if (!inherits(.Data, "numeric")) {
        stop("item_filter times should be numeric")
      }
    }
    if (!length(.Data) > 0) stop("tried to set item_filter times and found NULL")
    if (any(duplicated(.Data))) warning("item_filter times are not unique")
    .Object@.Data <- .Data
  }
  .Object
})

setClass("GeoFilter", contains = "character")
setMethod("initialize", "GeoFilter", function(.Object, .Data) {
  if (!missing(.Data)) {
    if (!all(nchar(.Data) > 0)) stop("geo filter is not a positive-length character")
    if (!length(.Data) > 0)     stop("geo filter is NULL")
    if (any(duplicated(.Data))) warning("item_filter geo are not unique")
    .Object@.Data <- .Data
  }
  .Object
})

Target <- R6::R6Class("Target", cloneable = FALSE, 
  private = list(prop_ = NULL, strata_ = NULL, tbl_ = NULL),
  public = list(weight = NULL))

Target$set("public", "print", function(...) {
  cat("<Target>:", fill = TRUE)
  if (!length(self$tbl) > 0) {
    cat("\tNo data", fill = TRUE)
  } else {
  cat("\ttarget data (", nrow(self$tbl), " x ", ncol(self$tbl), ")", sep = "", fill = TRUE)
  cat("\tstrata variables: ", self$strata, fill = TRUE)
  cat("\tpopulation proportion variable: ", self$prop, fill = TRUE)
  }
})
Target$set("public", "test_names", test_names)
Target$set("active", "strata",     gen_bind(strata_, check_names = TRUE))
Target$set("active", "prop",       gen_bind(prop_, check_names = TRUE))
Target$set("active", "tbl",        bind_tbl)

Filter <- R6::R6Class("Filters", cloneable = FALSE, 
  public = list(geo = NULL, min_survey = NULL, min_t = NULL, time = NULL))

Filter$set("public", "print", function(...) {
  cat("<Filter>:", fill = TRUE)
  cat("\tgeo: ", self$geo, fill = TRUE)
  cat("\ttime: ", self$time, fill = TRUE)
  cat("\tmin_survey: ", self$min_survey, fill = TRUE)
  cat("\tmin_t: ", self$min_t, fill = TRUE)
})

Control <- R6::R6Class(NULL, cloneable = FALSE, 
  private = list(groups_ = NULL),
  public = list(
    separate_t = NULL,
    constant_item = NULL,
    delta_tbar_prior_mean = NULL,
    delta_tbar_prior_sd = NULL,
    innov_sd_theta_scale = NULL,
    innov_sd_delta_scale = NULL
  )
)
Control$set("public", "print", function(...) {
  cat("<Control>:", fill = TRUE)
  cat("\tgroup variables: ", self$groups, fill = TRUE)
  cat("\tconstant_item: ", self$constant_item, fill = TRUE)
  cat("\tdelta_tbar_prior_mean: ", self$delta_tbar_prior_mean, fill = TRUE)
  cat("\tdelta_tbar_prior_sd: ", self$delta_tbar_prior_sd, fill = TRUE)
  cat("\tinnov_sd_delta_scale: ", self$innov_sd_delta_scale, fill = TRUE)
  cat("\tinnov_sd_theta_scale: ", self$innov_sd_theta_scale, fill = TRUE)
  cat("\tseparate_t: ", self$separate_t, fill = TRUE)
})
Control$set("active", "groups", gen_bind(groups_))

Modifier <- R6::R6Class("Modifier",
  cloneable = FALSE,
  private = list(
    geo_ = NULL,
    l2_only_ = NULL,
    modifiers_ = NULL,
    t1_modifiers_ = NULL,
    tbl_ = NULL,
    time_ = NULL
  ),
  public = list(
    NNl2 = NULL,
    SSl2 = NULL,
    WT = NULL,
    ZZ = NULL,
    ZZ_prior = NULL,
    group_design_matrix = NULL
  )
)

Modifier$set("public", "print", function(...) {
  cat("<Modifier>:", fill = TRUE)
  cat("hierarchical data (", nrow(self$tbl), " x ", ncol(self$tbl), ")", sep = "", fill = TRUE)
  cat("\tobserved geos: ", sort(unique(self$tbl[[self$geo]])), fill = TRUE)
  cat("\tobserved times: ", sort(unique(self$tbl[[self$time]])), fill = TRUE)
  cat("modifiers: ", self$modifiers, fill = TRUE) 
  cat("t1 modifiers: ", self$t1_modifiers, fill = TRUE) 
})
Modifier$set("public", "test_names",   test_names)
Modifier$set("public", "get_names",    get_names)
Modifier$set("active", "geo",          gen_bind(geo_, check_names = TRUE))
Modifier$set("active", "modifiers",    gen_bind(modifiers_, check_names = TRUE))
Modifier$set("active", "t1_modifiers", gen_bind(t1_modifiers_, check_names = TRUE))
Modifier$set("active", "l2_only",      gen_bind(l2_only_))
Modifier$set("active", "tbl",          bind_tbl)
Modifier$set("active", "time",         gen_bind(time_, check_names = TRUE))

Item <- R6::R6Class("Item", cloneable = FALSE, 
  private = list(
    G_hier_ = NULL,
    geo_ = NULL,
    group_counts_ = NULL,
    group_grid_ = NULL,
    group_grid_t_ = NULL,
    items_ = NULL,
    survey_ = NULL,
    time_ = NULL),
  public = list(
    MMM = NULL,
    check_groups = check_groups,
    control = NULL,
    filters = NULL,
    get_names = get_names,
    has_hierarchy = has_hierarchy,
    initialize = function() {
      self$modifier = Modifier$new()
      self$control = Control$new()
      self$filters = Filter$new()
      self$targets = Target$new()
    },
    modifier = NULL,
    print = function(...) {
      cat("<Item>:", fill = TRUE)
      cat("\titem data (", nrow(self$tbl), " x ", ncol(self$tbl), ")", sep = "", fill = TRUE)
      cat("\titems: ", paste(sort(intersect(names(self$tbl), self$items)), sep = ", "), fill = TRUE)
      cat("\tobserved geos: ", sort(unique(self$tbl[[self$geo]])), fill = TRUE)
      cat("\tobserved times: ", sort(unique(self$tbl[[self$time]])), fill = TRUE)
      if (length(self$control) > 0) print(self$control)
      if (length(self$filters) > 0) print(self$filters)
      if (length(self$targets) > 0) print(self$targets)
      if (length(self$modifier) > 0) print(self$modifier)
    },
    targets = NULL,
    tbl = NULL,
    test_names = test_names),
  active = list(
    G = bind_G,
    G_hier = bind_G_hier,
    H = bind_H,
    H_prior = bind_H_prior,
    N = bind_N,
    P = bind_P,
    Q = bind_Q,
    S = bind_S,
    T = bind_T,
    geo = gen_bind(geo_, check_names = TRUE),
    groups = function(value) self$control$groups,
    group_counts = gen_bind(group_counts_),
    group_grid = gen_bind(group_grid_),
    group_grid_t = gen_bind(group_grid_t_),
    items = bind_items,
    survey = gen_bind(survey_),
    # tbl = bind_tbl,
    time = gen_bind(time_, check_names = TRUE),
    weight = function(value) self$targets$weight
  )
)
