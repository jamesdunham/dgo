setClass("ItemVar", contains="character")
setMethod("initialize", "ItemVar", function(.Object, .Data) {
  if (!missing(.Data)) {
    if (!all(nchar(.Data) > 0)) stop("variable name is not a positive-length character")
    if (!length(.Data) > 0) stop("variable name is NULL")
    if (any(duplicated(.Data))) stop("duplicate variable name")
    .Object@.Data <- .Data
  }
  .Object
})

Targets <- setRefClass("Targets",
  fields = list(
    tbl = "data.frame",
    strata = "formula",
    prop = "ItemVar"
  )
)

Filters <- setRefClass("Filters",
  fields = list(
    t = "numeric",
    geo = "character",
    min_t = "numeric",
    min_survey = "numeric")
)

Control <- setRefClass("Control",
  fields = list(
    constant_item = "integer",
    separate_t = "integer",
    delta_tbar_prior_mean = "numeric",
    delta_tbar_prior_sd = "numeric",
    innov_sd_delta_scale = "numeric",
    innov_sd_theta_scale = "numeric"))

Modifier <- setRefClass("Modifier",
  fields = list(
    tbl = set_tbl,
    tbl_ = "ANY",
    modifiers = set_modifiers,
    modifiers_ = "ItemVar",
    t1_modifiers = set_t1_modifiers,
    t1_modifiers_ = "ItemVar",
    time = set_time,
    time_ = "ItemVar",
    geo = set_geo,
    geo_ = "ItemVar",
    Gl2 = "integer",
    WT = "array",
    l2_only = "array",
    NNl2 = "array",
    SSl2 = "array",
    group_design_matrix = "matrix",
    P = get_p,
    ZZ = "array",
    ZZ_prior = "array",
    H = get_h,
    S = get_s,
    H_prior = get_h_prior))

Item <- setRefClass("Item",
  fields = list(
    tbl = set_tbl,
    tbl_ = "ANY",
    items = set_items,
    items_ = "ItemVar",
    groups = set_groups,
    groups_ = "ItemVar",
    time = set_time,
    time_ = "ItemVar",
    geo = set_geo,
    geo_ = "ItemVar",
    survey = set_survey,
    survey_ = "ItemVar",
    weight = set_weight,
    weight_ = "ItemVar",
    filters = "Filters",
    targets = "Targets",
    modifier = "Modifier",
    control = "Control",
    T = get_t,
    Q = get_q,
    G = get_g,
    N = get_n,
    group_grid = "data.frame",
    group_grid_t = "data.frame",
    group_counts = "data.frame",
    MMM = "array"))

Item$methods(has_hierarchy = function() {
  !inherits(item$modifier$tbl, "uninitializedField")
})

Item$methods(make_WT = function() {
  .self$modifier$WT <- array(1, dim = c(.self$T, .self$modifier$Gl2, .self$G))
})

Item$methods(make_modifier_group_n = function() {
  .self$modifier$Gl2 <- count_modifier_groups(.self)
})

Item$methods(get_names = function() {
  nm = Map(function(i) .self[[i]], names(Item$fields())[grep("ItemVar", Item$fields())])
  unique(unlist(nm))
})

Item$methods(test_names = function(x) {
  stopifnot(inherits(x, "ItemVar"))
  for (s in x) {
    if (!s %in% names(.self$tbl)) {
      stop(s, " is not a variable in item data")
    }
  }
})

Item$methods(restrict = function() {
  .self <- restrict_items(.self)
  .self <- restrict_modifier(.self)
  tbl <<- droplevels(.self$tbl)
})

Item$methods(reweight = function() {
  if (!length(.self$targets$tbl) < 1) {
    .self <- weight(.self)
  }
})

Item$methods(make_gt_variables = function() {
  gt_table <- create_gt_variables(.self)
  .self$tbl <- dplyr::bind_cols(.self$tbl, gt_table)
})

Item$methods(find_missingness = function() {
  .self$MMM <- make_missingness_array(item)
})

Item$methods(get_group_grid = function() {
  .self$group_grid <- make_group_grid(item)
})

Item$methods(make_l2_only = function() {
  .self$modifier$l2_only <- make_dummy_l2_only(.self)
})

Item$methods(make_NNl2 = function() {
  .self$modifier$NNl2 <- make_dummy_l2_counts(.self)
})

Item$methods(make_SSl2 = function() {
  .self$modifier$SSl2 <- make_dummy_l2_counts(.self)
})

Item$methods(make_group_design = function() {
    .self$modifier$group_design_matrix <- make_design_matrix(.self)
})

Item$methods(make_ZZ = function() {
  .self$modifier$ZZ <- make_hierarchical_array(.self)
})

Item$methods(make_ZZ_prior = function() {
  .self$modifier$ZZ_prior <- make_hierarchical_array(.self)
})

Item$methods(check_groups = function(group_grid_t) {
  for (s in .self$groups) {
    if (length(levels(group_grid_t[[s]])) < 2) {
      stop("no variation in group variable ", s)
    }
  }
})

Item$methods(list_groups = function() {
  group_grid <<- make_group_grid(.self)
})

Item$methods(list_groups_t = function() {
  group_grid_t <<- make_group_grid_t(.self)
})

Item$methods(group_n = function() {
  group_counts <<- make_group_counts(.self)
})
