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

Target <- R6::R6Class("Target",
  cloneable = FALSE,
  class = FALSE,
  public = list(
    prop = NULL,
    strata = NULL,
    tbl = NULL,
    test_names = test_names,
    weight = NULL)
)

Filter <- R6::R6Class("Filters",
  cloneable = FALSE,
  class = FALSE,
  public = list(
    geo = NULL,
    min_survey = NULL,
    min_t = NULL,
    time = NULL)
)

Control <- R6::R6Class(NULL,
  cloneable = FALSE,
  class = FALSE,
  public = list(
    constant_item = NULL,
    delta_tbar_prior_mean = NULL,
    delta_tbar_prior_sd = NULL,
    groups = NULL,
    innov_sd_delta_scale = NULL,
    innov_sd_theta_scale = NULL,
    separate_t = NULL))

Modifier <- R6::R6Class("Modifier",
  cloneable = FALSE,
  class = FALSE,
  private = list(
    geo_ = NULL,
    l2_only_ = NULL,
    modifiers_ = NULL,
    t1_modifiers_ = NULL,
    tbl_ = NULL,
    time_ = NULL),
  public = list(
    NNl2 = NULL,
    SSl2 = NULL,
    WT = NULL,
    ZZ = NULL,
    ZZ_prior = NULL,
    group_design_matrix = NULL,
    test_names = test_names,
    get_names = get_names),
  active = list(
    H_prior = bind_H_prior,
    geo = bind_geo,
    modifiers = bind_modifiers,
    t1_modifiers = bind_t1_modifiers,
    l2_only = bind_l2_only,
    tbl = bind_tbl,
    time = bind_time))

Item <- R6::R6Class("Item",
  cloneable = FALSE,
  class = FALSE,
  private = list(
    G_hier_ = NULL,
    geo_ = NULL,
    group_counts_ = NULL,
    group_grid_ = NULL,
    group_grid_t_ = NULL,
    items_ = NULL,
    survey_ = NULL,
    tbl_ = NULL,
    time_ = NULL),
  public = list(
    MMM = NULL,
    control = NULL,
    filters = NULL,
    modifier = NULL,
    targets = NULL,
    check_groups = check_groups,
    get_names = get_names,
    test_names = test_names,
    restrict = restrict,
    reweight = reweight,
    make_gt_variables = make_gt_variables,
    list_groups = list_groups,
    list_groups_t = list_groups_t,
    group_n = group_n,
    has_hierarchy = has_hierarchy,
    find_missingness = find_missingness,
    make_WT = make_WT,
    make_l2_only = make_l2_only,
    make_NNl2 = make_NNl2,
    make_SSl2 = make_SSl2,
    make_group_design = make_group_design,
    make_ZZ = make_ZZ,
    make_ZZ_prior = make_ZZ_prior),
  active = list(
    G = bind_G,
    G_hier = bind_G_hier,
    H = bind_H,
    N = bind_N,
    P = bind_P,
    Q = bind_Q,
    S = bind_S,
    T = bind_T,
    geo = bind_geo,
    groups = bind_groups,
    group_counts = bind_group_counts,
    group_grid = bind_group_grid,
    group_grid_t = bind_group_grid_t,
    items = bind_items,
    survey = bind_survey,
    tbl = bind_tbl,
    time = bind_time,
    weight = bind_weight))
