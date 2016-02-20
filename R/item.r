setClass("ItemVar", contains="character")

setGeneric(".Data", function(x) standardGeneric(".Data"))
setGeneric(".Data<-", function(x, value) standardGeneric(".Data<-"))

setGeneric("drop", function(.Object, value = NULL) standardGeneric("drop"))
setMethod("drop", c("ItemVar", "ANY"), function(.Object, value = NULL) {
  if (!length(value) < 1) {
    if (!is.character(value)) stop("value must be character")
    .Object@.Data <- .Object@.Data[!.Object@.Data %in% value]
  }
  .Object
})

setGeneric("keep", function(.Object, value = NULL) standardGeneric("keep"))
setMethod("keep", c("ItemVar", "ANY"), function(.Object, value = NULL) {
  if (is.character(value)) {
    .Object@.Data <- intersect(.Object@.Data, value)
  } else if (!length(value) > 0) {
    .Object@.Data <- character(0)
  } else {
    stop("value must be character")
  }
  .Object
})

setMethod("initialize", "ItemVar", function(.Object, .Data) {
  if (!missing(.Data)) {
    if (!all(nchar(.Data) > 0)) stop("variable name is not a positive-length character")
    if (!length(.Data) > 0) stop("variable name is NULL")
    if (any(duplicated(.Data))) stop("duplicate variable name")
    .Object@.Data <- .Data
  }
  .Object
})

# Item

set_tbl <- function(x) {
  if (!missing(x) && length(x) > 0) {
    tbl_ <<- x
    if (!inherits(x, "data.frame")) {
      stop("item data must inherit from data.frame")
    }
    if (!all(dim(x) > 0)) {
      stop("item data has an empty dimension")
    } else {
      for (s in .self$get_names()) {
        if (!s %in% names(x)) {
          stop(s, " is not a variable in item data")
        }
      }
    } 
  } else {
    tbl_
  }
}

set_items <- function(x) {
    if (!missing(x)) {
      if (!length(x) < 1)  {
        if (is.character(x)) {
          x <- new("ItemVar", x)
        }
        .self$test_names(x)
      }
      items_ <<- x
    } else {
      items_
    }
}

set_groups <- function(x) {
    if (!missing(x) && length(x) > 0) {
      groups_ <<- x
      .self$test_names(x)
    } else {
      groups_
    }
}

set_time <- function(x) {
    if (!missing(x) && length(x) > 0) {
      time_ <<- x
      .self$test_names(x)
    } else {
      time_
    }
}

set_geo <- function(x) {
    if (!missing(x) && length(x) > 0) {
      geo_ <<- x
      .self$test_names(x)
    } else {
      geo_
    }
}

set_survey <- function(x) {
    if (!missing(x) && length(x) > 0) {
      survey_ <<- x
      .self$test_names(x)
    } else {
      survey_
    }
}

set_weight <- function(x) {
    if (!missing(x) && length(x) > 0) {
      weight_ <<- x
      .self$test_names(x)
    } else {
      weight_
    }
}

add_filters <- function(arg) {
  filters <- Filters$new()
  filters$t <- arg$periods
  filters$geo <- arg$use_geo
  filters
} 

# filters = list(periods = c(2006:2010))
Filters <- setRefClass("Filters",
  fields = list(
    t = "numeric",
    geo = "character",
    min_t = "numeric",
    min_survey = "numeric")
)

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
    filters = "Filters"))

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

# Targets <- setRefClass("Targets",
#   fields = list(
#     tbl = set_internal,
#     internal = "ANY",
#     groups = "Name",
#     prop_name = "Name"
#   )
# )
#
Item$methods(restrict = function() {
  .self <- restrict_items(.self)
  tbl <<- droplevels(.self$tbl)
})
#
# Item$methods(weight = function() {
#   if (!is.null(dim(.self$targets$internal)) > 0) {
#     tbl <<- create_weights(.self)
#   }
# })
#
# Item$methods(dichotomize = function() {
#   # all rows should have at least 1 item response
# })
#
# Item$methods(is_missing = function() {
#   MMM <- create_missingness_array(group_counts, group_grid, arg)
# })
#
# Item$methods(count = function() {
#   Q <<- count_questions(.self)
# })
#

restrict_items <- function(item) {
  initial_dim <- dim(item$tbl)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message("Applying restrictions, pass ", iter, "...")
    initial_dim <- dim(item$tbl)
    item <- keep_t(item)
    item <- drop_itemless_respondents(item)
    item <- drop_responseless_items(item)
    item <- drop_items_rare_in_time(item)
    item <- drop_items_rare_in_polls(item)
    not_empty(item$tbl)
    final_dim <- dim(item$tbl)
    iter <- iter + 1L
    message("Remaining: ", nrow(item$tbl), " rows, ", length(item$items), " items")
    message()
  }
  item
}


keep_t <- function(item) {
  item$tbl <- item$tbl %>%
    dplyr::filter_(lazyeval::interp(~observed_t %in% keep_t, observed_t = as.name(item$time), keep_t = item$filters$t))
  not_empty(item$tbl)
  item
}

# item = items
drop_itemless_respondents <- function(item) {
  items_kept <- intersect(item$items, colnames(item$tbl))
  item$items <- intersect(item$items, items_kept)
  item_data <- item$tbl %>% dplyr::select_(~one_of(items_kept))
  item$tbl$none_valid <- get_itemless_respondents(item)
  item$tbl <- item$tbl %>% dplyr::filter_(lazyeval::interp(~!none_valid))
  message(sprintf(ngettext(sum(item$tbl$none_valid),
        "\tDropped  %i row for lack of item responses",
        "\tDropped %i rows for lack of item responses"),
      sum(item$tbl$none_valid)))
  item
}

drop_responseless_items <- function(item) {
  assertthat::assert_that(assertthat::not_empty(item$tbl))
  assertthat::assert_that(assertthat::not_empty(item$items))
  responseless_items <- get_responseless_items(item)
  item$items <- setdiff(item$items, responseless_items)
  if (length(responseless_items) > 0) {
    item$tbl <- item$tbl %>%
      dplyr::select_(lazyeval::interp(~-one_of(v), v = responseless_items))
  }
  message(sprintf(ngettext(length(responseless_items),
        "\tDropped %i item for lack of responses",
        "\tDropped %i items for lack of responses"),
      length(responseless_items)))
  item
}

drop_items_rare_in_time <- function(item) {
  q_rare <- get_items_rare_in_time(item)
  item$items <- setdiff(item$items, q_rare)
  if (length(q_rare) > 0) {
    item$tbl <- item$tbl %>% dplyr::select_(lazyeval::interp(~-one_of(v),
        v = q_rare))
  }
  message(sprintf(ngettext(length(q_rare),
        "\tDropped %i items for failing min_t requirement (%i)",
        "\tDropped %i items for failing min_t requirement (%i)"),
      length(q_rare), item$filters$min_t))
  assertthat::assert_that(assertthat::not_empty(setdiff(item$items, q_rare)))
  item
}

get_responseless_items <- function(item) {
  n_item_responses <- item$tbl %>%
    dplyr::summarise_each_(~sum(!is.na(.)), vars = ~one_of(item$items)) %>%
    wrap_melt(id.vars = NULL, value.name = "n_responses")
  responseless_items <- n_item_responses %>%
    dplyr::filter_(~n_responses == 0L)
  responseless_items <- unlist(responseless_items$variable)
  assertthat::assert_that(is.character(responseless_items))
  responseless_items
}

get_itemless_respondents <- function(item) {
  if (length(item$items) < 1) stop("no remaining items")
  not_na <- !is.na(item$tbl %>%
    dplyr::select_(~one_of(item$items)))
  rowSums(not_na) == 0
}

get_items_rare_in_time <- function(item) {
  q_when_asked <- get_question_periods(item)
  q_t_count <- colSums(dplyr::select_(q_when_asked,
    lazyeval::interp(~-one_of(v), v = item$time)))
  q_rare <- names(q_t_count)[q_t_count < item$filters$min_t]
  q_rare
}

drop_items_rare_in_polls <- function(item) {
  lt_min_surveys <- get_items_rare_in_polls(item)
  item$items <- setdiff(item$items, lt_min_surveys)
  if (length(lt_min_surveys) > 0) {
    item$tbl <- item$tbl %>%
      dplyr::select_(lazyeval::interp(~-one_of(v), v = lt_min_surveys))
  }
  message(sprintf(ngettext(length(lt_min_surveys),
        "\t%i question fails min_surveys requirement (%i)",
        "\t%i questions fail min_surveys requirement (%i)"),
      length(lt_min_surveys), item$filters$min_survey))
  assertthat::assert_that(assertthat::not_empty(setdiff(item$items, lt_min_surveys)))
  item
}

get_items_rare_in_polls <- function(item) {
  q_which_asked <- get_question_polls(item)
  q_counts <- q_which_asked %>% dplyr::select_(lazyeval::interp(~-one_of(v),
      v = item$survey)) %>%
    dplyr::summarise_each(~sum)
  lt_min_surveys <- colnames(q_counts)[unlist(q_counts) < item$filters$min_survey]
  lt_min_surveys
}

get_question_periods <- function(item) {
  question_periods <- item$tbl %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_each_(~anyValid, vars = ~one_of(item$items))
  question_periods
}

# add_gt_variables <- function(level1, arg) {
#   gt_table <- create_gt_variables(d = level1, items = arg$items)
#   level1 <- dplyr::bind_cols(level1, gt_table)
#   return(level1)
# }
#
