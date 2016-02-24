set_modifiers <- function(x) {
    if (!missing(x) && length(x) > 0) {
      modifiers_ <<- x
      .self$test_names(x)
    } else {
      modifiers_
    }
}

set_t1_modifiers <- function(x) {
    if (!missing(x) && length(x) > 0) {
      t1_modifiers_ <<- x
      .self$test_names(x)
    } else {
      t1_modifiers_
    }
}


make_hierarchical_array <- function(item) {
  if (item$has_hierarchy()) {
    ZZ <- shape_hierarchical_data(item)
  } else {
    zz.names <- list(item$filters$t, dimnames(item$modifier$group_design_matrix)[[2]], "")
    ZZ <- array(data = 0, dim = lapply(zz.names, length), dimnames = zz.names)
  }
  ZZ
}

create_gt_variables <- function(item){
  widths <- c("item" = 30, "class" = 10, "levels" = 12, "responses" = 16)
  print_varinfo_header(widths)
  out <- lapply(item$items, function(i) {
    if (is.ordered(item$tbl[[i]])) {
      i_levels <- na.omit(levels(droplevels(item$tbl[[i]])))
      values <- match(as.character(item$tbl[[i]]), i_levels)
    } else if (is.numeric(item$tbl[[i]])) {
      i_levels <- sort.int(na.omit(unique.default(item$tbl[[i]])))
      values <- match(item$tbl[[i]], i_levels)
    } else {
      stop("each item should be an ordered factor or numeric")
    }
    gt_levels <- seq_along(i_levels)[-length(i_levels)]
    if (length(gt_levels) < 1) stop("no variation in item ", deparse(i))
    if (identical(length(gt_levels), 1L)) {
      assertthat::assert_that(has_length(i_levels, 2))
    }
    if (length(gt_levels) > 1L) {
      assertthat::assert_that(length(i_levels) > 2L)
    }
    print_varinfo(item$tbl, i, item_levels = i_levels, gt_levels = gt_levels, widths)
    gt_cols <- lapply(gt_levels, function(gt) {
      ifelse(values > gt, 1L, 0L)
    })
    assertthat::assert_that(not_empty(gt_cols))
    gt_names <- paste(i, gt_levels, sep = "_gt")
    setNames(gt_cols, gt_names)
  })
  print_varinfo_rule(widths)
  dplyr::bind_cols(out)
}

print_varinfo_header <- function(widths) {
  pad_side <- c("right", rep("left", length(widths) - 1))
  message(concat_varinfo(widths, names(widths), pad_side))
  print_varinfo_rule(widths)
}

print_varinfo_rule <- function(widths) {
  message(paste0(rep('-', sum(widths)), collapse = ''))
}

print_varinfo <- function(d, item, item_levels, gt_levels, widths) {
  item_name = ifelse(nchar(item) > 30, paste0(stringi::stri_sub(item, 1, 26), '...'), item)
  responses = format(sum(!is.na(d[[item]])), big.mark = ",")
  values <- c("item" = item_name, "class" = class(d[[item]]), "levels" = length(item_levels), responses = responses)
  pad_side <- c("right", rep("left", length(values) - 1))
  assertthat::assert_that(identical(length(widths), length(values)))
  message(concat_varinfo(widths, values, pad_side))
}

concat_varinfo = function(widths, values, pad_side) {
  mapply(function(width, value, side) {
      paste0(stringi::stri_pad(value, width, side = side), collapse = '')
    }, widths, values, pad_side)
}

make_group_grid <- function(item) {
  group_grid <- expand.grid(c(
    setNames(list(item$filters$t), item$time),
    lapply(item$tbl[, c(item$geo, item$groups), drop = FALSE], function(x) sort(unique(x)))))
  assertthat::assert_that(not_empty(group_grid))
  group_grid %>% dplyr::arrange_(.dots = c(item$time, item$groups, item$geo))
}

make_group_grid_t <- function(item) {
  group_grid_t <- item$group_grid %>%
    dplyr::select_(~-one_of(item$time)) %>%
    dplyr::distinct() %>%
    dplyr::arrange_(.dots = c(item$groups, item$geo))
  item$check_groups(group_grid_t)
  group_grid_t
}

restrict_items <- function(item) {
  item <- factorize_arg_vars(item)
  initial_dim <- dim(item$tbl)
  final_dim <- c()
  iter <- 1L
  while (!identical(initial_dim, final_dim)) {
    message()
    message("Applying restrictions, pass ", iter, "...")
    if (identical(iter, 1L)) item <- drop_rows_missing_covariates(item)
    initial_dim <- dim(item$tbl)
    item <- keep_t(item)
    item <- drop_itemless_respondents(item)
    item <- drop_responseless_items(item)
    item <- drop_items_rare_in_time(item)
    item <- drop_items_rare_in_polls(item)
    not_empty(item$tbl)
    final_dim <- dim(item$tbl)
    iter <- iter + 1L
    if (identical(initial_dim, final_dim)) {
      message("\tNo changes")
    } else {
      message("Remaining: ", format(nrow(item$tbl), big.mark = ","), " rows, ", length(item$items), " items")
    }
  }
  item
}

factorize_arg_vars <- function(item) {
  arg_vars <- intersect(names(item$tbl),
    c(item$groups, item$geo, item$survey, item$modifier$modifiers, item$modifier$t1_modifiers))
  numeric_groups <- item$tbl %>% dplyr::summarize_each_(~is.numeric, vars = arg_vars) %>% unlist()
  if (any(numeric_groups)) {
    message("Defining groups via numeric variables is allowed, but output names won't be descriptive. Consider using factors.")
    for (varname in names(numeric_groups)) {
      item$tbl[[varname]] <- paste0(varname, as.character(item$tbl[[varname]]))
    }
  }
  item$tbl <- with_contr.treatment(item$tbl %>% 
    dplyr::arrange_(.dots = arg_vars) %>%
    dplyr::mutate_each_(dplyr::funs(factor(., levels = sort(unique(as.character(.))))), vars = arg_vars))
  item
}

drop_rows_missing_covariates <- function(item) {
  n <- nrow(item$tbl)
  item$tbl <- item$tbl %>%
    dplyr::filter_(lazyeval::interp(~!is.na(geo_name) & !is.na(time_name),
  geo_name = as.name(item$geo),
  time_name = as.name(item$time)))
  for (v in c(item$time, item$geo, item$groups, item$survey)) {
    item$tbl <- item$tbl %>%
      dplyr::filter_(lazyeval::interp(~!is.na(varname), varname = as.name(v)))
  }
  if (!identical(n, nrow(item$tbl))) {
    message("\tDropped ", format(n - nrow(item$tbl), big.mark = ","), " rows for missingness in covariates")
    item$tbl <- droplevels(item$tbl)
  }
  item
}

with_contr.treatment <- function(...) {
  contrast_options = getOption("contrasts")
  options("contrasts"= c(unordered = "contr.treatment", ordered = "contr.treatment"))
  res <- eval(...)
  options("contrasts"= contrast_options)
  res
}

keep_t <- function(item) {
  item$tbl <- item$tbl %>%
    dplyr::filter_(lazyeval::interp(~observed_t %in% keep_t, observed_t = as.name(item$time), keep_t = item$filters$t))
  not_empty(item$tbl)
  item
}

# item = items
drop_itemless_respondents <- function(item) {
  item$items <- intersect(item$items, colnames(item$tbl))
  item$tbl$none_valid <- get_itemless_respondents(item)
  if (any(item$tbl$none_valid)) {
    item$tbl <- item$tbl %>% dplyr::filter_(lazyeval::interp(~!none_valid))
    message(sprintf(ngettext(sum(item$tbl$none_valid),
          "\tDropped  %i row for lack of item responses",
          "\tDropped %i rows for lack of item responses"),
        format(sum(item$tbl$none_valid), big.mark = ",")))
  }
  item
}

drop_responseless_items <- function(item) {
  responseless_items <- get_responseless_items(item)
  item$items <- setdiff(item$items, responseless_items)
  if (length(responseless_items) > 0) {
    item$tbl <- item$tbl %>%
      dplyr::select_(.dots = responseless_items)
    message(sprintf(ngettext(length(responseless_items),
          "\tDropped %i item for lack of responses",
          "\tDropped %i items for lack of responses"),
        length(responseless_items)))
  }
  item
}

drop_items_rare_in_time <- function(item) {
  q_rare <- get_items_rare_in_time(item)
  item$items <- setdiff(item$items, q_rare)
  if (length(q_rare) > 0) {
    item$tbl <- item$tbl %>% dplyr::select_(.dots =  q_rare)
    message(sprintf(ngettext(length(q_rare),
          "\tDropped %i items for failing min_t requirement (%i)",
          "\tDropped %i items for failing min_t requirement (%i)"),
        length(q_rare), item$filters$min_t))
  }
  item
}

get_responseless_items <- function(item) {
  n_item_responses <- item$tbl %>%
    dplyr::summarise_each_(~sum(!is.na(.)), vars = item$items) %>%
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
    dplyr::select_(.dots = item$items))
  rowSums(not_na) == 0
}

get_items_rare_in_time <- function(item) {
  q_when_asked <- get_question_periods(item)
  q_t_count <- colSums(dplyr::select_(q_when_asked, ~-one_of(item$time)))
  q_rare <- names(q_t_count)[q_t_count < item$filters$min_t]
  q_rare
}

drop_items_rare_in_polls <- function(item) {
  lt_min_surveys <- get_items_rare_in_polls(item)
  item$items <- setdiff(item$items, lt_min_surveys)
  if (length(lt_min_surveys) > 0) {
    item$tbl <- item$tbl %>%
      dplyr::select_(~-one_of(lt_min_surveys))
  }
  assertthat::assert_that(assertthat::not_empty(setdiff(item$items, lt_min_surveys)))
  item
}

get_items_rare_in_polls <- function(item) {
  q_which_asked <- get_question_polls(item)
  q_counts <- q_which_asked %>% dplyr::select_(~-one_of(item$survey)) %>%
    dplyr::summarise_each(~sum)
  lt_min_surveys <- colnames(q_counts)[unlist(q_counts) < item$filters$min_survey]
  if (length(lt_min_surveys) > 0) {
  message(sprintf(ngettext(length(lt_min_surveys),
        "\t%i question fails min_surveys requirement (%i)",
        "\t%i questions fail min_surveys requirement (%i)"),
      length(lt_min_surveys), item$filters$min_survey))
  }
  lt_min_surveys
}

get_question_polls <- function(item) {
  question_polls <- item$tbl %>%
    dplyr::group_by_(item$survey) %>%
    dplyr::summarise_each_(dplyr::funs(any_not_na), vars = item$items)
  question_polls
}

get_question_periods <- function(item) {
  question_periods <- item$tbl %>%
    dplyr::group_by_(item$time) %>%
    dplyr::summarise_each_(~any_not_na, vars = item$items)
  question_periods
}

make_group_counts <- function(item) {
  item$tbl$n_responses <- count_responses(item)
  design_effects <- make_design_effects(item)
  trial_counts <- count_trials(item, design_effects)
  group_means <- make_group_means(item)
  success_counts <- count_successes(item, trial_counts, group_means)
  format_counts(item, trial_counts, success_counts)
}

make_missingness_array <- function(item) {
  missingness <- get_missingness(item)
  cast_missingness(item, missingness)
}

restrict_modifier <- function(item) {
  if (item$has_hierarchy()) {
  message("Restricting modifier data to time and geo observed in item data...")
    inital_nrow <- nrow(item$modifier$tbl)
    item$modifier$tbl <- item$modifier$tbl %>%
      dplyr::filter_(lazyeval::interp(~modifier_geo %in% item$filter$geo
        && modifier_t %in% item$filter$t, modifier_geo = as.name(item$modifier$geo),
          modifier_t = as.name(item$modifier$t)))
    final_nrow <- nrow(item$modifier$tbl)
    message("\tDropped ", initial_nrow - final_nrow, " rows")
    message("Remaining: ", format(final_nrow, big.mark = ","), " rows")
  }
  item
}
