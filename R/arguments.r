# Check argument lengths
check_arg_lengths <- function(.arg) {
    if (is.null(.arg$items) || length(.arg$items) < 1) {
        stop("no variable(s) given as items")
    }
    # These arguments should be length-one; confirm they are
    length_one <- .arg[c("time_id", "geo_id", "survey_weight", "survey_id")]
    if (any(sapply(length_one, length) == 0)) {
        stop(sprintf("no variable given for %s", stringi::stri_c(c("time_id", "geo_id", "survey_weight",
            "survey_id")[sapply(length_one, length) == 0], collapse = ", ")))
    } else if (any(sapply(length_one, length) != 1)) {
        stop(sprintf("more than one variable given for %s", stringi::stri_c(names(which(sapply(length_one,
            length) != 1)), collapse = ", ")))
    }
    if (any(!is.null(.arg$target_proportion)) & length(.arg$target_proportion) > 1) {
        stop("more than one variable given for target_proportion")
    }
    return(TRUE)
}

# Check that names given in arguments appear in data.frame arguments
check_arg_names <- function(..arg) {
    # These variables must exist in level1
    check_names(unlist(..arg[c("items", "time_id", "groups", "geo_id", "survey_weight", "survey_id")]),
        ..arg$level1)

    # If any of these variables are given, level2 must also
    imply_level2 <- unlist(..arg[c("level2_modifiers", "level2_period1_modifiers")])
    if (!all(sapply(imply_level2, is.null)) && is.null(..arg$level2)) {
        stop(sprintf("%s given but level2 is NULL", stringi::stri_c(names(which(!sapply(imply_level2,
            is.null))), collapse = ", ")))
    }

    # If level2 exists, these variables must exist in it
    check_names(unlist(..arg[c("time_id", "geo_id")]), ..arg$level2, check_tab = FALSE)

    # If given, these variables with NULL defaults must exist in level2
    check_names(unlist(..arg[c("level2_modifiers", "level2_period1_modifiers")]), ..arg$level2,
      check_tab = FALSE, check_nulls = FALSE)

    # If targets exists, these variables must exist in it
    check_names(unlist(..arg[c("time_id", "groups", "geo_id", "target_groups",
      "target_proportion")]), ..arg$targets, check_tab = FALSE)

    return(TRUE)
}

# Check that names appear in data.frames
check_names <- function(varnames, tab, check_tab = TRUE, check_nulls = TRUE) {
    if (is.null(tab)) {
        if (check_tab) {
            stop(sprintf("%s missing with no default", as.expression(substitute(tab))))
        } else if (!check_tab) {
            return(NULL)
        }
    }
    if (check_nulls) {
        if (is.null(varnames)) {
            stop(sprintf("%s missing with no default", as.expression(substitute(varnames))))
        }
        nulls <- varnames[is.null(varnames)]
        if (length(nulls) > 0) {
            stop(sprintf("%s missing with no default", stringi::stri_c(nulls, collapse = ", ")))
        }
    }
    defined_varnames <- varnames[!is.null(varnames)]
    if (length(defined_varnames) > 0) {
        missing_vars <- setdiff(defined_varnames, names(tab))
        if (length(missing_vars) > 0) {
            stop(sprintf(ngettext(length(missing_vars), "%s isn't an element of %s", "%s aren't elements of %s"),
                stringi::stri_c(missing_vars, collapse = ", "), sub(".arg$", "", as.expression(substitute(tab)),
                  fixed = T)))
        }
    }
    return()
}

# Check argument types and throw an error if a check fails
check_arg_types <- function(..arg) {

    # data and targets should inherit from data.frame
    if (is.null(..arg$level1))
        stop("level1 is NULL")
    if (!inherits(..arg$level1, "data.frame"))
        stop("level1 should inherit from data.frame")
    if (!is.null(..arg$level2)) {
        if (!inherits(..arg$level2, "data.frame"))
            stop("level2 should inherit from data.frame")
        if (!is.character(..arg$level2_modifiers))
            stop("level2_modifiers should be a character vector")
        if (!is.character(..arg$level2_period1_modifiers))
            stop("level2_period1_modifiers should be a character vector")
    }
    if (!is.null(..arg$targets)) {
        if (!inherits(..arg$targets, "data.frame"))
            stop("targets should inherit from data.frame")
        if (!is.character(..arg$target_proportion))
            stop("targets given so target_proportion should be a character vector")
    }

    if (!is.character(..arg$items))
        stop("items should be a character vector")
    if (!is.character(..arg$time_id))
        stop("time_id should be a character vector")
    if (!is.character(..arg$groups))
        stop("groups should be a character vector")
    if (!is.character(..arg$geo_id))
        stop("geo_id should be a character vector")
    if (!is.character(..arg$survey_weight))
        stop("survey_weight should be a character vector")
    if (!is.character(..arg$survey_id))
        stop("survey_id should be a character vector")

    if (is.null(..arg$difficulty_count) || !is.numeric(..arg$difficulty_count) || ..arg$difficulty_count <
        1 || ..arg$difficulty_count%%1 != 0) {
        stop("difficulty_count should be a positive integer")
    }

    if (is.null(..arg$min_surveys) || !is.numeric(..arg$min_surveys) || ..arg$min_surveys <
        1 || ..arg$min_surveys%%1 != 0) {
        stop("min_surveys should be a positive integer")
    }

    if (is.null(..arg$min_periods) || !is.numeric(..arg$min_periods) || ..arg$min_periods <
        1 || ..arg$min_periods%%1 != 0) {
        stop("min_periods should be a positive integer")
    }

    if (is.null(..arg$constant_item) || !is.logical(..arg$constant_item)) {
        stop("constant_item should be TRUE or FALSE")
    }

    if (is.null(..arg$separate_periods) || !is.logical(..arg$separate_periods)) {
        stop("separate_periods should be TRUE or FALSE")
    }

    if (is.null(..arg$silent) || !is.logical(..arg$silent)) {
        stop("silent should be TRUE or FALSE")
    }

    return(TRUE)
}

check_no_defaults <- function(..arg) {
    no_default <- ..arg[c("items", "time_id", "groups", "geo_id", "survey_weight", "survey_id")]
    if (any(sapply(no_default, is.symbol))) {
        stop(sprintf("%s missing with no default", stringi::stri_c(names(which(sapply(no_default,
            is.symbol))), collapse = ", ")))
    }
}

# Set non-NULL defaults
set_arg_defaults <- function(..arg) {
    if (is.null(..arg$separate_periods))
        ..arg$separate_periods <- FALSE
    if (is.null(..arg$difficulty_count))
        ..arg$difficulty_count <- 1L
    if (is.null(..arg$min_surveys))
        ..arg$min_surveys <- 1L
    if (is.null(..arg$min_periods))
        ..arg$min_periods <- 1L
    if (is.null(..arg$constant_item))
        ..arg$constant_item <- TRUE
    if (is.null(..arg$silent))
        ..arg$silent <- FALSE
    return(..arg)
}
