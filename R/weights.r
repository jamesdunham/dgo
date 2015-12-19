#' Create individual survey weights from population targets
#'
#' Strata in the sampled data must be a subset of those in the population targets.
#' There can't be missingness in the stratifying variables.
#' @param .level1 \code{data.frame} with survey responses at the lowest level of aggregation.
#' @param .arg Arguments to `format_data`
#' @return \code{data_frame} of survey responses with new weights normalized to have mean 1 in each period.
create_weights <- function(.level1, .arg) {

  weight_formulas <- create_formulas(.arg$target_groups)
  weight_vars <- get_weight_vars(.arg$target_groups)

  # We can't have missingness in the stratifying variables of the sampled data
  if (!all(colSums(is.na(.level1[, weight_vars])) == 0)) {
    stop("Missingness in stratifying variables of the data")
  }

  # All strata in the data must appear in the target matrix. expand.grid
  # errored out, so do this, though it takes a while
  .level1 <- .level1 %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  .arg$targets <- .arg$targets %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(stratum = ~interaction(.[, weight_vars], drop = TRUE))
  if (!unique(.level1$stratum) %in% unique(.arg$targets$stratum)) {
    stop("Not all strata in the data appear in the target matrix")
  }

  # We'll create a design object from the target data.frame; this is a
  # data.frame with attributes that indicate the survey design
  target_design <- survey::svydesign(ids = ~1, data = .arg$targets,
    weights = formula(paste0("~", .arg$target_proportion)))

  rake_weight <- function(.level1, formula.list, target_design) {
    ds <- survey::svydesign(ids = ~1, data = .level1,
      weights = formula(paste0("~", .arg$survey_weight)))
    # NB this weight variable here is the individual weight
    pop.list <- lapply(formula.list, survey::svytable, design = target_design)
    rk <- rake_partial(design = ds, sample.margins = formula.list,
      population.margins = pop.list)
    wts <- 1 / rk$prob
    return(wts)
  }

  .level1$preweight <- rake_weight(.level1, formula.list = weight_formulas,
    target_design = target_design)

  .level1 <- .level1 %>%
    dplyr::group_by_(.dots = .arg$t_var) %>%
    dplyr::mutate_(preweight = ~preweight / mean(new_weight, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(.level1)
}

create_formulas <- function(target_groups) {
  if (!length(target_groups) > 0) stop("target_groups is NULL")
  if (is.list(target_groups)) {
    out <- sapply(target_groups, function(x) {
      as.formula(paste("~", paste(x, collapse = " + ")))
    })
  } else {
    out <- list(as.formula(paste("~", paste(target_groups, collapse = " + "))))
  }
  return(out)
}

get_weight_vars <- function(target_groups) {
  unique(unlist(target_groups))
}

# This is a minor modification of survey::rake that ignores (leaves unweighted)
# empty cells
rake_partial <- function(design, sample.margins, population.margins, control = list(maxit = 10,
    epsilon = 1, verbose = FALSE), compress = NULL) {
    if (!missing(control)) {
        control.defaults <- formals(survey::rake)$control
        for (n in names(control.defaults)) if (!(n %in% names(control)))
            control[[n]] <- control.defaults[[n]]
    }
    is.rep <- inherits(design, "svyrep.design")
    if (is.rep && is.null(compress))
        compress <- inherits(design$repweights, "repweights_compressed")
    if (is.rep)
        design$degf <- NULL
    if (length(sample.margins) != length(population.margins))
        stop("sample.margins and population.margins do not match.")
    nmar <- length(sample.margins)
    if (control$epsilon < 1)
        epsilon <- control$epsilon * sum(weights(design, "sampling"))
    else epsilon <- control$epsilon
    strata <- lapply(sample.margins, function(margin) if (inherits(margin,
        "formula")) {
        mf <- model.frame(margin, data = design$variables, na.action = na.fail)
    })
    allterms <- unlist(lapply(sample.margins, all.vars))
    ff <- formula(paste("~", paste(allterms, collapse = "+"),
        sep = ""))
    oldtable <- survey::svytable(ff, design)
    if (control$verbose)
        print(oldtable)
    oldpoststrata <- design$postStrata
    iter <- 0
    converged <- FALSE
    while (iter < control$maxit) {
        design$postStrata <- NULL
        for (i in 1:nmar) {
            # The only change is to add ", partial = TRUE" below
            design <- survey::postStratify(design, strata[[i]], population.margins[[i]],
                compress = FALSE, partial = TRUE)
        }
        newtable <- survey::svytable(ff, design)
        if (control$verbose)
            print(newtable)
        delta <- max(abs(oldtable - newtable))
        if (delta < epsilon) {
            converged <- TRUE
            break
        }
        oldtable <- newtable
        iter <- iter + 1
    }
    rakestrata <- design$postStrata
    if (!is.null(rakestrata)) {
        class(rakestrata) <- "raking"
        design$postStrata <- c(oldpoststrata, list(rakestrata))
    }
    design$call <- sys.call()
    if (is.rep && compress)
        design$repweights <- survey::compressWeights(design$repweights)
    if (is.rep)
        design$degf <- survey::degf(design)
    if (!converged)
        warning("Raking did not converge after ", iter, " iterations.\n")
    return(design)
}
