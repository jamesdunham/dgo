# This is a minor modification of survey::rake that ignores (leaves unweighted) empty cells
# nolint start
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
    assertthat::assert_that(equal_length(sample.margins, population.margins))
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
# nolint end
