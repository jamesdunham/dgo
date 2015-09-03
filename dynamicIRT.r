# Define the user-exposed functions for dynamic IRT
# Created 6/15/15
# J.Dunham
#
# TODO: filestring

## Dependencies ##

library(rstan)
library(reshape2)
# TODO: confirm dependency
library(car)
# TODO: rewrite the remaining plyr calls with dplyr
library(plyr)
library(survey)
# TODO: rewrite in base R
library(stringr)
# TODO: confirm (could probably use tidry, already a dplyr dependency, instead)
library(data.table)
# TODO: confirm
library(rms)
library(abind)
# NOTE: Load dplyr late, or use of filter() may break
library(dplyr)
library(tidyr)

## Internal functions ##

source('funs.r')
source('stan.code.r')

## User-exposed functions ##

checkData = function(.data, .group.data = NULL, .opts) {
  # TODO: docstring

  # NOTE: for dev
  # .data = as.tbl(abort.df)
  # .group.data = as.tbl(abort.st.df)
  # .opts = abort.test.opts

  # Convert to tidyr tbl_df for dplyr
  .data = as.tbl(.data)

  # TODO: confirm which strings must exist as variables if specified
  # Check that variables given as strings exist
  # all.vars = c(
  #   .opts$q.vars,
  #   .opts$fm.vars,
  #   .opts$time.var,
  #   .opts$demo.vars,
  # if (!all(all.vars %in% colnames(.data))) {
  #   stop(paste("Didn't find all variabels given as strings in .data",
  #       str_c(names(all.vars[!(all.vars %in% colnames(data))]) , collapse=", ")))
  # }

  # Get levels of the time variable, handling the possibility that it isn't a factor
  if (is.factor(.data[[.opts$time.var]])) {
    periods = as.numeric(levels(.data[[.opts$time.var]]))
  } else {
    periods = as.numeric(unique(.data[[.opts$time.var]]))
  }
  cat("\n\n")
  cat("Time variable has", length(periods), "levels: ")
  cat(sort(periods), sep=", ")
  cat("\n")

  # Get the range of the time variable
  time.range = c("min" = min(periods), "max" = max(periods))
  # TODO: handle the possibility that the user doesn't want to estimate all
  # periods in the range

  # Check for question columns with no valid responses
  invalid.vars = names(.data)[colSums(!is.na(.data)) == 0]
  if (length(invalid.vars) > 0) {
    cat("\n\n")
    cat(length(invalid.vars), "response variables are entirely missing: ")
    cat(invalid.vars, sep=", ")
    # Drop the invalid variables from the local vector of question variable
    # names. (Data untouched.)
    .opts$q.vars = sort(setdiff(.opts$q.vars, invalid.vars))
    cat("\n\n")
    cat(length(.opts$q.vars), "remaining response variables: ")
    cat(.opts$q.vars, sep=", ")
  }

  # Check for rows with no valid question responses
  # FIXME: error from abany_binary and abrape_binary missing from .data
  none.valid = rowSums(!is.na(.data[, .opts$q.vars])) == 0
  cat("\n\n")
  cat(sum(none.valid), "rows out of", nrow(.data), "have no valid responses.")

  # Check for missingness in frontmatter vars
  # TODO: do anything? just report for now
  row.missing.fm = rowSums(is.na(select(.data, one_of(.opts$fm.vars)))) > 0
  colsum.na.fm = colSums(is.na(select(.data, one_of(.opts$fm.vars))))
  if (any(row.missing.fm)) {
    cat("\n\n")
    cat(sum(row.missing.fm), "rows out of", nrow(.data), "have missingness in frontmatter variables.\n")
    cat("Proportion missing by variable:\n\n")
    print(round(colsum.na.fm/nrow(.data), 2))
  }

  # Check whether variables satisfy min.years requirement
  yrs.asked = .data %>%
    group_by_(.opts$time.var) %>%
    summarise_each(funs(anyValid), one_of(.opts$q.vars))
  valid.yrs = colSums(select(yrs.asked, -one_of(.opts$time.var)))
  yrs.invalid = names(valid.yrs[valid.yrs < .opts$min.years])
  cat("\n")
  cat(sum(valid.yrs < .opts$min.years), "response variables out of", (length(valid.yrs)), "fail min.years requirement.\n")
  cat(yrs.invalid, sep=", ")

  # If this wasn't a dry run (.opts$test = TRUE), attempt to fix problems
  if (!.opts$test) {
    # Drop rows with no valid question responses
    .data = filter(.data, !none.valid)
    cat("\n\nDropped", sum(none.valid), "rows for lack of any responses.")

    # Drop question columns with no valid responses
    if (length(invalid.vars) > 0) {
      .data = select(.data, -one_of(invalid.vars))
    }
    cat("\n\nDropped", length(invalid.vars), "questions for lack of valid responses.")

    # Drop variables that don't satisfy min.years requirement
    if (length(yrs.invalid) > 0) {
      .data = select(.data, -one_of(yrs.invalid))
    }
    cat("\n\nDropped", length(yrs.invalid), "more questions for failing min.years requirement.\n\n")
  }

  # TODO: could re-check the data after attempting to fix problems here

  ## Create tables summarizing the data ##

  # Create a summary table for which questions appear in which periods
  yrs.asked.melt <- melt(yrs.asked, id="YearFactor")

  # Create a factor in the data for the combinations of year and survey (e.g. "2008.ess")
  .data$survey.year <- interaction(.data$YearFactor, .data$survey, drop=TRUE, lex.order=TRUE)

  # And use it to create a summary table for whether a question appeared in
  # combinations of years and surveys. This table replaces forms.asked in the
  # original code.
  svy.year.asked = .data %>%
    group_by(survey.year) %>%
    select(one_of(.opts$q.vars)) %>%
    summarise_each(funs(anyValid))

  poll.count = svy.year.asked %>%
    select(one_of(.opts$q.vars)) %>%
    colSums() %>% melt(value.name = 'polls')

  # Final check: do questions appear in the minimum number of polls?
  lt.min.polls = names(poll.count)[poll.count < .opts$min.polls]
  cat(length(lt.min.polls), 'questions appear in fewer than the minimum polls per question, which is set to', .opts$min.polls, '.\n')
  cat(names(lt.min.polls)[lt.min.polls], sep = ',')
  cat('Dropped', length(lt.min.polls), 'questions for appearing in too few polls.\n\n')
  if (!.opts$test & length(lt.min.polls) > 0) {
    .data = select(.data, -one_of(lt.min.polls))
  } else if (.opts$test) {
    cat("Finished test.\n")
  }

  # Make all the variables given as strings factors
  .data = .data %>%
    mutate_each_(funs(factor), vars = c(.opts$time.var, .opts$demo.vars,
        .opts$geo.var, .opts$geo.mod.vars, .opts$geo.mod.prior.vars))

  out = list(
    data = .data,
    yrs.asked = yrs.asked,
    svy.year.asked = svy.year.asked,
    poll.count = poll.count,
    fm.vars = .opts$fm.vars,
    q.vars = .opts$q.vars,
    time.var = .opts$time.var,
    yr.range = yr.range,
    min.years = .opts$min.years,
    min.polls = .opts$min.polls)
  invisible(out)
}

# TODO: need to update .opts variables after running cleanData() to exclude
# from the lists of variables in .opts any that we dropped in checkData(), for
# example with:
# .opts$q.vars = .opts$q.vars[.opts$q.vars %in% colnames(.data)]

formatData = function(.data, .opts) {
  # TODO: docstring
  # TODO: check whether data appear as expected
  # dev:
  # .data = checked.data
  # .opts = test.opts

  # These variables are from the original code; use them for now rather than
  # refactoring.
  covs = c(.opts$geo.var, .opts$demo.vars)
  covs.yr = c(covs, .opts$time.var)
  f0 = Formula(c("0", covs))
  f0.yr = Formula(c("0", covs.yr))

  # Drop rows lacking covariates or time variable
  .data$data = .data$data %>%
    filter(rowSums(is.na(select(.data$data, one_of(covs.yr)))) == 0)

  # Create .gt. variables
  .data$data = createGT(d = .data$data, .q.vars = .data$q.vars)

  # Drop rows lacking at least one .gt variable
  .data$data = .data$data %>%
    filter(rowSums(!is.na(select(.data$data, contains('.gt')))) > 0)

  # Fix levels of geo variable after filtering
  .data$data[, .data$geo.var] = droplevels(.data$data[, .data$geo.var])
  # Fix levels of covariates after filtering
  .data$data[, covs.yr] = droplevels(.data$data[, covs.yr])

  # Represent covariates in data as model frame
  MF = model.frame(f0, data=.data$data, na.action=return)
  MF.yr = model.frame(f0.yr, data=.data$data, na.action=return)

  # Create factors with levels for each combination of covariate values
  group = interaction(as.list(MF), sep="__", lex.order=FALSE)
  group.yr = interaction(as.list(MF.yr), sep="__", lex.order=FALSE)

  # Get frequency counts of factor combinations (reordered in factor order)
  xtab.yr = as.data.frame(table(MF.yr))
  # TODO: why does the original code subset to the first year here?
  xtab = xtab.yr %>%
    subset(YearFactor == .data$yr.range[1], -c(YearFactor, Freq))
  # Check that group.yr levels are in same order as rows of xtab.yr
  stopifnot(identical(levels(group.yr), unname(apply(subset(xtab.yr,, -Freq), 1, paste, collapse="__"))))

  # Get dummy variable representation of contingency table
  XX <- model.matrix(f0, xtab)
  # Don't use hierarchical model if we only have one predictor
  if (length(covs) == 1) {
    XX = matrix(0, nrow(XX), ncol(XX))
  }
  # Create a variable counting number of responses
  .data$data$n.responses = rowSums(
    !is.na(select(.data$data, contains('.gt')) %>% as.matrix(.)),
    na.rm=T)

  # de.df was a 176K x 20 matrix in which a design effect was attached to
  # each respondent. But the design effects are unique to combinations of
  # the identifiers in covs.yr (here Country, Gender, and YearFactor), so
  # there are fewer than two dozen values. Instead, create a table with
  # the design effect for each combination.

  # Create table of design effects
  de.tbl = .data$data %>%
    group_by_(.dots = covs.yr) %>%
    summarise(def = summariseDef(useweight))

  # Create table of (adjusted) trial counts
  NN.tbl = .data$data %>%
    group_by_(.dots = covs.yr) %>%
    select(n.responses, contains(".gt")) %>%
    mutate_each(funs(notNA), contains(".gt")) %>%
    mutate_each(funs(. / n.responses), contains(".gt"))  %>%
    full_join(de.tbl, by = covs.yr) %>%
    summarise_each(funs(ceiling(sum(. / def))), contains(".gt")) %>%
    ungroup() %>%
    mutate_each(funs(as.character), Country, Gender, YearFactor) %>%
    arrange_(covs.yr)

  # TODO: Check this: in the new NN, six all-NA covariate combinations are
  # excluded; not in the original NN. Should these cells be zero instead?

  # Create table of (weighted) outcomes
  y.bar.star = .data$data %>%
    group_by_(.dots = covs.yr) %>%
    select(useweight, n.responses, contains(".gt")) %>%
    summarise_each(funs(weighted.mean(., useweight / n.responses, na.rm=T)), contains(".gt")) %>%
    mutate_each(funs(ifelse(is.nan(.), 0, .)), contains(".gt")) %>%
    ungroup() %>%
    mutate_each_(funs(as.character), covs.yr) %>%
    arrange_(covs.yr)

  # Check row order before taking product
  stopifnot(identical(select(y.bar.star, Country:YearFactor),
    select(NN.tbl, Country:YearFactor)))
  SS = select(NN.tbl, contains('.gt')) *
      select(y.bar.star, contains('.gt'))
  SS = SS %>%
    bind_cols(NN.tbl[, 1:3]) %>%
    mutate_each(funs(round(., digits=0)), contains(".gt")) %>%
    arrange(Country, Gender, YearFactor)

  N.valid = data.frame(t = NN.tbl$YearFactor, n = select(NN.tbl, contains('.gt')) %>% rowSums()) %>%
    group_by(t) %>%
    summarise(any.valid = sum(n, na.rm=T) > 0)
  svy.yrs = as.numeric(as.character(N.valid$t[N.valid$any.valid]))
  svy.yr.range = factor(min(svy.yrs):max(svy.yrs))

  # TODO: these are both all 0; as expected?
  ZZ.prior = createZZPrior(.data, svy.yr.range, XX, .opts)
  ZZ = createZZ(.data, svy.yr.range, XX, .opts)

  # T gives the number of time periods
  T <- length(svy.yr.range)
  # Q gives the number of questions
  Q = sum(grepl('.gt', colnames(.data$data), fixed=T))
  # G gives the number of covariate (?) combinations
  G <- nlevels(group)

  # Generate factor levels for combinations of demographic variables
  if (is.null(.opts$demo.vars)) {
    demo.group <- gl(1, nrow(xtab))
  } else {
    demo.group <- interaction(as.list(xtab[, .opts$demo.vars, drop=FALSE]))
  }
  # Gnat gives the number of combinations
  Gnat <- nlevels(demo.group)

  # Calculate weights
  xtab.ds <- svydesign(~1, probs=1, data=xtab.yr)
  nat.wts <- 1/xtab.ds$prob
  WT = createWT(.nat.wts = nat.wts,
    .G = G,
    .T = T,
    .Gnat = Gnat,
    .demo.group = demo.group,
    .XX = XX)

  NN.nat = NN.tbl %>%
    mutate_each(funs(rep(0, length(.))), contains('.gt'))
  # TODO: confirm Q should be ncol(contains('.gt')) and not length(q) and not
  # length(.opts$q.vars)
  # TODO: nat_only is set to false for all groups here, but presumably this
  # should be exposed to the user
  # Quick fix for missing rownames in nat_only
  nat_only = NN.tbl %>%
    group_by(YearFactor) %>%
    summarise_each(funs(c(0)), contains('.gt'))
  rownames(nat_only) = nat_only$YearFactor
  nat_only = nat_only %>%
    select(-YearFactor) %>%
    as.matrix(nat_only, rownames.force=T)

  # Calculate N, including groups with data that aren't national-only
  N = full_join(
      melt(NN.tbl, id.vars = covs.yr) %>%
          mutate(n.nonzero = value > 0) %>%
          as.tbl(),
      melt(NN.nat, id.vars = covs.yr) %>%
        as.tbl() %>%
        mutate(not.nat = !value),
      by = c(covs.yr, 'variable')) %>%
    summarise(N = sum(n.nonzero & not.nat)) %>%
    unlist()

  NNnat = NN.nat %>%
    melt(id.vars = covs.yr) %>%
    group_by(YearFactor, Gender, variable) %>%
    summarise(country.sum = sum(value)) %>%
    mutate(country.sum = as.integer(country.sum)) %>%
    acast(YearFactor ~ variable ~ Gender,
      value.var = 'country.sum')

  # TODO: this is what happens in the original code, but then we don't have
  # any group-level test data
  SSnat = NNnat

  NN.tbl %>%
    mutate_each(funs(. == 0), contains('.gt')) %>%
    melt(id.vars = covs.yr) %>%
    group_by(YearFactor, Gender, variable) %>%
    summarise(country.sum = sum(value) == 0) %>%
    mutate(country.sum = as.integer(country.sum)) %>%
    acast(YearFactor ~ variable ~ Gender,
      value.var = 'country.sum')

  ns.long = left_join(
      melt(NN.tbl, id.vars = covs.yr, value.name = 'n.grp'),
      melt(SS, id.vars = covs.yr, value.name = 's.grp'),
      by = c('Country', 'Gender', 'YearFactor', 'variable')) %>%
    arrange(variable, YearFactor, desc(Gender), Country) %>%
    filter(!is.na(n.grp) & n.grp != 0) %>%
    mutate(name = str_c(Country, Gender, YearFactor, sep = '__')) %>%
    mutate(name = str_c(name, variable, sep = ' | '))

  # Create missingness indicator array
  # MMM <- array(1, dim=list(T, Q, G))
  MMM = ns.long %>%
    mutate(m.grp = as.integer(is.na(n.grp) | n.grp == 0)) %>%
    select(-s.grp) %>%
    acast(YearFactor ~ variable ~ Country + Gender, value.var = 'm.grp',
      fill = 1)

  n.vec = unlist(ns.long$n.grp)
  s.vec = unlist(ns.long$s.grp)
  names(n.vec) = ns.long$name
  names(s.vec) = ns.long$name

  # Return stan data
  list(
    n_vec = n.vec,      # response counts
    s_vec = s.vec,      # group counts
    NNnat = NNnat,
    SSnat = SSnat,
    XX = XX[, -1],
    ZZ = ZZ[, -1, , drop = FALSE],      # geographic predictors
    ZZ_prior = ZZ.prior[, -1, , drop = FALSE],
    MMM = MMM,             # missingness array (T x Q x G)
    G = G,                 # number of covariate groups
    Q = Q,                 # number of questions (items)
    T = T,                 # number of time units (years)
    N = N,                 # number of observed group-question cells
    P = ncol(XX[, -1]),    # number of hierarchical parameters
    S = dim(ZZ)[[2]] - 1,  # number of geographic units
    H = dim(ZZ)[[3]],      # number of geographic-level predictors
    Hprior = dim(ZZ.prior)[[3]],
    separate_years = .opts$separate.years,    # if 1, no pooling over time
    constant_item = .opts$constant.item,      # if 1, difficulties constant
    D = ifelse(.opts$constant.item, 1, T),
    WT = WT,                            # weight matrix for calculating national mean
    nat_only = nat_only,
    Gnat = Gnat)                        # number of national-level groups
}

runStan = function(
  # TODO: pick sane defaults
    stan.code,
    stan.data,
    n.iter = 2e3,
    n.chain = 2,
    max.save = 2e3,
    n.warm = min(1e4, floor(n.iter * 3/4)),
    n.thin = ceiling((n.iter - n.warm) / (max.save / n.chain)),
    init.range = 1,
    seed = 1,
    pars.to.save = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
        "nu_geo", "nu_geo_prior", "kappa", "sd_item",
        "sd_theta", "sd_theta_bar", "sd_gamma",
        "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
        "sd_total", "theta_nat", "var_theta_bar_nat"),
    parallel = TRUE) {

  ## date()
  ## system.time(
  ##   # TODO: make test run optional; expose arguments for test run
  ##   stan.test <- stan(model_code=stan.code, data=stan.data, chains=1, iter=10,
  ##     pars=pars.to.save, verbose=FALSE, seed=1,
  ##     init = "random", init_r = init.range)
  ##   )
  ## date()

  # FIXME: update to extract these successfully
  # stopifnot(all.equal(prod(test_sds = extract(stan.test, pars="sd_item")$sd_item), 1))
  # stopifnot(all.equal(exp(mean(test_diff = as.vector(extract(stan.test, pars="kappa")$kappa) / test_sds)), 1))

  cat(str_wrap(str_c(
    "Running ", n.iter
    , " iterations in each of ", n.chain,
    " chains, thinned at an interval of ", n.thin, ", with ", n.warm,
    " adaptation iterations over the years ", rownames(stan.data$ZZ)[1],
    "-", rownames(stan.data$ZZ)[length(rownames(stan.data$ZZ))], ".")))

  # TODO: Output this?
  # qs.used
  # covs
  # txt.out

  ## Change settings to run in parallel
  rstan_options(auto_write = parallel)
  if (parallel) {
    options(mc.cores = parallel::detectCores())
  }

  cat("\nStart: ", date(), "\n")
  stan.out <- stan(model_code = stan.code, data = stan.data, iter = n.iter,
                   chains = n.chain, warmup = n.warm, thin = n.thin,
                   verbose = FALSE, refresh = max(floor(n.iter/100), 1),
                   pars = pars.to.save, seed = seed,
                   init = "random", init_r = init.range)
  cat("\nEnd: ", date(), "\n")
  return(stan.out)
}
