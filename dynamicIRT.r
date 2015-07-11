# Define the user-exposed functions for dynamic IRT
# Created 6/15/15
# J.Dunham
#
# TODO: filestring

## Dependencies ##

library(rstan)
# TODO: only load parallel if more than one thread requested in runStan()
library(parallel)
library(reshape2)
# TODO: confirm dependency
library(car)
# TODO: rewrite the remaining plyr calls with dplyr
library(plyr)
library(survey)
# TODO: rewrite in base R
library(stringr)
# TODO: confirm (use tidry, already a dplyr dependency, instead?)
library(data.table)
# TODO: confirm
library(rms)
# TODO: confirm (use dplyr instead?)
library(abind)
library(dplyr)
library(tidyr)

## Internal functions ##

source('funs.r')
source('stan.code.r')

## User-exposed functions ##

checkData = function(.data, .opts) {
  # TODO: docstring

  # Convert to tidyr tbl_df for dplyr
  .data = as.tbl(.data)

  # Check that variables given as strings exist
  all.vars = c(.opts$q.vars, .opts$fm.vars, .opts$time.var)
  if (!all(all.vars %in% colnames(.data))) {
    stop(paste("Not found in .data:",
        str_c(names(all.vars[!(all.vars %in% colnames(data))]) , collapse=", ")))
  }

  # Get levels of the time variable, handling the possibility that it isn't a factor
  # TODO: could implement a verbosity argument
  if (is.factor(.data[[.opts$time.var]])) {
    yr.range = as.numeric(levels(.data[[.opts$time.var]]))
  } else {
    yr.range = as.numeric(unique(.data[[.opts$time.var]]))
  }
  cat("\n\n")
  cat("Time variable has", length(yr.range), "levels: ")
  cat(yr.range, sep=", ")

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
  none.valid = rowSums(!is.na(.data[, .opts$q.vars])) == 0
  cat("\n\n")
  cat(sum(none.valid), "rows out of", nrow(.data), "have no valid responses.")

  # Check for missingness in frontmatter vars
  # TODO: do anything? just report for now
  row.missing.fm = rowSums(is.na(select(.data, one_of(.opts$fm.vars)))) > 0
  colsum.na.fm = colSums(is.na(select(.data, one_of(.opts$fm.vars))))
  if (any(row.missing.fm)) {
    cat("\n\n")
    cat(sum(row.missing.fm), "rows out of", nrow(.data), "have missingness in frontmatter variables. Proportion missing by variable:\n\n")
    print(round(colsum.na.fm/nrow(.data), 2))
  }

  # Check whether variables satisfy min.years requirement
  yrs.asked = .data %>%
    group_by_(.opts$time.var) %>%
    summarise_each(funs(anyValid), one_of(.opts$q.vars))
  valid.yrs = colSums(select(yrs.asked, -one_of(.opts$time.var)))
  yrs.invalid = names(valid.yrs[valid.yrs < .opts$min.years])
  cat("\n")
  cat(sum(valid.yrs < .opts$min.years), "response variables out of", (length(valid.yrs)), "fail min.years requirement.")
  cat(yrs.invalid, sep=", ")

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
  cat(length(lt.min.polls), 'questions appear in fewer than the minimum polls per question, which is set to', .opts$min.polls, '\n')
  cat(names(lt.min.polls)[lt.min.polls], sep = ',')
  cat('\nDropped', length(lt.min.polls), 'questions for appearing in too few polls.\n\n')
  if (!.opts$test & length(lt.min.polls) > 0) {
    .data = select(.data, -one_of(lt.min.polls))
  } else if (.opts$test) {
    cat("Finished test.")
  }

  # NOTE: For comparison, this is the original code producing the output shown in comments.
  # years.asked <- matrix(NA, nrow = length(yr.range), ncol = length(.opts$q.vars),
  #   dimnames = list(yr.range, .opts$q.vars))
  # for (q in 1:length(.opts$q.vars)) {
  #   years.asked[, q] <- tapply(poll.df[, .opts$q.vars[q]], poll.df$YearFactor, anyValid)
  #   years.asked[, q][is.na(years.asked[, q])] <- FALSE
  # }
  # (.opts$q.vars <- .opts$q.vars[colSums(years.asked[, .opts$q.vars], na.rm=TRUE) >= min.years])
  # poll.df <- subset(poll.df,, c(.opts$fm.vars, .opts$q.vars))
  # dim(poll.df)
  #
  # years.asked.melt <-
  #   melt(data.frame(years.asked[, .opts$q.vars],
  #       YearFactor = as.integer(rownames(years.asked))),
  #     id="YearFactor")
  # poll.df$survey.year <-
  #   interaction(poll.df$YearFactor, poll.df$survey, drop=TRUE, lex.order=TRUE)
  # forms.asked <-
  #   daply(data.frame(!is.na(poll.df)), ~poll.df$survey.year, colSums) > 0
  # # poll.df$survey.year taxhigher taxrich1 unempjob survey.year
  # #      2008.ess           FALSE    FALSE    FALSE        TRUE
  # #      2008.evs           FALSE    FALSE     TRUE        TRUE
  # #      2009.eb_emp09      FALSE    FALSE    FALSE        TRUE
  # #      2009.eb_pov09       TRUE    FALSE    FALSE        TRUE
  # #      2009.eb_val09      FALSE    FALSE    FALSE        TRUE
  # #      2009.evs           FALSE    FALSE     TRUE        TRUE
  # #      2009.issp_ineq     FALSE     TRUE    FALSE        TRUE
  # #      2010.eb_pov10       TRUE    FALSE    FALSE        TRUE
  # #      2010.ess           FALSE    FALSE    FALSE        TRUE
  # #      2010.issp_env      FALSE    FALSE    FALSE        TRUE
  # forms.asked[, .opts$q.vars] <- forms.asked[, .opts$q.vars] * apply(forms.asked[, .opts$q.vars], 1, sum)
  # poll.df$survey.year taxrich1 unempjob survey.year
  # # 2008.ess              0        0           1
  # # 2008.evs              0        9           1
  # # 2009.eb_emp09         0        0           1
  # # 2009.eb_pov09         0        0           1
  # # 2009.eb_val09         0        0           1
  # # 2009.evs              0        9           1
  # # 2009.issp_ineq        4        0           1
  # # 2010.eb_pov10         0        0           1
  # # 2010.ess              0        0           1
  # # 2010.issp_env         0        0           1
  # .opts$q.vars <- .opts$q.vars[colSums(forms.asked[, .opts$q.vars], na.rm=TRUE) >= min.polls]
  # forms.asked.melt <- melt(forms.asked[, .opts$q.vars])
  # # names(forms.asked.melt) <- c('Poll', 'Question', 'nQuestions')
  # # 1         2008.ess     busown          0
  # # 2         2008.evs     busown          9
  # # 3    2009.eb_emp09     busown          0
  # # 4    2009.eb_pov09     busown          0
  # # 5    2009.eb_val09     busown          0
  # # 6         2009.evs     busown          9

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

# TODO: need to update .opts variables after running cleanData(), to this effect:
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

  is.original("covs")
  is.original("covs.yr")

  # Drop rows lacking covariates or time variable
  .data$data = filter(.data$data, rowSums(is.na(select(.data$data, one_of(covs.yr)))) == 0)

  # Create .gt. variables
  .data$data = createGT(d = .data$data, .q.vars = .data$q.vars)
  identical(colnames(select(.data$data, contains('.gt'))), get('gt.vars.use', envir=orig))

  # Drop rows lacking at least one .gt variable
  .data$data = filter(.data$data, rowSums(!is.na(select(.data$data, contains('.gt')))) > 0)

  # Fix levels of geo variable after filtering
  .data$data[, .data$geo.var] <- droplevels(.data$data[, .data$geo.var])
  # Fix levels of covariates after filtering
  .data$data[, covs.yr] <- droplevels(.data$data[, covs.yr])

  # Represent covariates in data as model frame
  MF <- model.frame(f0, data=.data$data, na.action=return)
  MF.yr <- model.frame(f0.yr, data=.data$data, na.action=return)

  # Create factors with levels for each combination of covariate values
  group <- interaction(as.list(MF), sep="__", lex.order=FALSE)
  group.yr <- interaction(as.list(MF.yr), sep="__", lex.order=FALSE)

  identical(unique(group), unique(get("group", envir = orig)))

  # Get frequency counts of factor combinations (reordered in factor order)
  xtab.yr <- as.data.frame(table(MF.yr))
  # TODO: why does the original code subset to the first year here?
  xtab <- subset(xtab.yr, YearFactor == .data$yr.range[1], -c(YearFactor, Freq))
  # Check that group.yr levels are in same order as rows of xtab.yr
  stopifnot(identical(levels(group.yr), unname(apply(subset(xtab.yr,, -Freq), 1, paste, collapse="__"))))
  # Create a row index
  xtab.yr$xtrow <- 1:nrow(xtab.yr)

  identical(xtab, get('xtab', envir = orig))
  identical(xtab.yr, get('xtab.yr', envir = orig))

  # Get dummy variable representation of contingency table
  XX <- model.matrix(f0, xtab)
  # Don't use hierarchical model if we only have one predictor
  if (length(covs) == 1) XX <- matrix(0, nrow(XX), ncol(XX))

  is.original('XX')

  # Create matrix of responses
  # YY <- select(.data$data, contains('.gt')) %>% as.matrix(.)

  # Create a variable counting number of responses
  .data$data$n.responses = rowSums(!is.na(select(.data$data, contains('.gt')) %>% as.matrix(.)), na.rm=T)

  # Check
  identical(.data$data$n.responses, unname(get('n.responses', envir=orig)))

  # FIXME: start original code
  YY <- select(.data$data, contains('.gt')) %>% as.matrix(.)
  (gt.vars <- sort(names(.data$data)[grep(".gt", names(.data$data))]))
  n.responses <- rowSums(!is.na(YY), na.rm=TRUE)
  # This has same dims as YY, and gives NA or not for each cell
  valid.gt <- !is.na(.data$data[, gt.vars])
  # We'll use the rows that aren't missing any covariates and have at least one question response
  use.rows <- rowSums(is.na(.data$data[, covs.yr])) == 0 & rowSums(valid.gt) >= 1
  fm.vars <- c(names(select(.data$data, YearFactor:age))) ## front matter
  # The deff table will have a "row" column giving a row number; a column for n.responses, and all the frontmatter variables
  de.df <- data.frame(row = 1:sum(use.rows), Nresponses = n.responses, .data$data[use.rows, fm.vars])
  # Split by combinations of covariates, and then calculate design effects
  # using the vector of weights for the observations in each split.
  de.df <- group_by_(de.df, .dots = lapply(covs.yr, as.symbol)) %>%
  mutate(de = 1 + (sd(useweight) / mean(useweight)) ^ 2,
    de = ifelse(is.na(de), 1, de))
  # Arrange by row number
  de.df <- plyr::arrange(de.df, row)
  de.df
  # end original code

  # This was a 176K x 20 matrix in which a design effect was attached to
  # each respondent. But the design effects are unique to combinations of
  # the identifiers in covs.yr (here Country, Gender, and YearFactor), so
  # there are fewer than two dozen values. Instead, create a table with
  # the design effect for each combination.

  # Create table of design effects
  de.tbl = .data$data %>% group_by_(.dots = covs.yr) %>%
    summarise(def = summariseDef(useweight))

  # Check
  identical(sort(unique(round(get("de.df", envir = orig)$de, 6))),
    sort(unique(round(de.tbl$def, 6))))

  # Create table of trial counts
  NN.tbl = .data$data %>%
    group_by_(.dots = covs.yr) %>%
    select(n.responses, contains(".gt")) %>%
    mutate_each(funs(notNA), vars = contains(".gt")) %>%
    mutate_each(funs(. / n.responses), vars = matches("^vars\\d{1,}$")) %>%
    full_join(de.tbl, by = covs.yr) %>%
    summarise_each(funs(ceiling(sum(. / def))), vars = matches("^vars\\d{1,}$")) %>%
    ungroup()

  # # Check:
  # NN.orig = get('NN', envir = orig)
  # NN.orig = data.frame(NN.orig[order(rownames(NN.orig)), ])
  # NN.orig$factor = rownames(NN.orig)
  # NN.orig = separate(NN.orig, factor, c("Country", "Gender", "YearFactor"), sep = "__")
  # rownames(NN.orig) = NULL
  # NN.orig = as.tbl(NN.orig) %>% group_by(Country, Gender, YearFactor) %>% arrange(Country, Gender, YearFactor)
  # NN.orig = as.tbl(bind_cols(NN.orig[, 114:116], NN.orig[, 1:113]))
  # NN.orig = NN.orig[!apply(NN.orig[, -c(1:3)], 1, allNA), ]
  # check.orig = ungroup(NN.orig) %>% mutate_each(funs(as.character), Country, Gender, YearFactor) %>% arrange(Country, Gender, YearFactor)
  # check.new = ungroup(NN.tbl) %>% mutate_each(funs(as.character), Country, Gender, YearFactor) %>% arrange(Country, Gender, YearFactor)
  # colnames(check.new) = colnames(check.orig)
  # identical(check.orig, check.new)

  # TODO:
  # Check: in the new NN, six all-NA covariate combinations are excluded; not in the original NN.
  # Should these cells be zero instead?

  # Create table of success counts
  # SS = createSS(YY, de.df, covs.yr, xtab.yr, group.yr)
  y.bar.star = .data$data %>%
    group_by_(.dots = covs.yr) %>%
    select(useweight, n.responses, contains(".gt")) %>%
    summarise_each(funs(weighted.mean(., useweight / n.responses, na.rm=T)), vars = contains(".gt")) %>%
    ungroup()

  # # TODO: rename the 'vars' variables as 'gt'
  identical(NN.tbl[, 1:3], y.bar.star[, 1:3])
  SS = select(NN.tbl, contains('vars')) * select(y.bar.star, contains('vars'))
  SS = bind_cols(NN.tbl[, 1:3], SS)
  SS = SS %>% mutate_each(funs(round(., digits=0)), vars = contains("vars"))
  SS = arrange(SS, Country, Gender, YearFactor)

  # Check
  # FIXME: these are all slightly different; I think the new SS is sorted differently
  SS.orig = get('SS', envir = orig)
  SS.orig = data.frame(SS.orig[!apply(SS.orig, 1, allNA), ])
  SS.orig$factor = rownames(SS.orig)
  SS.orig = separate(SS.orig, factor, c("Country", "Gender", "YearFactor"), sep = "__")
  rownames(SS.orig) = NULL
  SS.orig = as.tbl(SS.orig) %>% group_by(Country, Gender, YearFactor) %>% arrange(Country, Gender, YearFactor)
  SS.orig = as.tbl(bind_cols(SS.orig[, 114:116], SS.orig[, 1:113]))
  SS.orig = arrange(SS.orig, Country, Gender, YearFactor)
  SS.orig[1:10, 1:6]
  SS[1:10, 1:6]

  View(SS)
  View(SS.orig)

      SS[NN.tbl$Gender == "Male", 4:116] == SS.orig[SS.orig$Gender == 'Male', -c(1:3)]
  SS[1:10, 1:5]
  SS.orig[1:10, 1:5]
  dim(SS)
  dim(SS.orig)

  # There should be no more successes than trials
  # stopifnot(all(SS - select(NN, contains('vars')) <= 0))

  # Grab "greater than" variable names
  qs.used = str_subset(colnames(.data$data), '\\.gt')

  N.valid = data_frame(t = NN$YearFactor, n = select(NN, contains('var')) %>%
    rowSums()) %>%
    group_by(t) %>%
    summarise(any.valid = sum(n, na.rm=T) > 0)
  svy.yrs = as.numeric(as.character(N.valid$t[N.valid$any.valid]))
  svy.yr.range = min(svy.yrs):max(svy.yrs)

  # TODO: these are both all 0; as expected?
  ZZ.prior = createZZPrior()
  ZZ = createZZ()

  # T gives the number of time periods
  T <- length(svy.yr.range)
  # Q gives the number of questions
  Q = sum(grepl('.gt', colnames(.data$data), fixed=T))
  # G gives the number of covariate (?) combinations
  G <- nlevels(group)

  # Generate factor levels for combinations of demographic variables
  if (is.null(.data$demo.vars)) {
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

  # TODO: confirm difference between NN.nat and NNnat
  NN.nat <- array(FALSE, dim=dim(NN), dimnames=dimnames(NN))
  nat_only <- array(0, dim=c(T, Q), dimnames=list(svy.yr.range, qs.used))

  # groups with data and not national-only
  (N <- sum(select(NN, contains('vars')) > 0 & !NN.nat))
  NNnat <- SSnat <- array(0, c(T, Q, Gnat), list(svy.yr.range, qs.used, levels(demo.group)))

  # Create vectors of trials and successes
  n_vec <- s_vec <- integer(N)
  names(n_vec) <- names(s_vec) <- seq_len(N)

  # Create missingness indicator array
  MMM <- array(1, dim=list(T, Q, G))

  # TODO: Unsure what exactly is happening here. We're updating the objects
  # created in the last few lines. Pass out to a subfunction?
  pos <- 0
  for (yr in svy.yr.range) {
    if (!yr %in% svy.yrs) next
    # FIXME: this was a quick fix; clean up
    t = which(svy.yr.range == yr)
    yr.obs <- grep(yr, rownames(NN))
    for (q in 1:Q) {
      # if nation only
      # FIXME: check original line; getting "invalid subscript type 'closure'"
      if (nat_only[paste(yr), q]) {
        print(c(t, q))
        for (h in seq_len(Gnat)) {
          gn.obs <- demo.group == levels(demo.group)[h]
          (wt.mn <- weighted.mean(SS[yr.obs, q] / NN[yr.obs, q], WT[t, h, ], na.rm=TRUE))
          (NNnat[t, q, h] <- ceiling(sum(NN[yr.obs[gn.obs], q])))
          (SSnat[t, q, h] <- round(wt.mn * NNnat[t, q, h]))
        }
      } else {
        for (g in 1:G) {
          # skip if no data
          if (NN[yr.obs[g], q] == 0) next
          pos <- pos + 1
          # mark as not missing
          MMM[t, q, g] <- 0
          n_vec[pos] <- NN[yr.obs[g], q]
          s_vec[pos] <- SS[yr.obs[g], q]
          names(n_vec)[pos] <- names(s_vec)[pos] <- paste(rownames(NN)[yr.obs[g]], colnames(NN)[q], sep=" | ")
        }
      }
    }
  }

  # If using the pos counter, we should've iterated over all N
  # if (!nat_only[t, q]) stopifnot(identical(as.integer(pos), N))

  # Report out some statistics (TODO: user-facing explanations)
  cat("Count by 5-percentile bins:\n")
  cat(quantile(n_vec, seq(.05, .95, .05)), '\n\n')
  cat(table(n_vec), '\n\n')
  cat(table(s_vec), '\n\n')
  cat(mean(!MMM), '\n\n')

  # Return stan data
  list(
    n_vec = n_vec,      # response counts
    s_vec = s_vec,      # group counts
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
  # TODO: expose stan.data and stan.code arguments
  n.iter = 2e3,
  n.chain = 2,
  max.save = 2e3,
  n.warm = min(1e4, floor(n.iter * 3/4)),
  n.thin = ceiling((n.iter - n.warm) / (max.save / n.chain)),
  init.range = 1,
  pars.to.save = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
    "nu_geo", "nu_geo_prior", "kappa", "sd_item",
    "sd_theta", "sd_theta_bar", "sd_gamma",
    "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
    "sd_total", "theta_nat", "var_theta_bar_nat")) {

  date()
  system.time(
    # TODO: make test run optional; expose arguments for test run
    stan.test <- stan(model_code=stan.code, data=stan.data, chains=1, iter=10,
      pars=pars.to.save, verbose=FALSE, seed=1,
      init = "random", init_r = init.range)
    )
  date()

  stopifnot(all.equal(prod(test_sds <- extract(stan.test, pars="sd_item")$sd_item), 1))
  stopifnot(all.equal(exp(mean(test_diff <- as.vector(extract(stan.test, pars="kappa")$kappa) / test_sds)), 1))

  cat(str_wrap(str_c(
    "Running ", n.iter, " iterations in each of ", n.chain,
    " chains, thinned at an interval of ", n.thin, ", with ", n.warm,
    " adaptation iterations over the years ", rownames(stan.data$ZZ)[1],
    "-", rownames(stan.data$ZZ)[length(rownames(stan.data$ZZ))], ".")))

  # TODO: Output this?
  qs.used
  covs
  txt.out

  date()
  system.time(
    # FIXME: IIRC parallel::mclapply isn't available to Windows users
    stan.par <- mclapply(1:n.chain, mc.cores = n.chain, FUN = function(chain) {
      cat('\nStarting chain', chain, '\n')
      out <- stan(model_code = stan.code, data = stan.data, iter = n.iter,
        chains = 1, warmup = n.warm, thin = n.thin, verbose = FALSE,
        chain_id = chain, refresh = max(floor(n.iter/100), 1),
        pars = pars.to.save, seed = chain, init = "random",
        init_r = init.range
        )
      cat('Ending chain', chain, '\n\n')
      return(out)
      }))
  date()
}
