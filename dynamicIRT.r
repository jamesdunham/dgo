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

  # TODO: check argument types

  # TODO: confirm which strings must exist as variables, if specified
  # Check that variables given as strings exist
  if (!(.opts$t.var %in% colnames(.data))) {
    cat("Didn't find a time variable called", .opts$t.var,
      "in the individual data.")
  }
  if (!is.null(.group.data) & !(.opts$t.var %in% colnames(.group.data))) {
    cat("Didn't find a time variable called", .opts$t.var,
      "in the group data.")
  }
  if (!all(.opts$fm.vars %in% colnames(.data))) {
    cat("Didn't find a frontmatter variable in the individual data:",
      str_c(setdiff(.opts$fm.vars, colnames(.data)), collapse = ", "), "\n")
  }
  if (!all(.opts$q.vars %in% colnames(.data))) {
    cat("Didn't find a question variable in the individual data:",
      str_c(setdiff(.opts$q.vars, colnames(.data)), collapse = ", "), "\n")
  }
  if (!all(.opts$demo.vars %in% colnames(.data))) {
    cat("Didn't find a demographic variable in the individual data:",
      str_c(setdiff(.opts$demo.vars, colnames(.data)), collapse = ", "), "\n")
  }

  # Get levels of the time variable, handling the possibility that it isn't a factor
  if (is.factor(.data[[.opts$t.var]])) {
    t.which.observed = as.numeric(levels(.data[[.opts$t.var]]))
  } else {
    t.which.observed = as.numeric(unique(.data[[.opts$t.var]]))
  }
  cat("\nTime variable has", length(t.which.observed), "levels in the data.")

  # NOTE: instead of svy.year.range, etc., now using the vector of t.which.observed
  # and an argument for which to estimate

  # Report missing levels
  # t.missing = setdiff(seq(t.which.observed.range['min'], t.which.observed.range['max']),
  #   svy.periods)
  t.missing = setdiff(.opts$use.t, t.which.observed)
  if (length(t.missing) > 0) {
    cat("\n\nNot all periods specified in t.use appear in the data:", str_c(t.missing, collapse = ', '))
  } else {
    cat("\n\nAll periods specified in t.use appear in the data.")
  }

  # Check for question columns with no valid responses
  q.all.missing = names(.data)[colSums(!is.na(.data)) == 0]
  if (length(q.all.missing) > 0) {
    cat("\n\n")
    cat(length(q.all.missing), "response variables are entirely missing: ")
    cat(q.all.missing, sep=", ")
    # Drop the invalid variables from the local vector of question variable
    # names. (Data untouched.)
    .opts$q.vars = sort(setdiff(.opts$q.vars, q.all.missing))
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
    cat(sum(row.missing.fm), "rows out of", nrow(.data), "have missingness in frontmatter variables.\n")
    cat("Proportion missing by variable:\n\n")
    print(round(colsum.na.fm/nrow(.data), 2))
  }

  # Check whether variables satisfy min.years requirement
  q.when.asked = .data %>%
    group_by_(.opts$t.var) %>%
    summarise_each(funs(anyValid), one_of(.opts$q.vars))
  q.t.count = colSums(select(q.when.asked, -one_of(.opts$t.var)))
  q.rare = names(q.t.count[q.t.count < .opts$min.t])
  cat("\n")
  cat(sum(q.t.count < .opts$min.t), "response variables out of", (length(q.t.count)), "fail min.t requirement, which is", .opts$min.t)
  cat(q.rare, sep=", ")

  # If this wasn't a dry run (.opts$test = TRUE), attempt to fix problems
  if (!.opts$test) {
    # Drop rows with no valid question responses
    # TODO: none.valid is a logical vector corresponding to rows in .data as
    # provided; musn't manipulate them above. It would be safer to create
    # none.valid as a new variable in .data.
    .data = filter(.data, !none.valid)
    cat("\n\nDropped", sum(none.valid), "rows for lack of any responses.")

    # Drop question columns with no valid responses
    if (length(q.all.missing) > 0) {
      .data = select(.data, -one_of(q.all.missing))
    }
    cat("\n\nDropped", length(q.all.missing), "questions for lack of valid responses:",
      str_c(q.all.missing, collapse = ", "))

    # Drop variables that don't satisfy min.years requirement
    if (length(q.rare) > 0) {
      .data = select(.data, -one_of(q.rare))
    }
    cat("\n\nDropped", length(q.rare), "more questions for failing min.t requirement.")
  }

  # TODO: could re-check the data after attempting to fix problems here

  ## Create tables summarizing the data ##

  # For output, melt q.when.asked for a long summary table of which questions
  # appear in which periods
  q.when.asked = melt(q.when.asked, id=.opts$t.var)

  # Create a factor in the data for the combinations of surveys and the time
  # variable (e.g. "2008.ess")
  # TODO: as written this factor is created as "poll.year.factor" and there
  # musn't be an existing variable with that name. Could instead provide the
  # option of creating the factor, or use a specified existing variable.
  .data = .data %>% unite(poll.year.factor, one_of(.opts$poll.var, .opts$t.var), sep = ".", remove = F)
  .data = .data %>% mutate(poll.year.factor = factor(poll.year.factor))

  # And use it to create a summary table for whether a question appeared in
  # combinations of periods t and polls. This table replaces forms.asked in the
  # original code.
  q.which.observed = .data %>%
    group_by(poll.year.factor) %>%
    select(one_of(.opts$q.vars)) %>%
    summarise_each(funs(anyValid))

  # Final check: do questions appear in the minimum number of survey-periods?
  q.counts = q.which.observed %>%
    select(-poll.year.factor) %>%
    summarise_each(funs(sum))
  lt.min.polls = colnames(q.counts)[unlist(q.counts) < .opts$min.poll]
  cat('\n\n')
  cat(length(lt.min.polls), 'questions appear in fewer than the minimum polls per question, which is set to', .opts$min.poll, '\n')
  cat(str_c(lt.min.polls, collapse = ", "))
  cat('\nDropped', length(lt.min.polls), 'questions for appearing in too few polls.\n\n')
  if (!.opts$test & length(lt.min.polls) > 0) {
    .data = select(.data, -one_of(lt.min.polls))
  } else if (.opts$test) {
    cat("Finished test.\n\n")
  }

  # Make all the variables given as strings factors
  .data = .data %>%
    mutate_each_(funs(factor), vars = c(.opts$t.var, .opts$demo.vars,
        .opts$geo.var, .opts$geo.mod.vars, .opts$geo.mod.prior.vars, .opts$poll.var))

  out = list(
    data = .data,
    summary = list(
      q.when.asked = q.when.asked,
      q.all.missing = q.all.missing,
      q.counts = q.counts,
      t.which.observed = t.which.observed,
      q.which.observed = q.which.observed),
    varnames = list(
      fm.vars = .opts$fm.vars,
      q.vars = .opts$q.vars,
      t.var = .opts$t.var,
      poll.var = .opts$poll.var,
      geo.var = .opts$geo.var,
      weight.var = .opts$weight.var,
      demo.vars = .opts$demo.vars,
      geo.mod.vars = .opts$geo.mod.vars,
      geo.mod.prior.vars = .opts$geo.mod.prior.vars),
    params = list(
      use.t = .opts$use.t,
      min.t = .opts$min.t,
      min.poll = .opts$min.poll,
      separate.t = .opts$separate.t,
      constant.item = .opts$constant.item))

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
  # .data = abort.checked.data
  # .opts = abort.test.opts
  # .data = eu.checked.data
  # .opts = eu.test.opts

  ## RESPONSES ##

  # NOTE: working with character variable names in dplyr is proving tricky, so
  # create some temporary variables at the outset instead using the variables
  # specified as strings
  .data$data = .data$data %>%
    mutate_(
      dirt.weight = .data$varnames$weight.var,
      dirt.poll = .data$varnames$poll.var,
      dirt.t = .data$varnames$t.var,
      dirt.geo = .data$varnames$geo.var)
  .data$data = .data$data %>%
    mutate(dirt.demo = interaction(.[, .opts$demo.vars], drop = T))

  # These variables are from the original code; use them for now rather than
  # refactoring.
  # TODO: make consistent use of checked.data slots versus .opts
  # covs = c("dirt.geo", "dirt.demo")
  # covs.t = c(covs, "dirt.t")
  # f0 = Formula(c("0", covs))
  f0 = Formula(c("0", "dirt.geo", "dirt.demo"))
  # f0.t = Formula(c("0", covs, "dirt.t"))
  f0.t = Formula(c("0", "dirt.geo", "dirt.demo", "dirt.t"))

  # Drop rows lacking covariates or time variable
  .data$data = .data$data %>%
    mutate(missing.geo.demo.t = is.na(dirt.geo) | is.na(dirt.demo) | is.na(dirt.t)) %>%
    filter(!missing.geo.demo.t)
  stopifnot(nrow(.data$data) > 0)

  # Create .gt. variables
  .data$data = createGT(d = .data$data, .q.vars = .data$varnames$q.vars)
  # TODO: check for whether any variables were created

  # Drop rows lacking at least one .gt variable
  .data$data = .data$data %>%
    filter(rowSums(!is.na(select(.data$data, contains('.gt')))) > 0)
  # TODO: check that length > 0

  # Fix levels of variables after filtering
  .data$data$dirt.geo = droplevels(.data$data$dirt.geo)
  .data$data$dirt.demo = droplevels(.data$data$dirt.demo)
  # .data$data$demo.group = droplevels(.data$data$demo.group)

  # Represent covariates in data as model frame
  MF = model.frame(f0, data=.data$data, na.action=return)
  MF.t = model.frame(f0.t, data=.data$data, na.action=return)

  # Get frequency counts of factor combinations (reordered in factor order)
  xtab.t = as.data.frame(table(MF.t))
  # FIXME: why does the original code subset to the first year here?
  xtab = xtab.t %>%
    subset(dirt.t == .data$summary$t.which.observed[1]) %>%
    select(-dirt.t, Freq)

  # Get dummy variable representation of contingency table
  XX <- model.matrix(f0, xtab)
  # Don't use hierarchical model if we only have one predictor
  if (length(f0) == 1) {
    XX = matrix(0, nrow(XX), ncol(XX))
  }

  # Create a variable counting the number of responses given by each
  # respondent in the data
  .data$data$n.responses = rowSums(
    !is.na(select(.data$data, contains('.gt')) %>% as.matrix(.)),
    na.rm=T)

  ## GROUPS ##

  # de.df was a 176K x 20 matrix in which a design effect was attached to
  # each respondent. But the design effects are unique to combinations of
  # the identifiers in covs.t (here Country, Gender, and YearFactor), so
  # there are fewer than two dozen values. Instead, create a table with
  # the design effect for each combination.

  # Create table of design effects
  de.tbl = .data$data %>%
    group_by(dirt.geo, dirt.demo, dirt.t) %>%
    select(dirt.weight) %>%
    summarise(def = summariseDef(dirt.weight))

  # Create table of (adjusted) trial counts by geographic, demographic, and
  # time variable combination
  NN.tbl = .data$data %>%
    # The .gt variables can take values of 0/1/NA. For each response in the
    # data, return whether it's NA.  Because the .gt variables are indicators
    # referring to levels of a smaller number of questions (i.e., trials), the
    # count of non-NA responses isn't itself the trial count. Say we have .gt
    # variables:
    # A0,  A1,  A2,  B0,  B1,  C0,  C1.
    mutate_each(funs(notNA), contains(".gt")) %>%
    # For a respondent who only answered questions A and B, we now have
    #  T,   T,   T,   T,   T,   F,   F.
    mutate_each(funs(. / n.responses), contains(".gt"))  %>%
    # Dividing the booleans by n.responses = 5 calculated earlier gives us
    #.02, .02, .02, .02, .02,   0,   0.
    # Which ... FIXME: doesn't make sense. We started with 2 trials, so
    # n.responses should be 2.
    group_by(dirt.geo, dirt.demo, dirt.t) %>%
    summarise_each(funs(sum), contains(".gt")) %>%
    ungroup() %>%
    # Summing over respondents within the grouping variables gives the count
    # of trials for each combination of geo, demo, and time variables, divided
    # equally across the .gt indicators.
    full_join(de.tbl, by = c('dirt.geo', 'dirt.demo', 'dirt.t')) %>%
    # Joining the design effect table by the grouping variables attaches for
    # each variable combination the associated design effect
    mutate_each(funs(ceiling(. / def)), contains(".gt")) %>%
    # Dividing by the design effect gives us a design-effect-adjusted trial
    # count.
    select(-def) %>%
    # Tidy up. Recasting the grouping variables requires ungrouping.
    mutate_each(funs(as.character), dirt.geo, dirt.demo, dirt.t)

  # The table just created excludes covariate combinations not observed in the
  # data, but we want all combinations of the demographic and geographic
  # covariates and specified periods
  group.grid = expand.grid(
    'dirt.geo' = sort(unique(NN.tbl$dirt.geo)),
    'dirt.demo' = sort(unique(NN.tbl$dirt.demo)),
    'dirt.t' = as.character(.opts$use.t),
    stringsAsFactors = F)
  # A full_join of NN.tbl and group.grid will create rows for the desired
  # covariate combinations, where unobserved cells take NA values
  NN.tbl = full_join(NN.tbl, group.grid,
    by = c('dirt.geo', 'dirt.demo', 'dirt.t'))
  # Replace the NA values in those rows representing unobserved covariate
  # combinations with zeroes
  replaceNA = function(x) replace(x, is.na(x), 0)
  NN.tbl = NN.tbl %>% mutate_each(funs(replaceNA), contains('.gt'))
  # NOTE: Order will matter in a moment
  NN.tbl = NN.tbl %>% arrange(dirt.geo, dirt.demo, dirt.t) %>%
    mutate_each(funs(as.vector), contains('.gt'))

  # TODO: Check this: in the new NN, six all-NA covariate combinations are
  # excluded; not in the original NN. Should these cells be zero instead?

  replaceNaN = function(x) replace(x, is.nan(x), NA)
  # Create table of (weighted) average outcomes within combinations of
  # geographic, demographic, and time variables.
  y.bar.star = .data$data %>%
    group_by(dirt.geo, dirt.demo, dirt.t) %>%
        # Starting again from the data, select the .gt variables
    summarise_each(funs(weighted.mean(as.vector(.), dirt.weight/n.responses,
      na.rm = T)), contains('.gt')) %>%
    group_by(dirt.geo, dirt.demo, dirt.t) %>%
    # Replace NAN with NA
    mutate_each(funs(replaceNaN)) %>%
    ungroup() %>%
    mutate_each(funs(as.character), dirt.geo, dirt.demo, dirt.t)
  # As above, create rows for unobserved but desired covariate combinations,
  # but this time leave the values as NA
  y.bar.star = full_join(y.bar.star, group.grid,
      by = c('dirt.geo', 'dirt.demo', 'dirt.t')) %>%
    # NOTE: Order will matter in a moment
    arrange(dirt.geo, dirt.demo, dirt.t) %>%
    mutate_each(funs(as.vector), contains('.gt'))

  # summarise_each(x, funs(weighted.mean(., wt = dirt.weight/n.responses)), contains('.gt'))

  # Confirm row order is identical before taking product
  stopifnot(identical(
    select(y.bar.star, dirt.geo, dirt.demo, dirt.t),
    select(NN.tbl, dirt.geo, dirt.demo, dirt.t)))

  # Take the product of the (weighted) average cell outcome and the (adjusted)
  # trial count
  SS = select(NN.tbl, contains('.gt')) * select(y.bar.star, contains('.gt'))
  SS = SS %>%
    # Reattach our identifiers
    bind_cols(select(NN.tbl, dirt.geo, dirt.demo, dirt.t), .) %>%
    # Round off returning integers
    ungroup() %>%
    mutate_each(funs(round(., digits = 0)), contains(".gt")) %>%
    arrange(dirt.geo, dirt.demo, dirt.t)

  # Create a summary table: for each period t, are there any nonmissing responses?
  N.valid = data.frame(
      t = NN.tbl$dirt.t,
      n = select(NN.tbl, contains('.gt')) %>% rowSums()) %>%
    group_by(t) %>%
    summarise(any.valid = sum(n, na.rm=T) > 0)
  # Giving us the periods in which we observe responses
  t.observed = as.numeric(as.character(N.valid$t[N.valid$any.valid]))
  # Which may be different from the periods we'll use in estimation
  t.use = factor(.opts$use.t)

  # TODO: these are both all 0; as expected?
  ZZ.prior = createZZPrior(.data, t.use, XX, .opts)
  ZZ = createZZ(.data, t.use, XX, .opts)

  # T gives the number of time periods
  T = length(t.use)
  # Q gives the number of questions
  Q = sum(grepl('.gt', colnames(.data$data), fixed=T))
  # G gives the number of covariate combinations
  G = nlevels(.data$data$dirt.geo) * nlevels(.data$data$dirt.demo)

  # Generate factor levels for combinations of demographic variables
  if (is.null(.data$data$dirt.demo)) {
    demo.group = gl(1, nrow(xtab))
  } else {
    demo.group = xtab$dirt.demo
  }
  # G.l2 gives the number of level-2 covariate combinations
  # TODO: not implemented
  G.l2 = nlevels(.data$data$dirt.demo)

  # Calculate G x T x G.l2 weight matrix
  xtab.ds = svydesign(~1, probs=1, data=xtab.t)
  l2.wts = 1/xtab.ds$prob
  WT = createWT(.l2.wts = l2.wts,
    .G = G,
    .T = T,
    .G.l2 = G.l2,
    .demo.group = demo.group,
    .XX = XX)
  # FIXME: quick hack for no level-two data
  # WT = WT[, 1, ]

  # NOTE: creating NN.l2 as a placeholder for non-missing level-2 data
  NN.l2 = NN.tbl %>% mutate_each(funs(rep(0, length(.))), contains('.gt'))

  # TODO: confirm Q should be ncol(contains('.gt')) and not length(q) and not
  # length(.data$varnames$q.vars)

  # TODO: l2_only is set to false for all groups here, but if level-2 data are
  # provided this should be calculated or exposed
  # Quick fix for missing rownames in l2_only
  l2_only = NN.tbl %>%
    group_by(dirt.t) %>%
    summarise_each(funs(c(0)), contains('.gt'))
  rownames(l2_only) = l2_only$dirt.t
  l2_only = l2_only %>%
    select(-dirt.t) %>%
    as.matrix(l2_only, rownames.force=T)

  # Check that NN.tbl and NN.l2 contain three expected identifiers, ".gt"
  # variables, and nothing else. Otherwise, N will differ from length(n.vec.).
  stopifnot(length(setdiff(colnames(NN.tbl), c('dirt.geo', 'dirt.demo',
          'dirt.t', colnames(NN.tbl)[grep('.gt\\d+', colnames(NN.tbl))]))) == 0)
  stopifnot(length(setdiff(colnames(NN.l2), c('dirt.geo', 'dirt.demo',
          'dirt.t', colnames(NN.l2)[grep('.gt\\d+', colnames(NN.l2))]))) == 0)

  # Calculate N as the (adjusted) count of responses that aren't level-two-only
  # N = full_join(
  #       melt(NN.tbl, id.vars = c('dirt.demo', 'dirt.geo', 'dirt.t'), value.name = 'n.l1') %>%
  #         # mutate(n.nonzero = value > 0) %>%
  #         as.tbl(),
  #       melt(NN.l2, id.vars = c('dirt.demo', 'dirt.geo', 'dirt.t'), value.name = 'n.l2') %>%
  #         as.tbl(),
  #       by = c(c('dirt.demo', 'dirt.geo', 'dirt.t'), 'variable')) %>%
  #     mutate(N = n.l1 + n.l2 * !is.na(n.l1)) %>%
  #   summarise(N = sum(N)) %>%
  #   unlist()

  NNl2 = NN.l2 %>%
    melt(id.vars = c('dirt.demo', 'dirt.geo', 'dirt.t')) %>%
    group_by(dirt.t, dirt.demo, variable) %>%
    summarise(l2.sum = sum(value)) %>%
    mutate(l2.sum = as.integer(l2.sum)) %>%
    acast(dirt.t ~ variable ~ dirt.demo, value.var = 'l2.sum', fill = 0, drop = F)

  # TODO: this is what happens in the original code, but then we didn't have
  # any group-level data
  SSl2 = NNl2

  ns.long.obs = left_join(
      melt(NN.tbl, id.vars = c("dirt.geo", "dirt.demo", "dirt.t"), value.name = 'n.grp') %>% mutate(variable = as.character(variable)),
      melt(SS, id.vars = c("dirt.geo", "dirt.demo", "dirt.t"), value.name = 's.grp') %>% mutate(variable = as.character(variable)),
      by = c('dirt.geo', 'dirt.demo', 'dirt.t', 'variable')) %>%
    arrange(variable, dirt.t, desc(dirt.demo), dirt.geo) %>%
    # filter(!is.na(n.grp) & n.grp != 0) %>%
    # mutate(name = str_c(dirt.geo, dirt.demo, dirt.t, sep = '__')) %>%
    mutate(name = str_c(dirt.geo, dirt.demo, sep = '__')) %>%
    # mutate(name = str_c(name, variable, sep = ' | ')) %>%
    # NOTE: Dropping all zero-trial groups ... But want to include covariate
    # combinations observed in any period in all specified periods
    filter(n.grp != 0) %>%
    as.tbl()

  # So, a
  ns.long = data.frame(expand.grid(unique(ns.long.obs$name), t.use)) %>%
      mutate_each(funs(as.character)) %>%
    left_join(ns.long.obs, ., by = c('name' = 'Var1', 'dirt.t' = 'Var2')) %>%
    mutate(n.grp = replaceNA(n.grp),
      name = str_c(name, dirt.t, sep = '__'),
      name = str_c(name, variable, sep = ' | ')) %>%
    arrange(dirt.geo, desc(dirt.demo), variable, dirt.t)

  # NOTE: some NAs are the result of the join; they represent cells with
  # level-one data but no level-two data. Others are from NN.tbl -
  # double-check.
  ns.long$s.grp[is.na(ns.long$s.grp)] = 0
  ns.long$n.grp[is.na(ns.long$n.grp)] = 0
  ns.long$s.grp = as.integer(ns.long$s.grp)
  ns.long$n.grp = as.integer(ns.long$n.grp)
  # TODO: checks

  # Create missingness indicator array
  MMM = ns.long %>%
    full_join(group.grid, by = c('dirt.geo', 'dirt.demo', 'dirt.t')) %>%
    # Drop any NA-variable rows induced by full_join
    filter(!is.na(variable) & variable != '' & variable != 'NA') %>%
    # Get missingness by group
    group_by(dirt.geo, dirt.demo, dirt.t) %>%
    summarise(m.grp = as.integer(sum(n.grp, na.rm = T) == 0)) %>%
    acast(dirt.t ~ dirt.demo + dirt.geo, value.var = 'm.grp', fill = 1)

  n.vec = unlist(ns.long$n.grp)
  s.vec = unlist(ns.long$s.grp)
  names(n.vec) = ns.long$name
  names(s.vec) = ns.long$name

  # stopifnot(identical(dim(NNl2), dim(SSl2)))
  # dim(MMM)
  # dim(WT)
  # stopifnot(identical(dim(MMM), dim(WT)))

  # Return stan data
  list(
    n_vec = n.vec,      # trial counts
    s_vec = s.vec,      # sums
    NNl2 = NNl2,
    SSl2 = SSl2,
    XX = XX[, -1],
    ZZ = ZZ[, -1, , drop = FALSE],      # geographic predictors
    ZZ_prior = ZZ.prior[, -1, , drop = FALSE],
    MMM = MMM,             # missingness array (T x Q x G)
    G = G,                 # number of covariate groups
    Q = Q,                 # number of questions (items)
    T = T,                 # number of time units (years)
    N = length(n.vec),                 # number of observed group-question cells
    P = ncol(XX[, -1]),    # number of hierarchical parameters
    S = dim(ZZ)[[2]] - 1,  # number of geographic units
    H = dim(ZZ)[[3]],      # number of geographic-level predictors
    Hprior = dim(ZZ.prior)[[3]],
    separate_years = .data$params$separate.t,    # if 1, no pooling over time
    constant_item = .data$params$constant.item,      # if 1, difficulties constant
    D = ifelse(.data$params$constant.item, 1, T),
    WT = WT,                            # weight matrix for calculating level-two mean
    l2_only = l2_only,
    Gl2 = G.l2)                        # number of second-level groups
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
        "sd_total", "theta_l2", "var_theta_bar_l2"),
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
