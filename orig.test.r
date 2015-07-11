
## New functions for package ##

# FIXME: default DATA
getDesignEffects = function(d = .data, .n.responses = n.responses, .covs.yr = covs.yr){
  # Calculate design effects
  de.df = data.frame(row = 1:nrow(d$data), Nresponses = .n.responses, d$data[, d$fm.vars])
  de.df <- de.df %>% group_by_(.covs.yr) %>%
    mutate(def = 1 + (sd(useweight) / mean(useweight)) ^ 2, def = ifelse(is.na(def), 1, def))
  de.df = de.df %>% ungroup() %>% arrange(row)
  return(de.df)
}

## Define two helper functions to be used in createNN and createSS
# Get weighted sample size
Nstar <- function(.y = y, .def = def, .n.responses = Nresponses) {
  # weight by inverse of num responses
  ob.adj <- as.numeric(!is.na(.y)) / .n.responses
  # reduce N by design effect
  # FIXME: .def isn't what it's supposed to be
  n.star <- ceiling(sum(ob.adj / .def))
  return(n.star)
}

Sstar <- function(.y = y, .def = def, .n.responses = nr, wt) {
  y.bar.star <- weighted.mean(.y, wt/.n.responses, na.rm=TRUE)
  s.star <- round(Nstar(.y, .def, .n.responses) * y.bar.star, digits=0)
  return(s.star)
}

# Create table with counts of trials
createNN = function(.YY = YY, .de.df = de.df, .covs.yr = covs.yr, .xtab.yr = xtab.yr, .group.yr = group.yr){
  # FIXME: don't call globals
  NN.dt <- aaply(.YY, 2, function(this.q, ..de.df = .de.df, ..covs.yr = .covs.yr, ..xtab.yr = xtab.yr) {
    # FIXME: "object 'de.df' not found"
    gdf <- group_by_(data.frame(y = this.q, ..de.df), .dots = lapply(..covs.yr, as.symbol), drop = FALSE)
    sdf <- summarise(gdf, nstar = Nstar(.y = y, .def = def, .n.responses = Nresponses))
    mdf <- merge(x = ..xtab.yr, y = sdf, by = ..covs.yr, all.x = TRUE)
    mdf = as.tbl(mdf) %>% ungroup() %>% arrange(xtrow)
    # TODO: confirm
    # mdf <- plyr::arrange(mdf, xtrow)
    out <- mdf$nstar
    names(out) <- levels(.group.yr)
    return(out)
      }, .progress = "text")
  NN.dt[is.na(NN.dt)] <- 0
  t(as.data.frame(NN.dt))
}

# FIXME: this is generating an entirely NA table, and a slew of "'*' not
# meaningful for factors" warnings
# Create table with counts of successes
createSS = function(.YY = YY, .covs.yr = covs.yr, .xtab.yr = xtab.yr, .de.df = de.df, .group.yr = group.yr) {
  SS.dt = aaply(.YY, 2, function(this.q, ..de.df = .de.df) {
    # FIXME: this line doesn't make sense
    gdf = group_by_(data.frame(y = this.q, ..de.df), .dots = lapply(.covs.yr, as.symbol), drop = FALSE)
    # FIXME: reference to de below?
    sdf = summarise(gdf, sstar = Sstar(y, def, Nresponses, useweight))
    # TODO: joining would be faster
    mdf = merge(x = .xtab.yr, y = sdf, by = .covs.yr, all.x = TRUE)
    mdf = as.tbl(mdf) %>% ungroup() %>% arrange(xtrow)
    out = mdf$sstar
    names(out) = levels(.group.yr)
    return(out)
  }, .progress = "text")
  # Replace NA with 0
  SS.dt[is.na(SS.dt)] <- 0
  t(as.data.frame(SS.dt))
}

# Create design matrix for model of hierarchical coefficients
createZZPrior = function(d = .data, .svy.yr.range = svy.yr.range, .XX = XX){
  # First-period priors
  if (is.null(d$geo.mod.prior.vars)) {
    zz.prior.names <- list(.svy.yr.range, levels(d$data[, d$geo.var]), "Zero")
    ZZ.prior <- array(data=0, dim=llply(zz.prior.names, length), dimnames=zz.prior.names)
    ZZ.prior <- ZZ.prior[as.character(.svy.yr.range),,, drop=FALSE]
  } else {
    if (d$geo.var == "StPOAbrv") {
      # TODO: data-particular arguments here, but not sure what to make of them
      ZZ.prior <- melt(subset(st.dta,, -REGION4), id.vars=c("StPOAbrv", "year"))
      ZZ.prior <- acast(ZZ.prior, year ~ StPOAbrv ~ variable)
      ZZ.prior <- ZZ.prior[as.character(.svy.yr.range),, d$geo.mod.prior.vars, drop=FALSE]
    }
  }
  ZZp0 <- array(0, dim = c(dim(ZZ.prior)[1], ncol(.XX) - dim(ZZ.prior)[2], dim(ZZ.prior)[3]))
  ZZ.prior = abind(ZZ.prior, ZZp0, along = 2)
  cat(dim(ZZ.prior))
  return(ZZ.prior)
}

# Create hierarchical priors
createZZ = function(d = .data, .svy.yr.range = svy.yr.range, .XX = XX) {
  if (is.null(d$geo.mod.vars)) {
    zz.names <- list(.svy.yr.range, levels(d$data[, d$geo.var]), "Zero")
    ZZ <- array(data=0, dim=llply(zz.names, length), dimnames=zz.names)
  } else {
    # TODO: application-specific variables used here; expose?
    if (d$geo.var == "StPOAbrv") {
      ZZ <- melt(subset(st.dta,, -REGION4), id.vars=c("StPOAbrv", "year"))
      ZZ <- acast(ZZ, year ~ StPOAbrv ~ variable)
      ZZ <- ZZ[as.character(.svy.yr.range),, d$geo.mod.vars, drop=FALSE]
    }
  }
  ZZ0 <- array(0, dim = c(dim(ZZ)[1], ncol(.XX) - dim(ZZ)[2], dim(ZZ)[3]))
  ZZ <- abind(ZZ, ZZ0, along = 2)
  cat(dim(ZZ))
  return(ZZ)
}

# Create weights
createWT = function(.nat.wts = nat.wts, .G = G, .T = T, .Gnat = Gnat, .demo.group = demo.group, .XX = XX){
  WT <- array(.nat.wts, dim=c(.G, .T, .Gnat))
  # T x Gnat x G (TODO: will these dimensions always be corrrect?)
  WT <- aperm(WT, c(2, 3, 1))
  for (i in seq_len(.Gnat)) {
    (dg <- levels(.demo.group)[i])
    WT[, i, !.demo.group == dg] <- 0
    WT[, i, ] <- WT[, i, ] / rowSums(WT[, i, ])
    (WT[.T, i, ] %*% .XX)
  }
  # TODO: rewrite as test; I take it we should see all 1s
  apply(WT, 1, rowSums)
  return(WT)
}

# Create "greater than" indicators
createGT = function(d = .data$data, .q.vars=.data$q.vars) {
  # Create gt.vars
  for (q in .q.vars) {
    .levels = levels(factor(d[[q]]))
    .levels = .levels[-length(.levels)]
    varname = paste0(q, '.gt.', .levels)
    d[, varname] = sapply(.levels, function(l) {
      as.numeric(d[[q]] > as.numeric(l))
  })
  }
  return(d)
}

### TEST ####

t0 = Sys.time()

  fm.vars = names(select(poll.df, YearFactor:age))     # front matter variable names, as character vector
  q.vars = names(select(poll.df, -c(YearFactor:age)))  # survey question variable names, as character vector
  time.var = 'YearFactor'             # time variable, as character
  # TODO: replace 'years' with 'periods'/'time' throughout; we're agnostic about units
  min.years = 1L                      # questions appearing in fewer periods will be dropped
  min.polls = 1L                      # questions appearing in fewer than this many polls will be dropped
  geo.var = 'Country'                 # geographic variable, as  character
  demo.vars = 'Gender'                # demographic variables, as character vector
  geo.mod.vars = NULL                 # TODO: description
  geo.mod.prior.vars = NULL           # TODO: description
  # TODO: just use T/F
  separate.years = as.integer(FALSE)  # no smoothing over time?
  constant.item = as.integer(TRUE)     # make difficulty parameters constant over time?
  test = TRUE                          # run checks without touching the data?

  covs = c(geo.var, demo.vars)
  covs.yr = c(covs, .data$time.var)
  f0 = Formula(c("0", covs))
  f0.yr = Formula(c("0", covs.yr))

  # Drop rows lacking covariates or time variable
  .data$data = filter(.data$data, rowSums(is.na(select(.data$data, one_of(covs.yr)))) == 0)
  # Create .gt. variables
  .data$data = createGT(d = .data$data, .q.vars = .data$q.vars)
  # Drop rows lacking at least one .gt. variable
  .data$data = filter(.data$data, rowSums(!is.na(select(.data$data, contains('.gt.')))) > 0)
  # Fix levels of geo variable after filtering
  .data$data[, .data$geo.var] <- droplevels(.data$data[, .data$geo.var])

  # Represent covariates in poll.df as model frame
  MF <- model.frame(f0, data=.data$data, na.action=return)
  MF.yr <- model.frame(f0.yr, data=.data$data, na.action=return)

  MF.orig = MF
  MF.yr.orig = MF.yr

  # Create factors with levels for each combination of covariate values
  group <- interaction(as.list(MF), sep="__", lex.order=FALSE)
  group.yr <- interaction(as.list(MF.yr), sep="__", lex.order=FALSE)

  group.orig = group
  group.yr.orig = group.yr

  # Get frequency counts of factor combinations (reordered in factor order)
  xtab.yr <- as.data.frame(table(MF.yr))
  # TODO: why does the original code subset to the first year here?
  xtab <- subset(xtab.yr, YearFactor == .data$yr.range[1], -c(YearFactor, Freq))
  # Check that group.yr levels are in same order as rows of xtab.yr
  stopifnot(identical(levels(group.yr), unname(apply(subset(xtab.yr,, -Freq), 1, paste, collapse="__"))))
  # Create a row index
  xtab.yr$xtrow <- 1:nrow(xtab.yr)

  xtab.yr.orig = xtab.yr
  xtab.orig = xtab

  # Get dummy variable representation of contingency table
  XX <- model.matrix(f0, xtab)
  # Don't use hierarchical model if we only have one predictor
  if (length(covs) == 1) XX <- matrix(0, nrow(XX), ncol(XX))

  XX.orig = XX

  # Create matrix of responses
  YY <- select(.data$data, contains('.gt.')) %>% as.matrix(.)
  n.responses <- rowSums(!is.na(YY), na.rm=TRUE)

  YY.orig = YY
  n.responses.orig = n.responses

  # Get design effects
  de.df = getDesignEffects(d = .data, .n.responses = n.responses, .covs.yr = covs.yr)

  de.df.orig = de.df

  # Create table of trial counts
  NN = createNN(.YY = YY, .de.df = de.df, .covs.yr = covs.yr, .xtab.yr = xtab.yr, .group.yr = group.yr)

  NN.orig = NN

  # Create table of success counts
  SS = createSS(.YY = YY, .covs.yr = covs.yr, .xtab.yr = xtab.yr, .group.yr = group.yr)
  # There should be no more successes than trials
  stopifnot(all(SS - NN <= 0))

  SS.orig = SS

  # Grab "greater than" variable names
  qs.used = str_subset(colnames(.data$data), '\\.gt\\.')

  qs.used.orig = qs.used

  # Identify years of surveys with valid responses
  svy.yrs = .data$yr.range[laply(.data$yr.range, function (yr) {
      # We've replaced NA with 0, so this is a check for any valid values in a year
      any(NN[grepl(yr, rownames(NN)), ] > 0)
    })]
  svy.yr.range = min(svy.yrs):max(svy.yrs)

  svy.yrs.orig = svy.yrs
  svy.yr.range.orig = svy.yr.range

  # TODO: these are both all 0; as expected?
  ZZ.prior = createZZPrior()
  ZZ = createZZ()

  ZZ.prior.orig = ZZ.prior
  ZZ.orig = ZZ

  # T gives the number of time periods
  T <- length(svy.yr.range)
  # Q gives the number of questions
  Q = sum(grepl('.gt.', colnames(.data$data), fixed=T))
  # G gives the number of covariate (?) combinations
  G <- nlevels(group)

  T.orig = T
  Q.orig = Q
  G.orig = G

  # Generate factor levels for combinations of demographic variables
  if (is.null(.data$demo.vars)) {
    demo.group <- gl(1, nrow(xtab))
  } else {
    demo.group <- interaction(as.list(xtab[, demo.vars, drop=FALSE]))
  }
  # Gnat gives the number of combinations
  Gnat <- nlevels(demo.group)

  demo.group.orig = demo.group
  Gnat.orig = Gnat

  # Calculate weights
  xtab.ds <- svydesign(~1, probs=1, data=xtab.yr)
  nat.wts <- 1/xtab.ds$prob
  WT = createWT(.nat.wts = nat.wts,
    .G = G,
    .T = T,
    .Gnat = Gnat,
    .demo.group = demo.group,
    .XX = XX)

  xtab.ds.orig = xtab.ds
  nat.wts.orig = nat.wts
  WT.orig = WT

  # TODO: confirm difference between NN.nat and NNnat
  NN.nat <- array(FALSE, dim=dim(NN), dimnames=dimnames(NN))
  nat_only <- array(0, dim=c(T, Q), dimnames=list(svy.yr.range, qs.used))

  NN.nat.orig = NN.nat
  nat_only.orig = nat_only

  # groups with data and not national-only
  (N <- base::sum(NN > 0 & !NN.nat))
  NNnat <- SSnat <- array(0, c(T, Q, Gnat), list(svy.yr.range, qs.used, levels(demo.group)))

  N.orig = N
  NNnat.orig = NNat

  # Create vectors of trials and successes
  n_vec <- s_vec <- integer(N)
  names(n_vec) <- names(s_vec) <- seq_len(N)

  n_vec.orig = n_vec

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

  NNnat.orig = NNat
  SSnat.orig = SSnat
  MMM.orig = MMM

  # If using the pos counter, we should've iterated over all N
  # if (!nat_only[t, q]) stopifnot(identical(as.integer(pos), N))

  # Report out some statistics (TODO: user-facing explanations)
  cat("Count by 5-percentile bins:\n")
  cat(quantile(n_vec, seq(.05, .95, .05)), '\n\n')
  cat(table(n_vec), '\n\n')
  cat(table(s_vec), '\n\n')
  cat(mean(!MMM), '\n\n')

  # Return stan data
  out = list(
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
    separate_years = separate.years,    # if 1, no pooling over time
    constant_item = constant.item,      # if 1, difficulties constant
    D = ifelse(constant.item, 1, T),
    WT = WT,                            # weight matrix for calculating national mean
    nat_only = nat_only,
    Gnat = Gnat)                        # number of national-level groups

  cat(Sys.time() - t0)
