# Define internal functions for dynamic IRT
# Created 6/30/15
# J.Dunham
#
# TODO: filestring

## Functions from original code ##

allNA <- function (x) all(is.na(x))
anyNA <- function (x) any(is.na(x))
allValid <- function (x) all(!is.na(x))
anyValid <- function (x) any(!is.na(x))
sumValid <- function (x) sum(!is.na(x))

Formula <- function (vars, y=NULL, text=FALSE) {
  Y <- paste(y, "~")
  vars <- paste(vars, collapse = " + ")
  if (text) return(paste(Y, vars))
  else return(as.formula(paste(Y, vars)))
}

# TODO: new or old?
inTable = function(.table, s, .all=TRUE) {
  out = sapply(s, function(x) !is.null(.table[[x]]))
  if (.all) {
    return(all(out))
  } else {
    return(out)
  }
}

## New functions for package ##

setUserHome = function() {
  # Try to determine the current user and set working directory to their home
  user = Sys.info()["user"]
  if (grepl("devin", user, ignore.case=TRUE)) {
    home.dir = "~/Dropbox/shared/work/Dynamic_MRP"
    # data.dir = paste(home.dir, "Data", sep="/")
    # save.dir = paste(home.dir, "R-output", sep="/")
    # plot.dir = paste(save.dir, "Plots/Devin", sep="/")
    # out.dir = paste0(save.dir, "/Out/Devin")
  } else if (grepl("cwarshaw", user, ignore.case=TRUE)) {
    # TODO: add cwarshaw folders
  } else if (grepl("james", user, ignore.case=TRUE)) {
    home.dir = "~/projects/group-irt"
    # data.dir = home.dir
    # save.dir = home.dir
    # plot.dir = home.dir
    # out.dir = home.dir
  } else {
    home.dir = "~/"
    # data.dir = home.dir
    # plot.dir = home.dir
    # save.dir = home.dir
    # out.dir = home.dir
  }
  setwd(home.dir)
  cat('Working in', getwd(), '\n')
}

countValid <- function (x) sum(!is.na(x))
notNA <- function (x) !is.na(x)

# Create summary table of design effects
summariseDef = function(x) {
  y = 1 + (sd(x) / mean(x)) ^ 2
  if (is.na(y)) return(1)
  else return(y)
}

# Create design matrix for model of hierarchical coefficients
createZZPrior = function(d = .data,
    .svy.yr.range = svy.yr.range,
    .XX = XX,
    ..opts = .opts) {
  # First-period priors
  if (is.null(..opts$geo.mod.prior.vars)) {
    zz.prior.names <- list(levels(.svy.yr.range), levels(d$data[, ..opts$geo.var]), "Zero")
    ZZ.prior <- array(data=0, dim=llply(zz.prior.names, length), dimnames=zz.prior.names)
    ZZ.prior <- ZZ.prior[as.character(.svy.yr.range),,, drop=FALSE]
  } else {
    if (..opts$geo.var == "StPOAbrv") {
      # TODO: data-particular arguments here, but not sure what to make of them
      ZZ.prior <- melt(subset(st.dta,, -REGION4), id.vars=c("StPOAbrv", "year"))
      ZZ.prior <- acast(ZZ.prior, year ~ StPOAbrv ~ variable)
      ZZ.prior <- ZZ.prior[.svy.yr.range,, ..opts$geo.mod.prior.vars, drop=FALSE]
    }
  }
  ZZp0 <- array(0, dim = c(dim(ZZ.prior)[1], ncol(.XX) - dim(ZZ.prior)[2], dim(ZZ.prior)[3]))
  ZZ.prior = abind(ZZ.prior, ZZp0, along = 2)
  # Quick fix for null colnames
  dimnames(ZZ.prior)[[2]] = colnames(.XX)
  return(ZZ.prior)
}

# Create hierarchical priors
createZZ = function(d = .data,
    .svy.yr.range = svy.yr.range,
    .XX = XX,
    ..opts = .opts) {
  if (is.null(..opts$geo.mod.vars)) {
    zz.names <- list(levels(.svy.yr.range), levels(d$data[, ..opts$geo.var]), "Zero")
    ZZ <- array(data=0, dim=llply(zz.names, length), dimnames=zz.names)
  } else {
    # TODO: application-specific variables used here; expose?
    if (..opts$geo.var == "StPOAbrv") {
      ZZ <- melt(subset(st.dta,, -REGION4), id.vars=c("StPOAbrv", "year"))
      ZZ <- acast(ZZ, year ~ StPOAbrv ~ variable)
      ZZ <- ZZ[as.character(.svy.yr.range),, ..opts$geo.mod.vars, drop=FALSE]
    }
  }
  ZZ0 <- array(0, dim = c(dim(ZZ)[1], ncol(.XX) - dim(ZZ)[2], dim(ZZ)[3]))
  ZZ <- abind(ZZ, ZZ0, along = 2)
  # Quick fix for null colnames
  dimnames(ZZ)[[2]] = colnames(.XX)
  return(ZZ)
}

# Create weights
createWT = function(.l2.wts = l2.wts, .G = G, .T = T, .G.l2 = G.l2, .demo.group = demo.group, .XX = XX){
  WT = array(.l2.wts, dim=c(.G, .T, .G.l2))
  # TODO: perm was 2 x 3 x 1; why x1?
  WT = aperm(WT, c(.G.l2, .T, 1))
  for (i in seq_len(.G.l2)) {
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
    varname = paste0(q, '.gt', .levels)
    d[, varname] = sapply(.levels, function(l) {
      as.numeric(d[[q]] > as.numeric(l))
  })
  }
  return(d)
}

# Test whether an object (identified by string) is identical to one of the same
# name in the "orig" environment
is.original = function(x) {
  identical(eval(as.symbol(x)), get(x, envir = orig), ignore.environment = TRUE)
}
