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

onDevin <- function (...) {
  user <- Sys.info()["user"]
  onD <- grepl("devin", user, ignore.case=TRUE)
  return(onD)
}
onChris <- function (...) {
  user <- Sys.info()["user"]
  onD <- grepl("cwarshaw", user, ignore.case=TRUE)
  return(onD)
}
onJames <- function (...) {
  user <- Sys.info()["user"]
  onJ <- grepl("james", user, ignore.case=TRUE)
  return(onJ)
}
onHMDC <- function (...) {
  user <- Sys.info()["nodename"]
  hmdc <- grepl("hmdc", user, ignore.case=TRUE)
  return(hmdc)
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

countValid <- function (x) sum(!is.na(x))
notNA <- function (x) !is.na(x)

# FIXME: default DATA
# getDesignEffects = function(d = .data, .n.responses = n.responses, .covs.yr = covs.yr){
#   # Calculate design effects
#   # TODO: change all d$opts references to implementation of .opts$
#   de.df = data.frame(row = 1:nrow(d$data), Nresponses = .n.responses, d$data[, d$fm.vars])
#   # FIXME: 'useweight'?
#   de.df <- de.df %>% group_by_(.covs.yr) %>%
#     # mutate(def = 1 + (sd(useweight) / mean(useweight)) ^ 2, def = ifelse(is.na(def), 1, def))
#     mutate(def = mutate.def(useweight))
#   de.df = de.df %>% ungroup() %>% arrange(row)
#   return(de.df)
# }

# FIXME: use this summary table of de

summariseDef = function(x) {
  y = 1 + (sd(x) / mean(x)) ^ 2
  if (is.na(y)) return(1)
  else return(y)
}

obAdj = function(x, .n.responses = n.responses) { 
  out = sumValid(x)/
  names(out) = names(x)
  return(out)
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
    varname = paste0(q, '.gt', .levels)
    d[, varname] = sapply(.levels, function(l) {
      as.numeric(d[[q]] > as.numeric(l))
  })
  }
  return(d)
}

# Test whether an object (identified by string) is
# identical to one of the same name in the "orig"
# environment
is.original = function(x) {
  identical(eval(as.symbol(x)), get(x, envir = orig), ignore.environment = TRUE)
}
