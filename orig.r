options(width=110)

################################################################################
#### LIBRARIES #################################################################
################################################################################

library(rstan)
library(foreign)
library(parallel)
library(TeachingDemos)
library(ggplot2)
library(reshape2)
library(car)
library(plyr)
library(dplyr)
library(survey)
library(stringr)
library(data.table)
library(rms)
library(abind)
detectCores()

t0 = Sys.time()

################################################################################
#### FUNCTIONS #################################################################
################################################################################

as.numchar <- function(x) as.numeric(as.character(x))
OrdLev <- function (x) {
  x <- factor(x)
  if (nlevels(x) == 2 && grepl("non-", levels(x)[2], TRUE)) {
    x <- factor(x, levels=rev(levels(x)))
  }
  return(x)
}
LoopProgress <- function(index, interval=1) {
  if (index %% interval == 0) print (index)
}
FileName <- function (path=NULL, name=NULL, ext=NULL, replace=FALSE) {
  p <- paste(path, collapse = "")
  n <- paste(name, collapse = "")
  e <- paste(".", ext, sep = "")
  d <- format(Sys.Date(), "%y%m%d") 
  for (l in seq_along(letters)) {
    if (l > 1) old_file_name <- file_name
    file_name <- paste0(p, d, paste(n, letters[l], sep="-"), e)
    if (!any(grepl(file_name, list.files()))) {
      if (replace) file_name <- ifelse(l > 1, old_file_name, file_name)
      break
    }
  }
  cat("\nFile name:", file_name, "\n\n")
  return(file_name)
}
allNA <- function (x) all(is.na(x))
anyNA <- function (x) any(is.na(x))
allValid <- function (x) all(!is.na(x))
anyValid <- function (x) any(!is.na(x))
sumValid <- function (x) sum(!is.na(x))
whichValid <- function (x) which(!is.na(x))
POtoSouth11 <- function (pos, dta=st.info) {
  dta$South11[match(pos, dta$POAbrv)]
}
POtoSouth13 <- function (pos, dta=st.info) {
  dta$South13[match(pos, dta$POAbrv)]
}
NameToPO <- function (stnames, dta=st.info) {
  as.character(dta$POAbrv[match(tolower(stnames), tolower(dta$Name))])
}
CodeToPO <- function (stcodes, dta=st.info) {
  as.character(dta$POAbrv[match(stcodes, dta$ICPSRCode)])
}
POtoCode <- function (pos, dta=st.info) {
  return(as.integer(dta$ICPSRCode[match(pos, dta$POAbrv)]))
}
FIPStoPO <- function (fips, dta=st.info) {
  as.character(dta$POAbrv[match(fips, dta$fips/1000)])
}
CodeToName <- function (stcodes, dta=st.info) {
  return(as.character(dta$Name[match(stcodes, dta$ICPSRCode)]))
}
POtoRegion4 <- function (pos, dta=st.info) {
  as.character(dta$REGION4[match(pos, dta$POAbrv)])
}
myRecode <- function(var, recode.ls, as.factor.result=TRUE, ...) {
  options(useFancyQuotes = FALSE)
  recode.ls <- sapply(recode.ls, function(x) paste(sQuote(x), collapse='='))
  recodes <- paste(recode.ls, collapse=';')
  print(recodes)
  revar <- Recode(var, recodes, as.factor.result=TRUE, ...)
  options(useFancyQuotes = TRUE)
  revar
}
mypdf <- function(name, ...) {
  date <- format(Sys.Date(), "%y%m%d")
  pdf(paste(paste(name, collapse=""), date, ".pdf", sep=""), ...)
}
mysw <- function (...) {
  setwd(paste(..., sep = "/"))
}
NAtoF <- function (x) {
  y <- x
  y[is.na(y)] <- FALSE
  return(y)
}
NAto0 <- function (x) {
  y <- x
  y[is.na(y)] <- 0
  return(y)
}
NonNA <- function (x) {
  return(x[!is.na(x)])
}
PasteEval <- function(..., print = FALSE) {
  txt <- paste(..., sep = "")
  if (print) { cat("\n", txt, "\n") }
  eval(parse(text = txt), envir = parent.frame())
}
ReadLatest <- function (name, dir=".", format="dta", ...) {
  all.files <- list.files(dir)
  files <- all.files[grepl(name, all.files) & grepl(format, all.files)]
  dates <- sub(".*([0-9]{6}).*", "\\1", files)
  if (format == "dta") {
    data <- read.dta(paste(dir, files[which.max(dates)], sep="/"), ...)
  }
  if (format == "csv") {
    data <- read.csv(paste(dir, files[which.max(dates)], sep="/"), ...)
  }
  cat("\nOpening:", files[which.max(dates)], "\n")
  return (data)
}
SourceLatest <- function (name, dir=".", ...) {
  all.files <- list.files(dir)
  files <- all.files[grepl(name, all.files) & grepl("\\.R\\>", all.files)]
  dates <- sub(".*([0-9]{6}).*", "\\1", files)
  file <- paste(dir, files[which.max(dates)], sep="/")
  cat("\nOpening:", files[which.max(dates)], "\n")
  source(file, ...)
}
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
onHMDC <- function (...) {
  user <- Sys.info()["nodename"]
  hmdc <- grepl("hmdc", user, ignore.case=TRUE) 
  return(hmdc)
}
grepSub <- function (obj, pattern, drop=FALSE, ...) {
  sub <- grepl(pattern, x=obj, ...)
  if (drop) sub <- !sub
  return (obj[sub])
}
unform <- function (f) {
  x <- paste(as.character(f), collapse=" ")
  x <- strsplit(x, " \\+ ")
  x <- unlist(x)
  x[1] <- gsub("[~ ]", "", x[1])
  x
}
yr2cong <- function (yr) (yr - 1786)/2

################################################################################
#### DIRECTORIES ###############################################################
################################################################################

if (onDevin()) {
  home.dir <- "~/Dropbox (Personal)/2-shared/work/collaborations/Warshaw-O'Grady/european public opinion"
  data.dir <- paste(home.dir, "Data", sep="/")
  save.dir <- paste(home.dir, "R-output", sep="/")
  plot.dir <- paste0(save.dir, "/pdf")
  out.dir <- paste0(save.dir, "/Out")
}
if (onHMDC()) {
  Sys.setenv(GOTO_NUM_THREADS = 1)
  home.dir <- "/nfs/projects_nobackup/o/oldpolls"
  data.dir <- "/nfs/projects_nobackup/o/oldpolls"
  plot.dir <- "/nfs/projects_nobackup/o/oldpolls/save/Plot"
  save.dir <- "/nfs/projects_nobackup/o/oldpolls/save"
  out.dir <- paste0(save.dir, "/Out")
}

################################################################################
#### SET-UP ####################################################################
################################################################################

if (onDevin()) {
  options(device = 'quartz')
}

# JD
out.dir = "~/Dropbox\ (MIT)/dc-cw-scratch"

poll.set <- 'EU'
q.set <- 'econ'

# JD
# setwd(out.dir)
setwd('~/projects/group-irt')
# txt.out <- FileName(c(poll.set, '-', q.set),, "out", replace=FALSE)
# txtStart(file=txt.out)
# print(txt.out)
getwd()

constant.item <- as.integer(TRUE) ## difficulty parameters constant over time?
change.at.election <- as.integer(FALSE) ## change model at elections?
separate.years <- as.integer(FALSE) ## no smoothing over time?
yes.nat.only.qs <- as.integer(FALSE) ## data at national (EU-wide) level?
western.europe.only <- as.integer(TRUE) ## restrict to Western Europe?

# JD: This needn't be run. It creates the data; use what DC shared.
################################################################################
#### DATA ######################################################################
################################################################################

# run.data.code <- FALSE 
#
# if (run.data.code) {
#   setwd(data.dir)
#   (c.codes <- plyr::arrange(read.csv('CountryCodes.csv'), Code))
#   if (q.set == 'econ') {
#     load('final_dataset_econ.Rda')
#     poll.df <- final_dataset_econ
#   } 
#   if (q.set == 'cult') {
#     load('final_dataset_cult.Rda')
#     poll.df <- final_dataset_cult
#   }
#   if (western.europe.only) {
#     poll.df <- subset(poll.df, w_europe == 1)
#     poll.set <- paste0(poll.set, "-W")
#   }
#   poll.df <- poll.df[, c("w_europe", setdiff(names(poll.df), "w_europe"))]
#   poll.df <- subset(poll.df,, -c(retire, flexibility)) ## bad items
#   summary(poll.df)
#   ls()
#
#   yr.range <- 2008:2010
#
#   poll.df <- subset(poll.df, year %in% yr.range) %>%
#       mutate(YearFactor = factor(year, yr.range),
#              Country = factor(country, levels = c.codes$Code,
#                  labels = c.codes$Country),
#              Gender = factor(gender, labels = c('Male', 'Female')))
#   poll.df <- data.frame(select(poll.df, c(YearFactor:Gender)),
#                         select(poll.df, -c(YearFactor:Gender))) 
#
#   setwd(data.dir)
#   write.dta(poll.df, 'FunTestEU150616.dta')
# }
#
################################################################################
#### MANIPULATE DATA ###########################################################
################################################################################

poll.df <- read.dta('FunTestEU150616.dta')

yr.range <- as.numeric(levels(poll.df$YearFactor))
min.years <- 1 ## min. years per question
min.polls <- 1 ## min. polls per question

(fm.vars <- c(names(select(poll.df, YearFactor:age)))) ## front matter
(q.vars <- names(select(poll.df, -c(YearFactor:age)))) ## survey questions
## subsample questions for test only

# JD don't do this, for comparison
# q.vars <- sample(q.vars, size = 6)

geo.var <- c('Country')
demo.vars <- c('Gender')
geo.mod.vars <- NULL##c('gdp_pc')
geo.mod.prior.vars <- NULL##c('gdp_pc')
(covs <- c(geo.var, demo.vars))
covs.yr <- c(covs, "YearFactor")
(f0 <- Formula(c("0", covs)))
f0.yr <- Formula(c("0", covs.yr))

## Drop variables with no valid responses
(valid.vars <- names(poll.df)[colSums(!is.na(poll.df)) > 0])
(q.vars <- sort(intersect(q.vars, valid.vars)))

## Drop rows with no valid question responses
dim(poll.df <- poll.df[rowSums(!is.na(poll.df[q.vars])) > 0, ])
(q.vars <- sort(intersect(q.vars, names(poll.df))))
(fm.vars <- sort(intersect(fm.vars, names(poll.df))))
years.asked <- matrix(NA, nrow = length(yr.range), ncol = length(q.vars),
                      dimnames = list(yr.range, q.vars))
for (q in 1:length(q.vars)) {
  years.asked[, q] <- tapply(poll.df[, q.vars[q]], poll.df$YearFactor, anyValid)
  years.asked[, q][is.na(years.asked[, q])] <- FALSE
}
(q.vars <- q.vars[colSums(years.asked[, q.vars], na.rm=TRUE) >= min.years])
poll.df <- subset(poll.df,, c(fm.vars, q.vars))
dim(poll.df)

years.asked.melt <-
  melt(data.frame(years.asked[, q.vars],
                  YearFactor = as.integer(rownames(years.asked))),
       id="YearFactor")
poll.df$survey.year <-
  interaction(poll.df$YearFactor, poll.df$survey, drop=TRUE, lex.order=TRUE)
forms.asked <-
  daply(data.frame(!is.na(poll.df)), ~poll.df$survey.year, colSums) > 0
forms.asked[, q.vars] <- 
  forms.asked[, q.vars] * apply(forms.asked[, q.vars], 1, sum)
q.vars <- q.vars[colSums(forms.asked[, q.vars], na.rm=TRUE) >= min.polls]
forms.asked.melt <- melt(forms.asked[, q.vars])
names(forms.asked.melt) <- c('Poll', 'Question', 'nQuestions') 

for (q in seq_along(q.vars)) {
  LoopProgress(q, 10)
  levels.q <- levels(factor(poll.df[, q.vars[q]]))
  for (l in 1:(length(levels.q) - 1)) {
    poll.df[ncol(poll.df) + 1] <-
      as.integer(poll.df[, q.vars[q]] > levels.q[l])
    names(poll.df)[ncol(poll.df)] <-
      paste0(q.vars[q], ".gt", levels.q[l])
  }
}
(gt.vars <- sort(names(poll.df)[grep(".gt", names(poll.df))]))

valid.gt <- !is.na(poll.df[, gt.vars])
use.rows <- rowSums(is.na(poll.df[, covs.yr])) == 0 & rowSums(valid.gt) >= 1
gt.vars.use <- gt.vars[colSums(!is.na(poll.df[use.rows, gt.vars])) >= 1]
rm(list="valid.gt")
poll.df[, geo.var] <- droplevels(poll.df[, geo.var])

## Represent covariates in poll.df as model frame
MF <- model.frame(f0, data=poll.df[use.rows, ], na.action=return)
MF.yr <- model.frame(f0.yr, data=poll.df[use.rows, ], na.action=return)

## Factor with level for each combination of covariate values
group <- interaction(as.list(MF), sep="__", lex.order=FALSE)
group.yr <- interaction(as.list(MF.yr), sep="__", lex.order=FALSE)
## Matrix of every factor combination (reordered to match group factor order)
xtab.yr <- as.data.frame(table(MF.yr))
xtab <- subset(xtab.yr, YearFactor == yr.range[1], -c(YearFactor, Freq))
## Check that group.yr levels are in same order as rows of xtab.yr
stopifnot(identical(levels(group.yr),
                    unname(apply(subset(xtab.yr, select = -Freq), 1, paste,
                                 collapse="__"))))
xtab.yr$xtrow <- 1:nrow(xtab.yr)
## Dummy variable representation of contingency table
XX <- model.matrix(f0, xtab)
## don't use hierarchical model if only one predictor
if (length(covs) == 1) XX <- matrix(0, nrow(XX), ncol(XX))
YY <- as.matrix(poll.df[use.rows, gt.vars.use]) ## matrix of responses
table(n.responses <- rowSums(!is.na(YY), na.rm=TRUE))

## design effects
de.df <- data.frame(row = 1:sum(use.rows), Nresponses = n.responses, 
                    poll.df[use.rows, fm.vars])
de.df <- group_by_(de.df, .dots = lapply(covs.yr, as.symbol)) %>%
    mutate(de = 1 + (sd(useweight) / mean(useweight)) ^ 2,
           de = ifelse(is.na(de), 1, de))
de.df <- plyr::arrange(de.df, row)
summary(de.df)

## weighted sample size
Nstar <- function (y, de, nr) {
  ob.adj <- as.numeric(!is.na(y)) / nr ## weight by inverse of num responses
  n.star <- ceiling(sum(ob.adj / de)) ## reduce N by design effect
  return(n.star)
}
Sstar <- function (y, de, nr, wt) {
  y.bar.star <- weighted.mean(y, wt/nr, na.rm=TRUE)
  s.star <- round(Nstar(y, de, nr) * y.bar.star, digits=0)
  return(s.star)
}
NN.dt <- aaply(YY, 2, function (q.var) {
  gdf <- group_by_(data.frame(y = q.var, de.df),
                   .dots = lapply(covs.yr, as.symbol), drop = FALSE)
  sdf <- summarise(gdf, nstar = Nstar(y, de, Nresponses))
  mdf <- merge(x = xtab.yr, y = sdf, by = covs.yr, all.x = TRUE)
  mdf <- plyr::arrange(mdf, xtrow)
  out <- mdf$nstar
  names(out) <- levels(group.yr)
  return(out)
}, .progress = "text")
NN <- t(as.data.frame(NN.dt))
SS.dt <- aaply(YY, 2, function (q.var) {
  gdf <- group_by_(data.frame(y = q.var, de.df),
                   .dots = lapply(covs.yr, as.symbol), drop = FALSE)
  sdf <- summarise(gdf, sstar = Sstar(y, de, Nresponses, useweight))
  mdf <- merge(x = xtab.yr, y = sdf, by = covs.yr, all.x = TRUE)
  mdf <- plyr::arrange(mdf, xtrow)
  out <- mdf$sstar
  names(out) <- levels(group.yr)
  return(out)
}, .progress = "text")
SS <- t(as.data.frame(SS.dt))
NN.cmb <- NN
SS.cmb <- SS
(qs.used <- gt.vars.use)
NN.cmb <- NN.cmb[, qs.used]
SS.cmb <- SS.cmb[, qs.used]
## Replace NA with 0
NN.cmb[is.na(NN.cmb)] <- 0
SS.cmb[is.na(SS.cmb)] <- 0
stopifnot(all(SS.cmb - NN.cmb <= 0)) ## no more success than trials

## Years with surveys
(svy.yrs <- yr.range[laply(yr.range, function (yr) {
  any(NN.cmb[grepl(yr, rownames(NN.cmb)), ] > 0)
})])
svy.yr.range <- min(svy.yrs):max(svy.yrs)
two.digit.labels <- paste0("'", str_sub(unique(4*trunc(svy.yr.range/4)), 3, 4))

(T <- length(svy.yr.range)) ## some years may not have a question
(Q <- length(qs.used))
(Gr <- nlevels(group))
nat.vars <- demo.vars
if (is.null(nat.vars)) {
  demo.group <- gl(1, nrow(xtab))
} else { 
  demo.group <- interaction(as.list(xtab[, nat.vars, drop=FALSE]))
}
(Gnat <- nlevels(demo.group))

xtab.ds <- svydesign(~1, probs=1, data=xtab.yr)
nat.wts <- 1/xtab.ds$prob
WT <- array(nat.wts, dim=c(G, T, Gnat))
WT <- aperm(WT, c(2, 3, 1)) ## T x Gnat x G
for (i in seq_len(Gnat)) {
  (dg <- levels(demo.group)[i])
  WT[, i, !demo.group == dg] <- 0
  WT[, i, ] <- WT[, i, ] / rowSums(WT[, i, ])
  (WT[T, i, ] %*% XX)
}
apply(WT, 1, rowSums)

NN.nat <- array(FALSE, dim=dim(NN.cmb), dimnames=dimnames(NN.cmb))
nat_only <- array(0, dim=c(T, Q), dimnames=list(svy.yr.range, qs.used))

(N <- base::sum(NN.cmb > 0 & !NN.nat)) ## groups with data and not national-only
NNnat <- SSnat <- array(0, c(T, Q, Gnat), list(svy.yr.range, qs.used,
                                               levels(demo.group)))
n_vec <- s_vec <- integer(N) ## Vectors of trials and successes
names(n_vec) <- names(s_vec) <- seq_len(N)
MMM <- array(1, dim=list(T, Q, G)) ## Missingness indicator array
pos <- 0
for (t in 1:T) {
  print(yr <- svy.yr.range[t])
  if (!yr %in% svy.yrs) next
  yr.obs <- grep(yr, rownames(NN.cmb)) 
  for (q in 1:Q) {
    if (nat_only[t, q]) { ## if nation only
      print(c(t, q))
      for (h in seq_len(Gnat)) {
        gn.obs <- demo.group == levels(demo.group)[h]
        (wt.mn <- weighted.mean(SS.cmb[yr.obs, q] /
                                    NN.cmb[yr.obs, q],
                                WT[t, h, ], na.rm=TRUE))
        (NNnat[t, q, h] <- ceiling(sum(NN.cmb[yr.obs[gn.obs], q])))
        (SSnat[t, q, h] <- round(wt.mn * NNnat[t, q, h]))
      }
    } else {
      for (g in 1:G) {
        if (NN.cmb[yr.obs[g], q] == 0) next ## skip if no data
        pos <- pos + 1 
        MMM[t, q, g] <- 0 ## mark as not missing
        n_vec[pos] <- NN.cmb[yr.obs[g], q]
        s_vec[pos] <- SS.cmb[yr.obs[g], q]
        names(n_vec)[pos] <- names(s_vec)[pos] <-
          paste(rownames(NN.cmb)[yr.obs[g]],
                colnames(NN.cmb)[q], sep=" | ")
      }
    }
  }
}
stopifnot(identical(as.integer(pos), N))
quantile(n_vec, seq(.05, .95, .05))
table(n_vec)
table(s_vec)
mean(!MMM)

## Design matrix for model of hierarchical coefficients
## first-period priors
if (is.null(geo.mod.prior.vars)) {
  zz.prior.names <- list(svy.yr.range, levels(poll.df[, geo.var]), "Zero")
  ZZ.prior <- array(data=0, dim=llply(zz.prior.names, length),
                    dimnames=zz.prior.names)
  ZZ.prior <- ZZ.prior[as.character(svy.yr.range),,, drop=FALSE]
} else {
  if (geo.var == "StPOAbrv") {
    ZZ.prior <- melt(subset(st.dta,, -REGION4), id.vars=c("StPOAbrv", "year"))
    ZZ.prior <- acast(ZZ.prior, year ~ StPOAbrv ~ variable)
    ZZ.prior <- ZZ.prior[as.character(svy.yr.range),,
                         geo.mod.prior.vars, drop=FALSE]
  }
}
ZZp0 <- array(0, dim = c(dim(ZZ.prior)[1], ncol(XX) - dim(ZZ.prior)[2],
                     dim(ZZ.prior)[3]))
dim(ZZ.prior <- abind(ZZ.prior, ZZp0, along = 2))
## hierarchical priors
if (is.null(geo.mod.vars)) {
  zz.names <- list(svy.yr.range, levels(poll.df[, geo.var]), "Zero")
  ZZ <- array(data=0, dim=llply(zz.names, length), dimnames=zz.names)
} else {
  if (geo.var == "StPOAbrv") {
    ZZ <- melt(subset(st.dta,, -REGION4), id.vars=c("StPOAbrv", "year"))
    ZZ <- acast(ZZ, year ~ StPOAbrv ~ variable)
    ZZ <- ZZ[as.character(svy.yr.range),, geo.mod.vars, drop=FALSE]
  }
}
ZZ0 <- array(0, dim = c(dim(ZZ)[1], ncol(XX) - dim(ZZ)[2], dim(ZZ)[3]))
dim(ZZ <- abind(ZZ, ZZ0, along = 2))

stan.data <- list(n_vec = n_vec, ## response counts
                  s_vec = s_vec, ## group counts
                  NNnat = NNnat,
                  SSnat = SSnat,
                  XX = XX[, -1],
                  ##XX = array(0, dim = dim(XX[, -1])),
                  ZZ = ZZ[, -1, , drop = FALSE], ## geographic predictors
                  ZZ_prior = ZZ.prior[, -1, , drop = FALSE],
                  MMM = MMM, ## missingness array (T x Q x G)
                  G = G, ## number of covariate groups
                  Q = Q, ## number of questions (items)
                  T = T, ## number of time units (years)
                  N = N, ## number of observed group-question cells
                  P = ncol(XX[, -1]), ## number of hierarchical parameters
                  S = dim(ZZ)[[2]] - 1, ## number of geographic units
                  H = dim(ZZ)[[3]], ## number of geographic-level predictors
                  Hprior = dim(ZZ.prior)[[3]],
                  separate_years = separate.years, ## if 1, no pooling over time
                  constant_item = constant.item, ## if 1, difficulties constant
                  D = ifelse(constant.item, 1, T),
                  WT = WT, ## weight matrix for calculating national mean
                  nat_only = nat_only,
                  Gnat = Gnat ## number of national-level groups
                  )
str(stan.data)

paste('elapsed:', Sys.time() - t0)
save.image('original-workspace.Rdata')
# 14s

#
# ################################################################################
# #### STAN ######################################################################
# ################################################################################
#
# n.iter <- 2e3
# n.chain <- 2
# max.save <- 2e3
# n.warm <- min(1e4, floor(n.iter * 3/4))
# n.thin <- ceiling((n.iter - n.warm) / (max.save / n.chain))
# init.range <- 1
#
# cat(stan.code <- "
# data {
#   int<lower=1> G; ## number of covariate groups
#   int<lower=1> Gnat; ## number of national-level demographic groups
#   int<lower=1> Q; ## number of items/questions
#   int<lower=1> T; ## number of years
#   int<lower=1> N; ## number of observed cells
#   int<lower=1> S; ## number of geographic units (e.g., states)
#   int<lower=1> P; ## number of hierarchical parameters, including geographic
#   int<lower=1> H; ## number of predictors for geographic unit effects
#   int<lower=1> Hprior; ## number of predictors for geographic unit effects (t=1)
#   int<lower=1> D; ## number of difficulty parameters per question
#   int<lower=0,upper=1> constant_item; ## indicator for constant item parameters
#   int<lower=0,upper=1> separate_years; ## indicator for no over-time smoothing
#   int n_vec[N]; ## long vector of trials
#   int s_vec[N]; ## long vector of successes
#   int NNnat[T, Q, Gnat]; ## trials
#   int SSnat[T, Q, Gnat]; ## successes
#   int<lower=0> MMM[T, Q, G]; ## missingness array
#   matrix<lower=0, upper=1>[G, P] XX; ## indicator matrix for hierarchical vars.
#   matrix<lower=0, upper=1>[Gnat, G] WT[T]; ## weight array
#   matrix[P, H] ZZ[T]; ## data for geographic model
#   matrix[P, Hprior] ZZ_prior[T]; ## data for geographic model (prior)
#   matrix<lower=0, upper=1>[T, Q] nat_only;
# }
# transformed data {
# }
# parameters {
#   vector[Q] diff_raw[D]; ## raw difficulty
#   vector<lower=0>[Q] disc_raw; ## discrimination
#   vector[T] xi; ## common intercept
#   vector[P] gamma_raw[T]; ## hierarchical parameters (raw)
#   vector[T] delta_gamma; ## weight placed on gamma from prev. period
#   vector[H] nu_geo[T]; ## weight on geographic predictors
#   vector[Hprior] nu_geo_prior; ## weight on geographic predictors (t=1)
#   vector[T] delta_tbar; ## 
#   vector[G] theta_bar_raw[T]; ## group mean ability (raw) #!#
#   #!# vector[G] theta_bar[T]; ## group means 
#   vector<lower=0>[T] sd_theta_bar; ## residual sd of group ability means
#   vector<lower=0>[T] sd_theta; ## sd of abilities (by period)
#   real<lower=0> sd_gamma; ## prior sd of geographic effects
#   real<lower=0> sd_innov_delta; ## innovation sd of nu_geo and delta_gamma
#   real<lower=0> sd_innov_logsd; ## innovation sd of sd_theta
#   real<lower=0> sd_innov_gamma; ## innovation sd of gamma, xi, and (opt.) diff
# }
# transformed parameters {
#   vector[G] theta_bar[T]; ## group means (transformed) #!# 
#   vector[Q] diff[D]; ## adjusted difficulty
#   vector[Q] kappa[D]; ## threshold
#   vector<lower=0>[Q] disc; ## normalized discrimination
#   vector<lower=0>[Q] sd_item; ## item standard deviation
#   vector<lower=0>[Q] var_item; ## item variance
#   vector<lower=0>[T] var_theta; ## within-group variance of theta
#   ## var. of theta_bar w/in each nat. group **NOT CONSTRAINED TO BE POSITIVE**
#   vector[Gnat] var_theta_bar_nat[T];
#   vector[P] gamma[T]; ## hierarchical parameters (adjusted)
#   vector[G] mu_theta_bar[T]; ## linear predictor for group means
#   vector[P] mu_gamma[T];
#   vector[G] z[T, Q]; ## array of vectors of group deviates
#   vector[Gnat] z_nat[T, Q]; ## 
#   real<lower=0,upper=1> prob[T, Q, G]; ## array of probabilities
#   vector[Gnat] prob_nat[T, Q]; ## array of probabilities
#   vector[Gnat] theta_nat[T]; ## national-level group abililities
#   ## scale (product = 1)
#   disc <- disc_raw * pow(exp(sum(log(disc_raw))), (-inv(Q)));
#   for (q in 1:Q) {
#     sd_item[q] <- inv(disc[q]); ## item standard deviations
#   }
#   for (d in 1:D) {
#     ## location (mean in first year = 0)
#     diff[d] <- diff_raw[d] - mean(diff_raw[1]); 
#     kappa[d] <- diff[d] ./ disc; ## item thresholds
#   }
#   var_item <- sd_item .* sd_item;
#   var_theta <- sd_theta .* sd_theta;
#   for (t in 1:T) { ## loop over years
#     if (t == 1 || separate_years == 1) {
#       mu_gamma[t] <- ZZ_prior[t] * nu_geo_prior;
#       gamma[t] <- mu_gamma[t] + sd_gamma * gamma_raw[t];
#       mu_theta_bar[t] <- xi[t] + XX * gamma[t];
#       ##mu_theta_bar[t] <- XX * gamma[t];
#     }
#     if (t > 1 && separate_years == 0) {
#       if (t == 2) {
#         ## In the second year, agian use uniformative prior for gamma, rather
#         ## than one centered on its lagged value, because gamma is likely to be
#         ## very different in periods 1 and 2 because in only in 2 is
#         ## theta_bar[t - 1] used to inform theta_bar[t].
#         mu_gamma[t] <- ZZ_prior[t] * nu_geo_prior;
#         gamma[t] <- mu_gamma[t] + sd_gamma * gamma_raw[t]; 
#       } else {
#         mu_gamma[t] <- gamma[t - 1] * delta_gamma[t] + ZZ[t] * nu_geo[t]; 
#         gamma[t] <- mu_gamma[t] + sd_innov_gamma * gamma_raw[t];
#       }
#       mu_theta_bar[t] <- xi[t] + XX * gamma[t] + theta_bar[t - 1] * delta_tbar[t];
#       ##mu_theta_bar[t] <- theta_bar[t - 1] * delta_tbar[t] + XX * gamma[t];
#     }      
#     ## Matt trick for group means
#     theta_bar[t] <- mu_theta_bar[t] + sd_theta_bar[t] * theta_bar_raw[t]; #!# 
#     ## Weighted average of group means (weights must sum to 1)
#     theta_nat[t] <- WT[t] * theta_bar[t]; ## Gnatx1 = GnatxG * Gx1
#     for (n in 1:Gnat) {
#       matrix[G, G] WTdiag;
#       for (g in 1:G) {
#         for (h in 1:G) {
#           if (g == h) {
#             WTdiag[g, h] <- WT[t][n][g];
#           }
#           if (g != h) {
#             WTdiag[g, h] <- 0;
#           }
#         }
#       }
#       ## (y - w'y)' W (y - w'y) = weighted variance
#       var_theta_bar_nat[t][n] <- (theta_bar[t] - theta_nat[t, n])' * WTdiag *
#           (theta_bar[t] - theta_nat[t, n]);
#     }
#     for (q in 1:Q) { ## loop over questions
#       real sd_tq;
#       real sd_nat_tq[Gnat];
#       sd_tq <- sqrt(var_theta[t] + var_item[q]);
#       for (n in 1:Gnat) {
#         sd_nat_tq[n] <- sqrt(square(sd_tq) + var_theta_bar_nat[t, n]);
#       }
#       ## Group-level IRT model
#       if (constant_item == 0) {
#         z[t, q] <- (theta_bar[t] - kappa[t][q]) / sd_tq;
#         for (n in 1:Gnat) {
#           z_nat[t, q, n] <-
#             (theta_nat[t, n] - kappa[t][q]) / sd_nat_tq[n];
#           prob_nat[t, q, n] <- Phi_approx(z_nat[t, q, n]);
#         }
#       }
#       if (constant_item == 1) {
#         z[t, q] <- (theta_bar[t] - kappa[1][q]) / sd_tq;
#         for (n in 1:Gnat) {
#           z_nat[t, q, n] <-
#             (theta_nat[t, n] - kappa[1][q]) / sd_nat_tq[n];
#           prob_nat[t, q, n] <- Phi_approx(z_nat[t, q, n]);
#         }
#       }
#       for (g in 1:G) { ## loop over groups
#         prob[t, q, g] <- Phi_approx(z[t, q, g]); ## fast normal CDF
#       }
#     } ## end question loop
#   } ## end year loop
# }
# model {
#   ## TEMPORARY VARIABLES
#   real prob_vec[N]; ## long vector of probabilities (empty cells omitted)
#   int pos;
#   pos <- 0;
#   ## PRIORS
#   if (constant_item == 1) {
#     diff_raw[1] ~ normal(0, 1); ## item difficulty (constant)
#   }
#   disc_raw ~ lognormal(0, 1); ## item discrimination
#   sd_gamma ~ cauchy(0, 2.5); ## sd of geographic effects
#   sd_innov_delta ~ cauchy(0, 2.5); ## innovation sd of nu_geo, delta_gamma
#   sd_innov_gamma ~ cauchy(0, 2.5); ## innovation sd. of gamma, xi, and diff
#   sd_innov_logsd ~ cauchy(0, 2.5); ## innovation sd of theta_sd
#   for (t in 1:T) { ## loop over years
#     gamma_raw[t] ~ normal(0, 1);
#     theta_bar_raw[t] ~ normal(0, 1); ## Matt trick done above #!#
#     #!# theta_bar[t] ~ normal(mu_theta_bar[t], sd_theta_bar[t]); ## group means
#     if (t == 1) {
#       if (constant_item == 0) {
#         diff_raw[t] ~ normal(0, 1); ## item difficulty
#       }
#       ## Priors for first period
#       sd_theta_bar[t] ~ cauchy(0, 2.5);
#       sd_theta[t] ~ cauchy(0, 2.5);
#       nu_geo[t] ~ normal(0, 10); 
#       nu_geo_prior ~ normal(0, 10); 
#       delta_gamma[t] ~ normal(0.5, 0.5); ## 68% of prior mass btwn 0 and 1
#       delta_tbar[t] ~ normal(0.5, 0.5);
#       xi[t] ~ normal(0, 10); ## intercept
#     }
#     if (t > 1) {
#       ## TRANSITION MODEL
#       ## Difficulty parameters (if not constant)
#       if (constant_item == 0) { 
#         diff_raw[t] ~ normal(diff_raw[t - 1], sd_innov_gamma);
#       }
#       ## predictors in geographic models (random walk)
#       delta_gamma[t] ~ normal(delta_gamma[t - 1], sd_innov_delta);
#       nu_geo[t] ~ normal(nu_geo[t - 1], sd_innov_delta);
#       delta_tbar[t] ~ normal(delta_tbar[t - 1], sd_innov_delta);
#       sd_theta_bar[t] ~ lognormal(log(sd_theta_bar[t - 1]), sd_innov_logsd);
#       sd_theta[t] ~ lognormal(log(sd_theta[t - 1]), sd_innov_logsd);
#       if (separate_years == 0 && t > 2) {
#         xi[t] ~ normal(xi[t - 1], sd_innov_gamma);
#       }
#       if (separate_years == 1 || t == 2) { ## Estimate model anew each period
#         xi[t] ~ normal(0, 10);
#       }
#     }
#     for (q in 1:Q) { ## loop over questions
#       if (nat_only[t, q] == 1) {
#         ## National mean
#         SSnat[t, q] ~ binomial(NNnat[t, q], prob_nat[t, q]);
#       }
#       for (g in 1:G) { ## loop over groups
#         if (MMM[t, q, g] == 0) { ## Use only if not missing
#           pos <- pos + 1;
#           prob_vec[pos] <- prob[t, q, g];
#         }
#       } ## end group loop
#     } ## end question loop
#   } ## end time loop
#   ## Sampling model for group responses
#   s_vec ~ binomial(n_vec, prob_vec);
# }
# generated quantities {
#   vector<lower=0>[T] sd_total;
#   for (t in 1:T) {
#     sd_total[t] <- sqrt(variance(theta_bar[t]) + square(sd_theta[t]));
#   }
# }
# ")
#
# pars.to.save <- c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
#                   "nu_geo", "nu_geo_prior", "kappa", "sd_item",
#                   "sd_theta", "sd_theta_bar", "sd_gamma",
#                   "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
#                   "sd_total", "theta_nat", "var_theta_bar_nat")
# date()
# system.time(
#     stan.test <- stan(model_code=stan.code, data=stan.data, chains=1, iter=10,
#                       pars=pars.to.save, verbose=FALSE, seed=1,
#                       init = "random", init_r = init.range)
# )
# date()
#
# stopifnot(all.equal(prod(test_sds <-
#   extract(stan.test, pars="sd_item")$sd_item), 1))
# stopifnot(all.equal(exp(mean(test_diff <-
#   as.vector(extract(stan.test, pars="kappa")$kappa) / test_sds)), 1))
#
# cat("\nRunning ", n.iter, " iterations in each of ", n.chain,
#     " chains,\nthinned at an interval of ", n.thin, ", with ", n.warm,
#     " adaptation iterations,\nusing poll set ", poll.set, '-', q.set,
#     " over the years ", min(yr.range), "-", max(yr.range), ".'\n",
#     sep = "")
# qs.used
# covs
# txt.out
# date()
# system.time(
#     stan.par <- mclapply(1:n.chain, mc.cores = n.chain, FUN = function(chain) {
#       cat('\nStarting chain', chain, '\n')
#       out <- stan(model_code = stan.code, data = stan.data, iter = n.iter,
#                   chains = 1, warmup = n.warm, thin = n.thin, verbose = FALSE,
#                   chain_id = chain, refresh = max(floor(n.iter/100), 1),
#                   pars = pars.to.save, seed = chain, init = "random",
#                   init_r = init.range
#                   )
#       cat('Ending chain', chain, '\n\n')
#       return(out)
#     }))
# date()
#
#
#
