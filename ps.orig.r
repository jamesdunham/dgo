## Load state-level data
mysw(data.dir, "state_data")
st.info <- read.csv("StateCodes.csv")
lower48 <- setdiff(as.character(st.info$POAbrv), c("AK", "HI", "DC"))
incom <- read.csv("states_income_real_19402010_yearly.csv") ## 1940
incom$StPOAbrv <- incom$abb
evang <- read.csv("states_evangelicals_19502010_yearly.csv") ## back to 1950
evang$StPOAbrv <- NameToPO(evang$state)
urban <- read.csv("states_urban_19502010_yearly.csv") ## 1950
urban$StPOAbrv <- urban$abb
union <- read.csv("states_union_19612010_yearly.csv") ## 1961
union$StPOAbrv <- NameToPO(union$state)
st.dta <- incom
st.dta <- merge(x=st.dta, y=evang, by=c("year", "StPOAbrv"), all.x=TRUE)
st.dta <- merge(x=st.dta, y=urban, by=c("year", "StPOAbrv"), all.x=TRUE)
st.dta <- merge(x=st.dta, y=union, by=c("year", "StPOAbrv"), all.x=TRUE)
st.dta <- plyr:::arrange(st.dta, year, StPOAbrv)
names(st.dta) <- gsub("percent", "prop", names(st.dta))
st.vars <- c("prop_evangelicals", "income_percapita", "prop_union",
             "prop_urban")
st.dta <- subset(st.dta,, c("year", "StPOAbrv", st.vars))
st.dta <- mutate(st.dta, REGION4=factor(POtoRegion4(StPOAbrv), exclude=""))
for (lev in levels(st.dta$REGION4)) {
  st.dta[lev] <- as.integer(st.dta$REGION4 == lev)
}
summary(st.dta)

### LOAD POLL DATA
if (grepl("quota", poll.set)) {
  ##topic.set <- "NonLabor"
  ##topic.set <- "Labor"
  topic.set <- "AllEcon"
  ##topic.set <-  "Race"
  poll.set <- paste0(poll.set, topic.set)
  drop.ideo <- TRUE
  min.years <- 1
  min.polls <- 1
  min.responses <- 1000
  st.dta <- subset(st.dta, StPOAbrv %in% lower48)
  mysw(data.dir, "Quota")
  poll.df <- ReadLatest("L3Topic")
  gc()
  if (!"weight" %in% names(poll.df)) poll.df$weight <- 1
  poll.df <- mutate(poll.df, weight=ifelse(weight > 0, weight, 1),
                    PHONE=factor(PHONE, levels=c('No Phone', 'Phone')),
                    PresMajor2=Recode(PresMajor, 'NA="Other/Non-Voter/Missing"'),
                    SOUTH13xBLACKxPRES=interaction(SOUTH13xBLACK, PresMajor2,
                        sep=' '),
                    SOUTH11xBLACKxPRES=interaction(SOUTH11xBLACK, PresMajor2,
                        sep=' '),
                    Voter = Recode(PresRetro, 
                        "'Did not vote' = 'Non-Voter'; else = 'Voter'",
                        as.factor.result=TRUE),
                    BLACKxVOTER=interaction(BLACK, Voter, sep=' '),
                    SOUTH13xBLACKxVOTER=interaction(SOUTH13xBLACK, Voter, sep=' '),
                    SOUTH11xBLACKxVOTER=interaction(SOUTH11xBLACK, Voter, sep=' '))
  (topic.vars <- sort(grepSub(names(poll.df), "^T[0-9]v")))
  (fm.vars <- setdiff(names(poll.df), topic.vars))
  if (drop.ideo) {
    ideo.df <- subset(poll.df,, grepSub(topic.vars, "^T2v090"))
    topic.vars <- grepSub(topic.vars, "^T2v090", drop=TRUE)
  }
  if (identical(topic.set, "AllEcon")) {
    topic.vars <- grepSub(topic.vars, "^T[12]v")
  }
  if (identical(topic.set, "Labor")) {
    topic.vars <- grepSub(topic.vars, "^T[1]v")
  }
  if (identical(topic.set, "NonLabor")) {
    topic.vars <- grepSub(topic.vars, "^T[2]v")
  }
  if (identical(topic.set, "Race")) {
    topic.vars <- grepSub(topic.vars, "^T[3]v")
  }
  q.vars <- topic.vars
  dim(poll.df <- poll.df[rowSums(!is.na(poll.df[, topic.vars])) > 0,
                         c(fm.vars, topic.vars)])
  min.yr <- max(c(min.yr, min(poll.df$Year, na.rm=TRUE)))
  max.yr <- min(c(max.yr, max(poll.df$Year, na.rm=TRUE)))
} else {
  if (grepl("modern", poll.set)) {
    q.subset <- "GovIsLib"
    min.years <- 1
    min.polls <- 1
    mysw(data.dir)
    load("GSS/GSS.RData")
    poll.df <- raw.poll.df <- ReadLatest("all_survey_keep")
    print(names(poll.df))
    (q.info <- ReadLatest("items_dimension",, "csv", stringsAsFactors=FALSE))
    q.cols <- grep("cces2006_minimumwage", names(poll.df)):ncol(poll.df)
    q.vars <- names(poll.df)[q.cols]
    stopifnot(all(q.vars %in% q.info$item))
    if (q.subset == "GovIsLib") {
      poll.set <- "modern-GovIsLib"
      q.vars <- q.info$item[q.info$lib_is_gov == 1]
      poll.df <- poll.df[, !names(poll.df) %in%
                         q.info$item[q.info$lib_is_gov == 0]]
      gss.NN <- gss.NN.gov
      gss.SS <- gss.SS.gov
      gss.df.table <- gss.df.table.gov 
    }
    if (q.subset == "AllLib") {
      poll.set <- "modern-AllLib"
      q.vars <- q.info$item
      gss.NN <- gss.NN.all
      gss.SS <- gss.SS.all
      gss.df.table <- gss.df.table.all
    }
    ## rename GSS questions asked on other polls
    colnames(gss.NN) <- ifelse(colnames(gss.NN) == "gunlaw_binary",
                               "issue_gunpermit", colnames(gss.NN))
    colnames(gss.NN) <- ifelse(colnames(gss.NN) == "cappun_binary",
                               "death_penalty", colnames(gss.NN))
    colnames(gss.SS) <- colnames(gss.NN) <- paste0(colnames(gss.NN), '.gt0')
    q.vars <- sort(setdiff(q.vars, "schoolprayer_amendment"))
    gss.vars <- colnames(gss.NN)
    print(c(q.vars, gss.vars))
  }
  if (poll.set == "judicial") {
    mysw(data.dir)
    poll.df <- ReadLatest("judicial_legitimacy")
    print(names(poll.df)) 
    (q.vars <- names(subset(poll.df,, sc_rating:judicial_therm)))
  }
  drop.cces <- TRUE
  if (drop.cces) {
    poll.df <- subset(poll.df, organization != "CCES")
  }
  ## State PO abbreviation
  table(poll.df$StPOAbrv <- factor(poll.df$abb, exclude=c("", "NA")))
  stopifnot(all(levels(poll.df$StPOAbrv) %in% levels(st.info$POAbrv)))
  poll.df <- subset(poll.df, StPOAbrv %in% levels(st.info$POAbrv))
  gss.df.table$StPOAbrv <- gss.df.table$STATEAB
  ## Education
  edu5.labels <- c("No High School Degree", "High School Grad",
                   "Some College", "College Graduate", "Graduate School")
  edu4.labels <- c("No High School Degree", "High School Grad",
                   "Some College", "College Graduate")
  gss.df.table <- mutate(gss.df.table,
                         Edu5=factor(education5, labels=edu5.labels),
                         Edu5=ordered(Edu5, levels=edu5.labels),
                         Edu4=Recode(Edu5, 'c("College Graduate",
                             "Graduate School")= "College Graduate"'),
                         Edu4=ordered(Edu4, levels=edu4.labels))
  poll.df <- mutate(poll.df,
                    Edu5=factor(education, labels=edu5.labels),
                    Edu5=ordered(Edu5, levels=edu5.labels),
                    Edu4=Recode(Edu5, 'c("College Graduate",
                        "Graduate School")= "College Graduate"'),
                    Edu4=ordered(Edu4, levels=edu4.labels))
  ## Female
  table(poll.df$Female <- factor(poll.df$gender, labels=c("Male", "Female")))
  table(gss.df.table$Female <- factor(gss.df.table$gender,
                                      labels=c("Male", "Female")))
  ## Race
  nlevels.race <- 3
  if (nlevels.race == 3) {
    race.labels <- c("White or Hispanic", "Black", "Other")
    if (grepl("modern", poll.set)) {
      poll.df$Race3 <- factor(poll.df$race_new, labels=race.labels)
    }
    if (poll.set == "judicial") {
      poll.df$Race3 <- factor(poll.df$race, labels=race.labels)
    }
    poll.df$Race2 <- "Non-Black"
    poll.df$Race2 <- ifelse(poll.df$Race3 == "Black", "Black",
                            poll.df$Race2)
    is.na(poll.df$Race2) <- is.na(poll.df$Race3)
    poll.df$Race2 <- factor(poll.df$Race2)
    ## GSS
    gss.df.table$Race3 <- factor(gss.df.table$race, labels=race.labels) 
    gss.df.table$Race2 <- "Non-Black"
    gss.df.table$Race2 <- ifelse(gss.df.table$Race3 == "Black", "Black",
                                 gss.df.table$Race2)
    is.na(gss.df.table$Race2) <- is.na(gss.df.table$Race3)
    gss.df.table$Race2 <- factor(gss.df.table$Race2) 
  }
  if (nlevels.race == 4) {
    race.labels <- c("White", "Black", "Hispanic", "Other")
    poll.df$Race4 <- factor(poll.df$race, labels=race.labels)
  }
  ## PID
  poll.df$PID3 <- ordered(poll.df$pid3, labels=c("D", "I", "R"))
  ## Year
  poll.df <- mutate(poll.df, Year=as.integer(year),
                    ElecYear=ifelse(month < 11, Year, Year + 1),
                    ElecYear=ifelse(month == 11 & !grepl("NES", source),
                        NA, ElecYear))
  gss.df.table <- mutate(gss.df.table, Year=as.integer(as.character(year)),
                         ElecYear=Year)
  ## weight variable
  if (!"weight" %in% names(poll.df)) poll.df$weight <- 1
  poll.df <- mutate(poll.df, weight=ifelse(weight > 0, weight, 1))
  (fm.vars <- setdiff(names(poll.df), q.vars))
  summary(poll.df)
  summary(gss.df.table)
  min.yr <- max(c(min.yr,
                  min(min(poll.df$Year, na.rm=TRUE), min(gss.df.table$Year))))
  max.yr <- min(c(max.yr,
                  max(max(poll.df$Year, na.rm=TRUE), max(gss.df.table$Year))))
}

## Drop years outside year range
(yr.range <- min.yr:max.yr)
if (change.at.election) {
  poll.df <- mutate(poll.df, YearFactor=factor(ElecYear, yr.range))
  if (any(grepl("gss", ls()))) {
    gss.df.table <- mutate(gss.df.table,
                           YearFactor=factor(ElecYear, yr.range))
  }
} else {
  poll.df <- mutate(poll.df, YearFactor=factor(Year, yr.range))
  if (any(grepl("gss", ls()))) {
    gss.df.table <- mutate(gss.df.table, YearFactor=factor(Year, yr.range))
  }
}
fm.vars <- c(fm.vars, "YearFactor")
dim(poll.df <- subset(poll.df, !is.na(YearFactor)))
gc()
## Drop variables with no valid responses
valid.vars <- names(poll.df)[sapply(poll.df, anyValid)]
(q.vars <- sort(intersect(q.vars, valid.vars)))
dim(poll.df <- subset(poll.df,, valid.vars))
## Drop rows with no valid question responses
dim(poll.df <- subset(poll.df, rowSums(!is.na(poll.df[q.vars])) > 0))
(q.vars <- sort(intersect(q.vars, names(poll.df))))
(fm.vars <- sort(intersect(fm.vars, names(poll.df))))
years.asked <- daply(data.frame(!is.na(poll.df)), ~poll.df$YearFactor, colSums)
(years.asked <- years.asked > 0)
(q.vars <- q.vars[colSums(years.asked[, q.vars], na.rm=TRUE) >= min.years])
poll.df <- subset(poll.df,, c(fm.vars, q.vars))
dim(poll.df)
gc()

if (any(grepl("gss", ls()))) {
  gss.years.asked <- ddply(data.frame(gss.df.table, gss.NN), ~YearFactor,
                           function (df) laply(df[gss.vars], sum))
  gss.years.asked[, -1] <- gss.years.asked[, -1] > 0
  colnames(gss.years.asked)[-1] <- colnames(gss.NN)
  years.asked.melt <-
    melt(merge(data.frame(years.asked[, q.vars],
                          YearFactor=as.integer(rownames(years.asked))),
               gss.years.asked, by="YearFactor", all=TRUE), id="YearFactor")
} else {
  years.asked.melt <-
    melt(data.frame(years.asked[, q.vars],
                    YearFactor=as.integer(rownames(years.asked))),
         id="YearFactor")
}
if (grepl("quota", poll.set)) {
  poll.df <- mutate(poll.df, PollForm=droplevels(PollForm))
  forms.asked <-
    daply(data.frame(!is.na(poll.df)), ~poll.df$PollForm, colSums) > 0
} else {
  poll.df$source.year <-
    interaction(poll.df$YearFactor, poll.df$source, drop=TRUE, lex.order=TRUE)
  forms.asked <-
    daply(data.frame(!is.na(poll.df)), ~poll.df$source.year, colSums) > 0
}    
forms.asked[, q.vars] <- 
  forms.asked[, q.vars] * apply(forms.asked[, q.vars], 1, sum)
q.vars <- q.vars[colSums(forms.asked[, q.vars], na.rm=TRUE) >= min.polls]
forms.asked.melt <- melt(forms.asked[, q.vars])
names(forms.asked.melt) <- c('Poll', 'Question', 'nQuestions') 

## setwd(plot.dir)
## qform.nm <- FileName(c(poll.set, "-qform"),, "pdf")
## pdf(qform.nm, height=3.5)
(ggplot(data=forms.asked.melt)
 + aes(y=factor(Question, levels=q.vars),
       x=factor(Poll, levels=rownames(forms.asked)),
       fill=nQuestions)
 + scale_fill_gradient(low="white", high="black", trans="sqrt"
                       , breaks=c(0, 1, 4, 9, 16))
 + theme_bw()
 + labs(y="Question", x="Poll", fill="# of\nQuestions\nAsked\nin Poll")
 + theme(axis.text=element_blank(), axis.ticks=element_blank())
 + geom_tile())
## dev.off()

## Transform ordinal questions into binary above/below threshold indicators
if (grepl("quota", poll.set)) {
  ## For quota polls, transform to binary lib/con because the set of possible
  ## responses is not consistent across all polls.
  for (q in seq_along(q.vars)) {
    LoopProgress(q, 10)
    col.q <- which(names(poll.df) == q.vars[q])
    poll.df[, col.q] <- as.integer(poll.df[, col.q] > 0)
    names(poll.df)[col.q] <- paste0(q.vars[q], ".gt", 0)
  }
} else {
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
}
(gt.vars <- sort(names(poll.df)[grep(".gt", names(poll.df))]))
gc()

### PREDICTOR VARIABLES
geo.var <- 'StPOAbrv'
if (grepl("quota", poll.set)) {
  demo.vars <- c('BLACK'##, 'PHONE'## , 'FARM', 'PROF', 'URBAN'
                 )
  pre.wt.vars.ls <- as.list(setdiff(c('FEMALE', 'PHONE', 'PROF'##, 'URBAN'
                                      ), demo.vars))
  geo.mod.vars <- c("Midwest", "Northeast", "South", "West")
  geo.mod.prior.vars <- c("Midwest", "Northeast", "South", "West")
} else {
  demo.vars <- c("Race2")
  pre.wt.vars.ls <- as.list(setdiff(c("Race2", "Female", "Edu4"), demo.vars))
  geo.mod.vars <- c("prop_evangelicals0", "prop_union0", "prop_urban0",
                    "income_percapita0")
  geo.mod.prior.vars <- c("prop_evangelicals0", "prop_union0", "prop_urban0" ,
                          "income_percapita0")
}
(covs <- c(geo.var, demo.vars))
covs.yr <- c(covs, "YearFactor")
pre.wt.vars.ls

### WEIGHTS
## Population targets
if (grepl("modern", poll.set) || grepl("judicial", poll.set)) {
  mysw(data.dir, "Targets")
  target.df <- ReadLatest("Targets1960to2010")
  # JD: reading in a dataframe in which each row is a demographic cell, and teh
  # final column, the frequency column, is the proportion fo the population
  # they are
  ## Eliminate 0's in population
  target.df <- ddply(target.df, ~Year, mutate,
                     Prop = ifelse(Prop == 0, 1e-10, Prop),
                     Prop = Prop / sum(Prop))
  if (min.yr < min(target.df$Year)) {
    for (yr in (min(target.df$Year) - 1):min.yr) {
      new.df <- mutate(subset(target.df, Year==min(target.df$Year)),
                       Year=yr)
      target.df <- rbind(new.df, target.df)
    }
  }
  if (max.yr > max(target.df$Year)) {
    for (yr in (max(target.df$Year + 1)):max.yr) {
      new.df <- mutate(subset(target.df, Year==max(target.df$Year)),
                       Year=yr)
      target.df <- rbind(target.df, new.df)
    }
  }
  # Application-specific recoding of variables
  target.df <- mutate(target.df,
                      Female=factor(ifelse(Female, "Female", "Male"),
                          levels(poll.df$Female)),
                      Race2=ifelse(race3 == '2', 'Black', 'Non-Black'),
                      Race2=factor(Race2, levels(poll.df$Race2)),
                      Edu5=factor(as.integer(education2), labels=edu5.labels),
                      Edu5=ordered(Edu5, levels=edu5.labels),
                      Edu4=Recode(Edu5, 'c("College Graduate",
                          "Graduate School") = "College Graduate"'),
                      Edu4=ordered(Edu4, levels=edu4.labels))
}
if (grepl("quota", poll.set)) {
  mysw(data.dir, "Targets")
  summary(target.df <- ReadLatest("Targets1930to1960"))
  target.df <- mutate(target.df,
                      Year=YEAR, 
                      FEMALE=factor(FEMALE, levels(poll.df$FEMALE)),
                      BLACK=factor(Recode(BLACK, '"Non-Black"="White"'),
                          levels=levels(poll.df$BLACK)),
                      SOUTH11=myRecode(POtoSouth11(StPOAbrv),
                          list(c("0", "Non-Southern"), c("1", "Southern"))),
                      SOUTH11xBLACK=interaction(SOUTH11, BLACK, sep=' '),
                      SOUTH13=myRecode(POtoSouth13(StPOAbrv),
                          list(c("0", "Non-Southern"), c("1", "Southern"))),
                      SOUTH13xBLACK=interaction(SOUTH13, BLACK, sep=' '))
  if ('PROF' %in% names(target.df)) {
    target.df$PROF=Recode(target.df$PROF,
        "'Non-Professional'='Not Professional'")
  }
  ## Interpolate missing years
  # JD: Don't include, but check
  (target.yrs <- sort(unique(target.df$Year)))
  for (y in seq_along(target.yrs)) {
    if (y == 1) next
    if (target.yrs[y] - target.yrs[y - 1] < 2) next
    (inter.yrs <- (target.yrs[y - 1] + 1):(target.yrs[y] - 1))
    for (i in seq_along(inter.yrs)) {
      (yr.i <- inter.yrs[i])
      dta.i <- subset(target.df, Year == target.yrs[y])
      dta.i <- mutate(dta.i, Year = yr.i, YEAR = yr.i)
      prop <- Interpolate(last.yr = target.yrs[y - 1],
                          next.yr = target.yrs[y], this.yr = yr.i,
                          last.value = subset(target.df,
                              Year == target.yrs[y - 1], Prop),
                          next.value = subset(target.df,
                              Year == target.yrs[y], Prop))
      dta.i$Prop <- prop$Prop
      target.df <- rbind(subset(target.df, Year < yr.i), dta.i,
                         subset(target.df, Year > yr.i))
    }
  }
}
# JD: from the svy package; all built around a svy.design object, which is a
# data.frame along with auxiliary information about how it was created and
# sampled.
target.ds <- svydesign(~1, weights=~Prop, data=target.df)

## Weight (PS or rake) w/in each group whose mean will be estimated
# JD: This is the list of formulas that describe how we'll create weights, if
# we want to (this isn't national weights; can be ignoredf or now and we'll
# take weights as given.
(pre.wt.form.ls <- llply(pre.wt.vars.ls, function (pwv) {
  vars <- c(pwv, demo.vars)
  ## drop unweightable variables
  vars <- gsub("\\<SOUTH13xBLACKxPRES\\>", "SOUTH13xBLACK", vars)
  vars <- gsub("\\<SOUTH11xBLACKxVOTER\\>", "SOUTH11xBLACK", vars)
  ## avoid geographic redundancy
  vars <- gsub("\\<SOUTH11xBLACK\\>", "BLACK", vars)
  vars <- gsub("\\<SOUTH13xBLACK\\>", "BLACK", vars)
  vars <- gsub("\\<WHITESOUTH13xBLACK\\>", "BLACK", vars)
  vars <- unique(c(vars, geo.var))
  Formula(vars)
}))
(f0 <- Formula(c("0", covs)))
f0.yr <- Formula(c("0", covs.yr))

## Rows with no missing weighting variables or covariates and >0 valid responses
valid.gt <- !is.na(poll.df[, gt.vars])
table(rowSums(valid.gt))
(wt.covs.yr <- unique(c(unlist(pre.wt.vars.ls), covs.yr)))
use.rows <- rowSums(is.na(poll.df[, wt.covs.yr])) == 0 & rowSums(valid.gt) >= 1
## Items used
gt.vars.use <- gt.vars[colSums(!is.na(poll.df[use.rows, gt.vars])) >= 1]
rm(list="valid.gt")
gc()
## Represent covariates in poll.df as model frame
MF <- model.frame(f0, data=poll.df[use.rows, ], na.action=return)
gc()
MF.yr <- model.frame(f0.yr, data=poll.df[use.rows, ], na.action=return)
## Factor with level for each combination of covariate values
gc()
group <- interaction(as.list(MF), sep="__", lex.order=FALSE)
group.yr <- interaction(as.list(MF.yr), sep="__", lex.order=FALSE)
## Matrix of every factor combination (reordered to match group factor order)
xtab.yr <- as.data.frame(table(MF.yr))
xtab <- subset(xtab.yr, YearFactor == yr.range[1], -c(YearFactor, Freq))
## Check that group.yr levels are in same order as rows of xtab.yr
stopifnot(identical(levels(group.yr),
                    unname(apply(subset(xtab.yr,, -Freq), 1, paste,
                                 collapse="__"))))
xtab.yr$xtrow <- 1:nrow(xtab.yr)
## Dummy variable representation of contingency table
## Note perfect collinearity 
XX <- model.matrix(f0, xtab)
## don't use hierarchical model if only one predictor
if (length(covs) == 1) XX <- matrix(0, nrow(XX), ncol(XX))
YY <- as.matrix(poll.df[use.rows, gt.vars.use])
table(n.responses <- rowSums(!is.na(YY), na.rm=TRUE))
gc()

## design effects
de.df <- data.frame(row=1:sum(use.rows), Nresponses = n.responses, 
                    poll.df[use.rows, fm.vars])
RakeWeight <- function (df, formula.list, target.design) {
  ds <- svydesign(~1, weights=~weight, data=df)
  pop.list <- llply(formula.list, svytable, design=target.design)
  rk <- RakePartial(ds, formula.list, pop.list)
  return (1/rk$prob)
}
## renormalize weights to be have mean of 1 within polls
if (grepl("quota", poll.set)) {
  de.df <- (group_by(de.df, PollForm) %>%
                mutate(weight=weight/mean(weight)))
} else {
  de.df <- (group_by(de.df, source, YearFactor) %>%
                mutate(weight=weight/mean(weight)))
}
## In each year, create weights that match specified targets
de.df <- ddply(de.df, ~YearFactor, function (df) {
  ds <- subset(target.ds, target.ds$variables$Year==unique(df$YearFactor))
  df$new_weight <- RakeWeight(df, pre.wt.form.ls, ds)
  df <- mutate(df, new_weight = new_weight/mean(new_weight, na.rm=TRUE))
  return(df)
}, .progress = "text")
## calculate design effect within groups
de.df <- group_by_(de.df, .dots = lapply(covs.yr, as.symbol)) %>%
    mutate(de = 1 + (sd(new_weight)/mean(new_weight))^2,
           de = ifelse(is.na(de), 1, de))
de.df <- plyr:::arrange(de.df, row)
summary(de.df)

## weighted sample size
gc()
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
  mdf <- plyr:::arrange(mdf, xtrow)
  out <- mdf$nstar
  names(out) <- levels(group.yr)
  return(out)
}, .progress = "text")
gc()
NN <- t(as.data.frame(NN.dt))
gc()
SS.dt <- aaply(YY, 2, function (q.var) {
  gdf <- group_by_(data.frame(y = q.var, de.df),
                   .dots = lapply(covs.yr, as.symbol), drop = FALSE)
  sdf <- summarise(gdf, sstar = Sstar(y, de, Nresponses, new_weight))
  mdf <- merge(x = xtab.yr, y = sdf, by = covs.yr, all.x = TRUE)
  mdf <- plyr:::arrange(mdf, xtrow)
  out <- mdf$sstar
  names(out) <- levels(group.yr)
  return(out)
}, .progress = "text")
SS <- t(as.data.frame(SS.dt))
gc()

if (any(grepl("gss", ls())) && all(covs %in% names(gss.df.table))
    && max.yr > 1972) {
  ## collapse GSS data and merge with other poll data
  gss.vars.use <- gss.vars[laply(gss.years.asked[, gss.vars], any)]
  gss.NN.sub <- aaply(gss.NN[, gss.vars.use], 2, function (q.var) {
    daply(subset(data.frame(gss.df.table, q.var), Year %in% yr.range),
          Formula(covs.yr), function (df) sum(df$q.var))
  }, .progress = "text")
  gss.NN.sub <- t(as.data.frame(gss.NN.sub))
  gss.SS.sub <- aaply(gss.SS[, gss.vars.use], 2, function (q.var) {
    daply(subset(data.frame(gss.df.table, q.var), Year %in% yr.range),
          Formula(covs.yr), function (df) sum(df$q.var))
  }, .progress = "text")
  gss.SS.sub <- t(as.data.frame(gss.SS.sub))
  q.overlap <- intersect(colnames(NN), colnames(gss.NN.sub))
  for (i in seq_along(q.overlap)) {
    qo <- q.overlap[i]
    print(qo)
    NN[, qo] <- NAto0(NN[, qo]) + NAto0(gss.NN.sub[, qo])
    SS[, qo] <- NAto0(SS[, qo]) + NAto0(gss.SS.sub[, qo])
  }
  gss.vars.use <- setdiff(gss.vars.use, q.overlap)
  gss.NN.sub <- gss.NN.sub[, gss.vars.use]
  gss.SS.sub <- gss.SS.sub[, gss.vars.use]
  NN.cmb <- cbind(NN, gss.NN.sub)
  SS.cmb <- cbind(SS, gss.SS.sub)
  (qs.used <- c(gt.vars.use, gss.vars.use))
} else {
  NN.cmb <- NN
  SS.cmb <- SS
  (qs.used <- gt.vars.use)
}
NN.cmb <- NN.cmb[, qs.used]
SS.cmb <- SS.cmb[, qs.used]
## Replace NA with 0
NN.cmb[is.na(NN.cmb)] <- 0
SS.cmb[is.na(SS.cmb)] <- 0
stopifnot(all(SS.cmb - NN.cmb <= 0)) ## no more success than trials

cbind(yr.range,
      laply(yr.range, function (yr) sum(NN.cmb[grepl(yr, rownames(NN.cmb)), ])))

## Years with surveys
(svy.yrs <- yr.range[laply(yr.range, function (yr) {
  any(NN.cmb[grepl(yr, rownames(NN.cmb)), ] > 0)
})])
svy.yr.range <- min(svy.yrs):max(svy.yrs)
two.digit.labels <- paste0("'", str_sub(unique(4*trunc(svy.yr.range/4)), 3, 4))

## setwd(plot.dir)
## qyr.nm <- FileName(c(poll.set, "-qyr"),, "pdf", replace=TRUE)
## pdf(qyr.nm, height=10, width=12)
(ggplot(subset(years.asked.melt, sub("\\.gt[0-9]*", "", variable) %in%
               sub("\\.gt[0-9]*", "", qs.used) & YearFactor %in% svy.yr.range),
        aes(x=variable, y=factor(YearFactor), fill=value))
 + scale_fill_manual(values=c("white", "steelblue"))
 + theme_bw()
 + guides(fill=FALSE)
 + labs(x="Question", y=element_blank())
 + theme(axis.text.x=element_text(size=rel(1), angle=60, hjust=1, vjust=1),
         axis.text=element_text(size=7), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
 + coord_flip()
 + geom_tile(color="grey")
 )
## dev.off()

(T <- length(svy.yr.range)) ## some years may not have a question
(Q <- length(qs.used))
(G <- nlevels(group))
nat.vars <- demo.vars
if (is.null(nat.vars)) {
  demo.group <- gl(1, nrow(xtab))
} else { 
  demo.group <- interaction(as.list(xtab[, nat.vars, drop=FALSE]))
}
(Gnat <- nlevels(demo.group))

## Weights
if ("WHITESOUTH13xBLACK" %in% covs.yr) {
  xtab.yr$BLACK <- factor(ifelse(xtab.yr$WHITESOUTH13xBLACK=='Black', 'Black',
                                 'White'), levels=levels(target.df$BLACK))
}
if ("SOUTH13xBLACK" %in% covs.yr) {
  xtab.yr$BLACK <- factor(ifelse(grepl('Black', xtab.yr$SOUTH13xBLACK),
                                 'Black', 'White'), levels=levels(target.df$BLACK))
}
if ("WHITESOUTH11xBLACK" %in% covs.yr) {
  xtab.yr$BLACK <- factor(ifelse(xtab.yr$WHITESOUTH11xBLACK=='Black', 'Black',
                                 'White'), levels=levels(target.df$BLACK))
}
if ("SOUTH11xBLACK" %in% covs.yr) {
  xtab.yr$BLACK <- factor(ifelse(grepl('Black', xtab.yr$SOUTH11xBLACK),
                                 'Black', 'White'), levels=levels(target.df$BLACK))
}
post.wt.vars <- covs.yr
(post.wt.vars <- gsub("\\<WHITESOUTH13xBLACK\\>", "BLACK", post.wt.vars))
(post.wt.vars <- gsub("\\<SOUTH13xBLACK\\>", "BLACK", post.wt.vars))
(post.wt.vars <- gsub("\\<WHITESOUTH11xBLACK\\>", "BLACK", post.wt.vars))
(post.wt.vars <- gsub("\\<SOUTH11xBLACK\\>", "BLACK", post.wt.vars))
(post.wt.vars <- gsub("\\<SOUTH11xBLACKxPRES\\>", 1, post.wt.vars))
(post.wt.vars <- gsub("\\<SOUTH13xBLACKxPRES\\>", 1, post.wt.vars))
(post.wt.vars <- gsub("\\<SOUTH11xBLACKxVOTER\\>", 1, post.wt.vars))
(post.wt.vars <- gsub("\\<SOUTH13xBLACKxVOTER\\>", 1, post.wt.vars))
target.ds <- update(target.ds, YearFactor=factor(Year, svy.yr.range))
xtab.ds <- svydesign(~1, probs=1, data=xtab.yr)
# JD: This is where we get the national weights from. These steps aren't
# present in the packageTemplate.r file DC gave me.
xtab.ps <- postStratify(xtab.ds, Formula(post.wt.vars),
                        svytable(Formula(post.wt.vars), target.ds))
nat.wts <- 1/xtab.ps$prob
WT <- array(nat.wts, dim=c(G, T, Gnat))
WT <- aperm(WT, c(2, 3, 1)) ## T x Gnat x G
for (i in seq_len(Gnat)) {
  (dg <- levels(demo.group)[i])
  WT[, i, !demo.group == dg] <- 0
  WT[, i, ] <- WT[, i, ] / rowSums(WT[, i, ])
  (WT[T, i, ] %*% XX)
}
apply(WT, 1, rowSums)

## national-only
NN.nat <- array(FALSE, dim=dim(NN.cmb), dimnames=dimnames(NN.cmb))
nat_only <- array(0, dim=c(T, Q), dimnames=list(svy.yr.range, qs.used))
if (yes.nat.only.qs) {
  q.by.org.year <- ddply(poll.df[use.rows, ], ~YearFactor + organization,
                         function(x) colSums(!is.na(x[, gt.vars.use])) > 0)
  nat.only <- subset(q.by.org.year, organization %in% c('ANES'))
  nat.only <- melt(nat.only, id.vars = c('YearFactor', 'organization'))
  gss.nat <- mutate(as.data.frame(gss.NN),
                    YearFactor=str_sub(rownames(gss.NN), -4, -1),
                    YearFactor=factor(YearFactor, levels=svy.yr.range))
  gss.nat <- ddply(subset(gss.nat, YearFactor %in% svy.yr.range), ~YearFactor,
                   function(x) colSums(subset(x,, -YearFactor)) > 0)
  gss.nat <- mutate(gss.nat, organization='GSS')
  gss.nat <- melt(gss.nat, id.vars = c('YearFactor', 'organization'))
  nat.only <- rbind(nat.only, gss.nat)
  nat.only <- subset(nat.only, value, -value)
  (nat.only <- plyr:::arrange(nat.only, YearFactor, variable))
  for (i in 1:nrow(nat.only)) {
    (yr <- nat.only$YearFactor[i])
    (var <- nat.only$variable[i])
    NN.nat[grep(yr, rownames(NN.cmb)), var] <- TRUE
    (t <- grep(yr, svy.yr.range))
    (q <- grep(var, qs.used))
    nat_only[t, q] <- 1
  }
}
mean(NN.nat)
(N <- base:::sum(NN.cmb > 0 & !NN.nat)) ## groups with data and not national-only

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
gc()
quantile(n_vec, seq(.05, .95, .05))
table(n_vec)
table(s_vec)
mean(!MMM)

## Extrapolate years not covered by state file
yrs.before <- min(st.dta$year) - min.yr
yrs.after <- max.yr - max(st.dta$year)
if (yrs.before > 0) {
  rows.before <- which(st.dta$year == min(st.dta$year))
  rows.before <- rep(rows.before, times = yrs.before)
  tmp <- st.dta[rows.before, ]
  is.na(tmp[, st.vars]) <- TRUE
  tmp$year <- tmp$year -
      rep(yrs.before:1, each=sum(st.dta$year == min(st.dta$year)))
  st.dta <- rbind(tmp, st.dta)
}
if (yrs.after > 0) {
  rows.after <- which(st.dta$year == max(st.dta$year))
  rows.after <- rep(rows.after, times = yrs.after)
  tmp <- st.dta[rows.after, ]
  is.na(tmp[, st.vars]) <- TRUE
  tmp$year <- tmp$year +
      rep(1:yrs.after, each=sum(st.dta$year == max(st.dta$year)))
  st.dta <- rbind(st.dta, tmp)
}
for (v in seq_along(st.vars)) {
  (sv <- st.vars[v])
  (min.valid.yr <- min(st.dta$year[!is.na(st.dta[sv])]))
  min.valid.obs <- which(st.dta$year == min.valid.yr)
  (max.valid.yr <- max(st.dta$year[!is.na(st.dta[sv])]))
  max.valid.obs <- which(st.dta$year == max.valid.yr)
  if (min.yr < min.valid.yr) {
    for (i in seq_len(min.valid.yr - min.yr)) {
      st.dta[min.valid.obs - i*nlevels(poll.df$StPOAbrv), sv] <-
        st.dta[min.valid.obs, sv]
    }
  }
  if (max.yr > max.valid.yr) {
    for (i in seq_len(max.yr - max.valid.yr)) {
      st.dta[max.valid.obs + i*nlevels(poll.df$StPOAbrv), sv] <-
        st.dta[max.valid.obs, sv]
    }
  }
}
## standardize by year
st.dta <- ddply(st.dta, ~year, mutate,
                prop_evangelicals0=scale(prop_evangelicals),
                income_percapita0=scale(income_percapita),
                prop_union0=scale(prop_union),
                prop_urban0=scale(prop_urban))
summary(st.dta)

## Design matrix for model of hierarchical coefficients
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
## Now move to ...-Stan.R
