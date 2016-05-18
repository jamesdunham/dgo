library(Rcpp)
library(microbenchmark)

x <- item_data[, c("Q_issue_legmarijuana", "state", "year", "D_weight"), with = F]
y <- x[, lapply(.SD, function(x)
                   weighted.mean(x, .SD$D_weight, na.rm = TRUE)),
          .SDcols = c("Q_issue_legmarijuana", "D_weight"), by = c("state", "year")]
y[!is.na(y$Q_issue_legmarijuana), ]

z <- x[, lapply(.SD, function(x) weighted_mean(x, .SD$D_weight)),
          .SDcols = c("Q_issue_legmarijuana", "D_weight"), by = c("state", "year")]
z[!is.na(z$Q_issue_legmarijuana), ]

all.equal(z$Q_issue_legmarijuana, y$Q_issue_legmarijuana)
######

x <- item_data[, c("Q_issue_legmarijuana", "state", "year", "D_weight"), with = F]
x$n <- 1L
x$def <- 1L

# DataFrame dichotomize_cpp (NumericVector response) {
#
#   NumericVector rlevels = sort_unique(response);

sourceCpp("src/count_items.cpp")
count_items_cpp(c(1:4, NA), rep(1, 5), rep(2, 5))

cppFunction("
double def (NumericVector x, NumericVector ct, NumericVector def) {
  LogicalVector obs = !is_na(x);
  NumericVector x_obs = x[obs];
  NumericVector adj_wt = 1 + (sd(x_obs) / mean(x_obs)) ^ 2;
  return(ceil(sum(as<NumericVector>(obs) / ct / def)));
}")

sourceCpp('../src/weighted_mean.cpp')
ceiling(sum(as.integer(!is.na(x)) / n_responses / def, na.rm = TRUE))	
cppFunction("
double count_items_cpp (NumericVector x, NumericVector ct, NumericVector def) {
  LogicalVector obs = !is_na(x);
  return(ceil(sum(as<NumericVector>(obs) / ct / def)));
}")
set.seed(42)
x <- runif(1:1000)
x[sample(1:1000, 100)] <- NA
ct <- runif(1:1000)
def <- rep(1, 1000)


microbenchmark(
dgirt:::count_items(x, ct, def)
count_items_cpp(x, ct, def)

item_data$n_responses = 1
item_data$def  = 1.2
item_n <- item_data[, lapply(.SD, count_items, get("n_responses"), get("def")),			
                    .SDcols = c(gt_names),			
                    by = c(ctrl@geo_name, ctrl@group_names, ctrl@time_name)]

cppFunction("double weighted_mean (NumericVector x, NumericVector ct, NumericVector def, NumericVector wt) {
  LogicalVector obs = !is_na(x);
  double y = ceil(sum(as<NumericVector>(obs) / ct / def));
  // ceiling(sum(as.integer(!is.na(x)) / n_responses / def, na.rm = TRUE))	
  NumericVector x_obs = x[obs];
  NumericVector wt_obs = wt[obs];
  if (x_obs.size() > 0) {
    return(sum(x_obs * wt_obs / sum(wt_obs)));
  } else {
    return(NA_REAL);
  }
}")

set.seed(42)
x <- runif(1:1000)
wt <- runif(1:1000)
x[sample(1:1000, 100)] <- NA
ct <- rep(1, 1000)
def <- rep(1, 1000)
weighted_mean(x, ct, def, wt)

microbenchmark(
   weighted.mean(x, wt),
   weighted_mean(x, wt),
   times = 10000)


calc_design_effects <- function(x) {
  y <- 1 + (sd(x, na.rm = T) / mean(x, na.rm = T)) ^ 2
  ifelse(is.na(y), 1, y)
}

sourceCpp("src/count_items.cpp")
item_data <- opinion
gt_names <- grep("^Q_", names(item_data))
item_data[, c("n_responses") := list(rowSums(!is.na(.SD))), .SDcols = gt_names]
str(item_data)
item_data[[gt_names[1]]] <- as.numeric(item_data[[gt_names[1]]])
item_data[[gt_names[2]]] <- as.numeric(item_data[[gt_names[2]]])

sourceCpp("src/count_items.cpp")
count_items_cpp(item_data[[gt_names[1]]], item_data[["n_responses"]],
                item_data[["weight"]])


item_data$n_responses = 1
item_data$def  = 1.2
gt_names <- grep("^Q_", names(item_data), value = TRUE)
item_n <- item_data[, lapply(.SD, count_items, get("n_responses"), get("def")),			
                    .SDcols = c(gt_names),			
                    by = "state"]

count_items_cpp(item_data[[gt_names[1]]], item_data[["n_responses"]],
                item_data[["weight"]])

setDT(item_data)
item_data[, lapply(.SD, count_items_cpp, row_n = n_responses, wt = weight),
          .SDcols = gt_names] , by = "state"]

sourceCpp('src/aggregate_items.cpp')
x = as.data.frame(matrix(c(1:4, NA), ncol = 2))
agg_items(x, rowSums(!is.na(x)), rep(1:4))
as.matrix(cars)
