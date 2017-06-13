#' Propagate uncertainty through the method of composition
#' 
#'  This function propagates uncertainty from variables measured with error by
#'  estimating a model on many samples of a dataset and then sampling from the
#'  distribution of the model parameters, a procedure known as the "method of
#'  composition" (Tanner 1996, 52; Treier and Jackman 2008).
#'  
#' @param data A data frame containing multiple samples from the
#'   measurement-error distribution of one or more variables.
#' @param model A fitted model object, from a model previously estimated (e.g.,
#'   on the whole dataset or one sample).
#' @param iter_var The name of the variable in \code{data} that identifies different
#'   samples.
#' @param iter_values Optionally, a vector of unique values of iteration variable
#'   \code{iter_var}, to which \code{data} will be subset.
#' @param vc_fun A function for extracting the variance-covariance matrix of the
#'   the parameters estimated by \code{model}.
#' @return Samples from distribution of model parameters.
#' @references 
#' 
#' Tanner, Martin A. 1996. \emph{Tools for Statistical Inference Methods for the
#' Exploration of Posterior Distributions and Likelihood Functions.} 3rd ed. New
#' York: Springer.
#' 
#' Treier, Shawn, and Simon Jackman. 2008. "Democracy as a Latent Variable." 
#' \emph{American Journal of Political Science} 52 (1): 201–217.
#' 
#' @author Devin Caughey
#' @import MASS
#' @examples
#' ### Randomly Generated Data
#' set.seed(1)
#' n_obs <- 100
#' n_samps <- 1000
#' y0 <- rnorm(n_obs)
#' x0 <- rnorm(n_obs)
#' samp.df <- data.frame(iteration=rep(seq_len(n_samps), each=n_obs),
#'                       x = rep(x0, n_samps) + rnorm(n_obs * n_samps),
#'                       y = rep(y0, n_samps))
#' mod_rand <- lm(y ~ x, data = subset(samp.df, iteration == 1))
#' summary(mod_rand)
#' moc_rand <- moc(samp.df, mod_rand)
#' colMeans(moc_rand) ## point estimate
#' apply(moc_rand, 2, sd) ## estimated standard error
#'
#' \dontrun{
#' if (require(sandwich, quietly = TRUE)) {
#'   ### DGO Output 
#'   dgirt_in_abortion <- shape(opinion, item_names = "abortion", time_name = "year",
#'                              geo_name = "state", group_names = "race3", 
#'                              geo_filter = c("CA", "GA", "LA", "MA"),
#'                              id_vars = "source")
#'  
#'   dgmrp_out_abortion <- dgmrp(dgirt_in_abortion, iter = 1500, chains = 4, 
#'                               cores = 4, seed = 42)
#'   d <- as.data.frame(dgmrp_out_abortion)
#'  
#'   samples = lapply(unique(d$iteration), function(x) {
#'     poststratify(d[iteration == x],
#'                  annual_state_race_targets, 
#'                  strata_names = c("state", "year"),
#'                  aggregated_names = "race3")
#'   })
#'   samples = rbindlist(samples, idcol = "iteration")
#'  
#'   ps <- poststratify(
#'     dgmrp_out_abortion, 
#'     annual_state_race_targets, 
#'     strata_names = c("state", "year"), 
#'     aggregated_names = "race3"
#'   )
#'  
#'   mod_dgmrp <- lm(value ~ 0 + state, ps)  
#'   summary(mod_dgmrp)
#'   sqrt(diag(sandwich::vcovHC(mod_dgmrp)))
#'  
#'   moc_dgmrp <- moc(data = samples, model = mod_dgmrp, vc_fun =
#'     sandwich::vcovHC)
#'   # point estimate
#'   colMeans(moc_dgmrp)
#'   # sd
#'   apply(moc_dgmrp, 2, sd)
#' }
#' }
moc <- function (data, model, iter_var = "iteration", iter_values = NULL, vc_fun =
  stats::vcov) {
  stopifnot(is.data.frame(data))
  stopifnot(inherits(model, "lm"))
  stopifnot(is.character(iter_var))
  stopifnot(iter_var %in% names(data))
  stopifnot(is.function(vc_fun))

  if (!length(iter_values)) {
    iter_values <- sort(unique(data[, iter_var]))
  }

  tildeB <- as.data.frame(matrix(nrow = length(iter_values), ncol =
      length(coef(model))))
  
  ## In each iteration...
  for (s in seq_along(iter_values)) {
    ## (1) Sample from p(X)
    data_s <- data[which(data[, iter_var] == iter_values[s]), ]
    
    ## (2) Sample from p(B|X_s):
    ##     (a) Estimate B_s and Cov(B_s) conditional on X_s.
    mod_s <- update(model, data = data_s)
    hatB_s <- coef(mod_s)
    hatV_s <- vc_fun(mod_s)
    
    ##     (b) Sample \tilde{B_s} from MV(\hat{B_s}, \hat{Cov(B_s)}).
    tildeB[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_s, Sigma = hatV_s)
  }
  model_names <- names(coef(model))
  stopifnot(length(model_names) == ncol(tildeB))
  names(tildeB) <- model_names
  ## samples from p(B), intergrating over p(X)
  return(tildeB)
}
