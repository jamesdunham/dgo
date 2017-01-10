data {
  int<lower=1> G; ## number of covariate groups
  int<lower=1> G_hier; ## number of level-two demographic groups
  int<lower=1> Q; ## number of items/questions
  int<lower=1> T; ## number of years
  int<lower=1> N; ## number of cells
  int<lower=1> N_observed; ## number of observed cells
  int<lower=1> S; ## number of geographic parameters (e.g., for states)
  int<lower=1> P; ## number of hierarchical parameters, including geographic
  int<lower=1> H; ## number of predictors for geographic unit effects
  int<lower=1> Hprior; ## number of predictors for geographic unit effects (t=1)
  int<lower=1> D; ## number of difficulty parameters per question
  int<lower=0,upper=1> constant_item; ## indicator for constant item parameters
  int<lower=0,upper=1> separate_t; ## indicator for no over-time smoothing
  real delta_tbar_prior_mean;
  real<lower=0> delta_tbar_prior_sd;
  real<lower=0> innov_sd_delta_scale;
  real<lower=0> innov_sd_theta_scale;
  int<lower=0> n_vec[N];
  int<lower=0> s_vec[N];
  int observed[N_observed]; ## non-missing indexes for n_vec and s_vec
  int NNl2[T, Q, G_hier]; ## trials
  int SSl2[T, Q, G_hier]; ## successes
  matrix<lower=0, upper=1>[G, P] XX; ## indicator matrix for hierarchical vars.
  matrix<lower=0, upper=1>[G_hier, G] WT[T]; ## weight array
  matrix[P, H] ZZ[T]; ## data for geographic model
  matrix[P, Hprior] ZZ_prior[T]; ## data for geographic model (prior)
  matrix<lower=0, upper=1>[T, Q] l2_only;
}

transformed data {
}

parameters {
  vector[T] xi; ## common intercept
  vector[P] gamma_raw[T]; ## hierarchical parameters (raw)
  vector[T] delta_gamma; ## weight placed on gamma from prev. period
  vector[H] nu_geo[T]; ## weight on geographic predictors
  vector[Hprior] nu_geo_prior; ## weight on geographic predictors (t=1)
  vector[T] delta_tbar; ##
  vector[G] theta_bar_raw[T]; ## group mean ability (raw) #!#
  #!# vector[G] theta_bar[T]; ## group means
  vector<lower=0>[T] sd_theta_bar; ## residual sd of group ability means
  real<lower=0> sd_gamma_geo; ## prior sd of geographic coefficients
  real<lower=0> sd_gamma_demo; ## prior sd of demographic coefficients
  real<lower=0> sd_innov_delta; ## innovation sd of nu_geo and delta_gamma
  real<lower=0> sd_innov_logsd; ## innovation sd of sd_theta
  real<lower=0> sd_innov_gamma; ## innovation sd of gamma, xi, and (opt.) diff
}

transformed parameters {
  vector[G] theta_bar[T]; ## group means (transformed) #!#
  ## var. of theta_bar w/in each level-two group **NOT CONSTRAINED TO BE POSITIVE**
  vector[P] gamma[T]; ## hierarchical parameters (adjusted)
  vector[G] mu_theta_bar[T]; ## linear predictor for group means
  vector[P] mu_gamma[T];
  vector[G] z[T, Q]; ## array of vectors of group deviates
  real<lower=0,upper=1> prob[T, Q, G]; ## array of probabilities
  ## scale (product = 1)
 for (t in 1:T) { ## loop over years
    if (t == 1 || separate_t == 1) {
      mu_gamma[t] = ZZ_prior[t] * nu_geo_prior;
      for (p in 1:P) {
        if (p <= S) { ## if geographic coefficient
          gamma[t][p] = mu_gamma[t][p] + sd_gamma_geo * gamma_raw[t][p];
        }
        if (p > S) { ## if demographic coefficient
          gamma[t][p] = mu_gamma[t][p] + sd_gamma_demo * gamma_raw[t][p];
        }
      }
      mu_theta_bar[t] = xi[t] + XX * gamma[t];
      ##mu_theta_bar[t] = XX * gamma[t];
    }
    if (t > 1 && separate_t == 0) {
      if (t == 2) {
        ## 2016-02-05: need to think more about nu_geo_prior; make it different for geographic and demographic
        ## parameters
        ##
        ## In the second year, agian use uniformative prior for gamma, rather
        ## than one centered on its lagged value, because gamma is likely to be
        ## very different in periods 1 and 2 because only in 2 is
        ## theta_bar[t - 1] used to inform theta_bar[t].
        mu_gamma[t] = ZZ_prior[t] * nu_geo_prior;
        for (p in 1:P) {
          if (p <= S) { ## if geographic coefficient
            gamma[t][p] = mu_gamma[t][p] + sd_gamma_geo * gamma_raw[t][p];
          }
          if (p > S) { ## if demographic coefficient
            gamma[t][p] = mu_gamma[t][p] + sd_gamma_demo * gamma_raw[t][p];
          }
        }
      } else {
        ## 2016-02-05: maybe delta_gamma should differ for geographic and demographic parameters ;
        ## could do random walk DLM for demographic parameters
        for (p in 1:P) {
          if (p <= S) { ## if geographic coefficient
            mu_gamma[t][p] = gamma[t - 1][p]*delta_gamma[t] + ZZ[t][p]*nu_geo[t];
          }
          if (p > S) { ## if demographic coefficient
            mu_gamma[t][p] = gamma[t - 1][p]; ## random walk
          }
        }
        gamma[t] = mu_gamma[t] + sd_innov_gamma * gamma_raw[t];
      }
      mu_theta_bar[t] = xi[t] + XX * gamma[t] + theta_bar[t - 1] * delta_tbar[t];
      ##mu_theta_bar[t] = theta_bar[t - 1] * delta_tbar[t] + XX * gamma[t];
    }
    ## Matt trick for group means
    theta_bar[t] = mu_theta_bar[t] + sd_theta_bar[t] * theta_bar_raw[t]; #!#
    ## Weighted average of group means (weights must sum to 1)
    for (q in 1:Q) { ## loop over questions
      ## Group-level IRT model
      z[t, q] = theta_bar[t];
      for (g in 1:G) { ## loop over groups
	  ## Manifest variable
	  	prob[t, q, g] = Phi_approx(z[t, q, g]);
      }
    } ## end question loop
  } ## end year loop
}

model {
  ## TEMPORARY VARIABLES
  real prob_vec[N]; ## long vector of probabilities
  int pos;
  pos = 0;

  ## PRIORS
  sd_gamma_geo ~ cauchy(0, 2.5); ## sd of geographic effects
  sd_gamma_demo ~ cauchy(0, 2.5); ## sd of demographic effects
  sd_innov_delta ~ cauchy(0, innov_sd_delta_scale); ## innovation sd of nu_geo, delta_gamma
  sd_innov_gamma ~ cauchy(0, 2.5); ## innovation sd. of gamma, xi, and diff
  sd_innov_logsd ~ cauchy(0, innov_sd_theta_scale); ## innovation sd of theta_sd

  for (t in 1:T) { ## loop over years
    gamma_raw[t] ~ normal(0, 1);
    theta_bar_raw[t] ~ normal(0, 1); ## Matt trick done above #!#
    if (t == 1) {
      ## Priors for first period
      sd_theta_bar[t] ~ cauchy(0, 2.5);
      nu_geo[t] ~ normal(0, 1);
      nu_geo_prior ~ normal(0, 1);
      delta_gamma[t] ~ normal(0.5, 0.5); ## 68% of prior mass btwn 0 and 1
      delta_tbar[t] ~ normal(delta_tbar_prior_mean, delta_tbar_prior_sd);
      xi[t] ~ normal(0, 1); ## intercept
 }
    if (t > 1) {
      ## TRANSITION MODEL
      ## predictors in geographic models (random walk)
      delta_gamma[t] ~ normal(delta_gamma[t - 1], sd_innov_delta);
      nu_geo[t] ~ normal(nu_geo[t - 1], sd_innov_delta);
      delta_tbar[t] ~ normal(delta_tbar[t - 1], sd_innov_delta);
      sd_theta_bar[t] ~ lognormal(log(sd_theta_bar[t - 1]), sd_innov_logsd);
      if (separate_t == 0 && t > 2) {
        xi[t] ~ normal(xi[t - 1], sd_innov_gamma);
      }
      if (separate_t == 1 || t == 2) { ## Estimate model anew each period
        xi[t] ~ normal(0, 10);
      }
    }
    for (q in 1:Q) { ## loop over questions
      for (g in 1:G) { ## loop over groups
        pos = pos + 1;
        prob_vec[pos] = prob[t, q, g];
      } ## end group loop
    } ## end question loop

  } ## end time loop

  ## Sampling model for group responses
  s_vec[observed] ~ binomial(n_vec[observed], prob_vec[observed]);
}

generated quantities {
}
