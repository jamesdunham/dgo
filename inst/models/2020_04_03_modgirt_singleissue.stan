functions {
  real p2l_real (real x) { // coverts scalar from probit to logit scale
    real y;
    y = 0.07056 * pow(x, 3) + 1.5976 * x;
    return y;
  }
  vector p2l_vector (vector x) { // coverts vector from probit to logit scale
    vector[num_elements(x)] y;
    for (i in 1:num_elements(x)) {
      y[i] = 0.07056 * pow(x[i], 3) + 1.5976 * x[i];
    }
    return y;
  }
}
data {
  int<lower=1> T;		  // number of years
  int<lower=1> G;		  // number of covariate groups
  int<lower=1> Q;		  // number of items/questions -> EDIT
  int<lower=1> D;		  // number of latent dimensions -> EDIT
  int<lower=1> K;		  // max number of answer options
  int<lower=1> P;		  // number of hierarchical predictors
  real<lower=0> SSSS[T, G, Q, K]; // number of responses (possibly non-integer) -> EDIT
  real beta_sign[Q, D];		  // sign restrictions on betas -> EDIT
  int unused_cut[Q, (K-1)];	  // indicates categories with no responses -> EDIT
  int<lower=0> N_nonzero;	       // number of non-zero elements of SSSS
  matrix<lower=0,upper=1>[G, P] XX;    // hier. preds. (includ. intercept)
  int<lower=0,upper=1> smooth_cross;   // indicator for hierarchical model
  int<lower=0,upper=1> smooth_time;    // indicator for no over-time smoothing
}
transformed data {
  matrix[G, P] XX_ctr;
  for (p in 1:P) {
    XX_ctr[1:G, p] = XX[1:G, p] - mean(XX[1:G, p]); // for interpretability
  }
}
parameters {
  real raw_bar_theta_N01[T, G, D]; // group means (pre-normalized, N(0,1) scale)
  ordered[K-1] alpha[Q];	   // thresholds (difficulty)
  vector<lower=0>[D] sd_raw_bar_theta_evolve_N01; // standard normal
  vector<lower=0>[D] sd_raw_bar_theta_evolve_IG;  // inverse-gamma
  real<lower=0> sd_xi_evolve_N01;	  // standard normal
  real<lower=0> sd_xi_evolve_IG;	  // inverse-gamma
  real<lower=0> B_cut;			  // slope for cutpoint prior
  vector[T] raw_xi;			  // year-specific intercept
  vector[1] delta_tbar;			  // lag coefficient
  vector[P-1] raw_gamma[1];		  // hierarchical parameters
}
transformed parameters {
  // Declarations
  real raw_bar_theta[T, G, D]; // group means (pre-normalized)
  real bar_theta[T, G, D];     // group means (normalized)
  vector[D] sd_raw_bar_theta_evolve;   // transition SD of theta
  vector[D] mean_raw_bar_theta;
  vector[D] sd_raw_bar_theta;
  real<lower=0> sd_xi_evolve;		  // evolution sd of xi
  // Assignments
  sd_raw_bar_theta_evolve =
    sd_raw_bar_theta_evolve_N01 .* sqrt(sd_raw_bar_theta_evolve_IG);
  sd_xi_evolve = sd_xi_evolve_N01 .* sqrt(sd_xi_evolve_IG);
  /// half-Cauchy(0, .1)
  for (t in 1:T) {
    if (t == 1 || smooth_time == 0) {
      for (g in 1:G) {
	for (d in 1:D) {
	  // implies raw_bar_theta[t, g, d] ~
	  // N(raw_xi[t] + XX_ctr*raw_gamma, 1)
	  raw_bar_theta[t, g, d] = raw_xi[t]
	    + XX_ctr[g, 2:P] * raw_gamma[1][1:(P-1)] * smooth_cross
	    + raw_bar_theta_N01[t, g, d];
	}
      }
    }
    if (t > 1 && smooth_time == 1) {
      for (g in 1:G) {
	for (d in 1:D) {
	  // implies raw_bar_theta[t] ~
	  // N(raw_xi[t] + delta_tbar * raw_bar_theta[t-1],
	  // sd_raw_bar_theta_evolve)
	  raw_bar_theta[t, g, d] = raw_xi[t]
	    + delta_tbar[1] * (raw_bar_theta[t-1, g, d] -
	    		       mean(raw_bar_theta[t-1, 1:G, d])) // centered
	    + sd_raw_bar_theta_evolve[d] * raw_bar_theta_N01[t, g, d]; 
	}
      }
    }
  }
  // Identify location and scale
  for (d in 1:D) {
    mean_raw_bar_theta[d] = mean(to_matrix(raw_bar_theta[1:T, 1:G, d]));
    sd_raw_bar_theta[d] = sd(to_matrix(raw_bar_theta[1:T, 1:G, d]));
    for (t in 1:T) {
      for (g in 1:G) {
	bar_theta[t, g, d] = (raw_bar_theta[t, g, d] - mean_raw_bar_theta[d])
	  ./ sd_raw_bar_theta[d];
      }
    }
  }
}
model {
  vector[N_nonzero] loglike_summands; // to store log-likelihood for summation
  int SSSS_pos = 0;
  // Priors
  for (q in 1:Q) {
    real used_cutp = K-1 - sum( unused_cut[q, 1:(K-1)] );
    real adjust_int = ( used_cutp / 2 ) + .5;
    real adjust_slp = 1;
    if (used_cutp > 1){
      adjust_slp = used_cutp - 1;
    }
    for (k in 1:(K-1)) {
      real priormean =
	100*unused_cut[q, k] + B_cut/adjust_slp*(k - adjust_int);
      alpha[q][k] ~ normal(priormean, 1);
    }
  }
  to_array_1d(raw_bar_theta_N01[1:T, 1:G, 1:D]) ~ normal(0, 1);
  sd_raw_bar_theta_evolve_N01 ~ normal(0, 1);	    // ditto
  sd_raw_bar_theta_evolve_IG ~ inv_gamma(0.5, 0.5); // ditto
  sd_xi_evolve_N01 ~ normal(0, 1);	    // sd_xi_evolve ~ cauchy(0, 1); 
  sd_xi_evolve_IG ~ inv_gamma(0.5, 0.5);    // ditto
  B_cut ~ normal(0, 1);
  delta_tbar ~ normal(.75, .25);
  raw_gamma[1] ~ normal(0, 10);
  for (t in 1:T) {
    if (t == 1 || smooth_time == 0) {
      raw_xi[t] ~ normal(0, 10);
    }
    if (t > 1 && smooth_time == 1) {
      raw_xi[t] ~ normal(raw_xi[t-1], sd_xi_evolve);
    }
  }
  // Likelihood
  for (t in 1:T) {
    for (q in 1:Q) { 
      vector[K-1] cut = p2l_vector(alpha[q][1:(K-1)]);
      for (g in 1:G) {
        for (k in 1:K) {
	  if (SSSS[t, g, q, k] > 0) {
	    real eta;
	    SSSS_pos += 1;
	    eta = p2l_real(bar_theta[t, g, 1]);
	    loglike_summands[SSSS_pos] =
	      SSSS[t, g, q, k] * ordered_logistic_lpmf(k | eta, cut);
	  }
        }
      }
    }
  }
  target += sum(loglike_summands);
}
generated quantities {
  vector[D] sd_bar_theta_evolve =
    sd_raw_bar_theta_evolve[1:D] ./ sd_raw_bar_theta;
  real<lower=0,upper=1> PPPP[T, G, Q, K];
  vector[T] xi;
  vector[P-1] gamma[1];			  
  for (d in 1:D) {
    gamma[1, 1:(P-1)] = raw_gamma[1, 1:(P-1)] ./ sd_raw_bar_theta[1];
  }
  for (t in 1:T) {
    for (d in 1:D) {
      xi[t] = (raw_xi[t] - mean_raw_bar_theta[d]) ./ sd_raw_bar_theta[d];
    }
    for (g in 1:G) {
      for (q in 1:Q) {
  	for (k in 1:K) {
  	  if (k == 1) {
  	    PPPP[t, g, q, k] =
	      1 - Phi_approx(bar_theta[t, g, 1] - alpha[q][k]);
  	  }
  	  if (k > 1 && k < K) {
  	    PPPP[t, g, q, k] =
  	      Phi_approx(bar_theta[t, g, 1] - alpha[q][k - 1]) -
  	      Phi_approx(bar_theta[t, g, 1] - alpha[q][k]);
  	  } if (k == K) {
  	    PPPP[t, g, q, k] =
  	      Phi_approx(bar_theta[t, g, 1] - alpha[q][k - 1]);
  	  }
  	}
      }
    }
  }
}
