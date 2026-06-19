// ============================================================
// Centennial climate reconstruction — sensitivity variant
// CHELSA-TraCE21k + Pfister-coded documentary evidence
//
// Differs from centennial_model.stan in ONE place only:
//   chelsa_sd is wired into the state equation as an additive
//   Gaussian uncertainty channel (chelsa_error ~ std_normal()).
//   Setting chelsa_sd = 0 in R recovers the canonical model.
//
// Identification note: chelsa_error and theta_centered both
//   contribute Gaussian noise to theta. With chelsa_sd constant
//   across centuries, sigma_innovation partially compensates for
//   changes in chelsa_sd. Report sigma_innovation posteriors
//   alongside theta posteriors when comparing grid cells.
// ============================================================

data {
  int<lower=1> N_centuries;
  int<lower=1> N_obs_centuries;
  array[N_obs_centuries] int<lower=1, upper=N_centuries> century_idx;

  int<lower=1> total_events;
  array[total_events] int<lower=1, upper=7> pfister_codes;
  array[N_obs_centuries] int<lower=1> events_count;

  vector[N_centuries] chelsa_mean;
  vector<lower=0>[N_centuries] chelsa_sd;   // varied from R across grid cells
}

parameters {
  vector[N_centuries] theta_centered;       // non-centred innovation
  vector[N_centuries] chelsa_error;         // CHELSA simulation uncertainty
  ordered[6] cutpoints;
  real<lower=0> sigma_innovation;
  real<lower=0, upper=1> rho;
}

transformed parameters {
  vector[N_centuries] theta;

  theta[1] = chelsa_mean[1]
           + chelsa_sd[1] * chelsa_error[1]
           + sigma_innovation * theta_centered[1];

  for (t in 2:N_centuries) {
    theta[t] = chelsa_mean[t]
             + chelsa_sd[t] * chelsa_error[t]
             + rho * (theta[t-1] - chelsa_mean[t-1])
             + sigma_innovation * theta_centered[t];
  }
}

model {
  theta_centered   ~ std_normal();
  chelsa_error     ~ std_normal();
  sigma_innovation ~ exponential(1);
  rho              ~ beta(3, 2);

  cutpoints[1] ~ normal(-3.0, 0.7);
  cutpoints[2] ~ normal(-1.5, 0.5);
  cutpoints[3] ~ normal(-0.5, 0.3);
  cutpoints[4] ~ normal( 0.5, 0.3);
  cutpoints[5] ~ normal( 1.5, 0.5);
  cutpoints[6] ~ normal( 3.0, 0.7);

  {
    int event_idx = 1;
    for (c in 1:N_obs_centuries) {
      int cent = century_idx[c];
      for (e in 1:events_count[c]) {
        pfister_codes[event_idx] ~ ordered_logistic(theta[cent], cutpoints);
        event_idx += 1;
      }
    }
  }
}

generated quantities {
  array[total_events] int pfister_pred;
  vector[N_centuries] shift_from_chelsa;

  for (t in 1:N_centuries)
    shift_from_chelsa[t] = theta[t] - chelsa_mean[t];

  {
    int event_idx = 1;
    for (c in 1:N_obs_centuries) {
      int cent = century_idx[c];
      for (e in 1:events_count[c]) {
        pfister_pred[event_idx] = ordered_logistic_rng(theta[cent], cutpoints);
        event_idx += 1;
      }
    }
  }
}
