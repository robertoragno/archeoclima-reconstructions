// ============================================================
// Centennial climate reconstruction
// CHELSA-TraCE21k + Pfister-coded documentary evidence
//
// Model structure:
//   - AR(1) deviations from the CHELSA centennial prior
//   - Documentary observations modelled as ordered logistic
//   - rho ~ Beta(3, 2): mode ~0.67, mean 0.60
//
// References:
//   Pfister (1999); Luterbacher et al. (2004)
// ============================================================

data {
  int<lower=1> N_centuries;                              // Total centuries to reconstruct (9: 1000-1800)
  int<lower=1> N_obs_centuries;                          // Centuries with at least one documentary observation
  array[N_obs_centuries] int<lower=1, upper=N_centuries> century_idx;

  // Documentary observations (pooled across centuries)
  int<lower=1> total_events;
  array[total_events] int<lower=1, upper=7> pfister_codes;  // Pfister scale recoded to 1-7
  array[N_obs_centuries] int<lower=1> events_count;

  // CHELSA-TraCE21k centennial prior
  vector[N_centuries] chelsa_mean;        // Anomaly (°C or %)
  vector<lower=0>[N_centuries] chelsa_sd; // Prior uncertainty
}

parameters {
  vector[N_centuries] theta_centered;  // Non-centred parameterisation
  ordered[6] cutpoints;                // Boundaries between Pfister categories
  real<lower=0> sigma_innovation;      // Century-to-century variability
  real<lower=0, upper=1> rho;          // AR(1) persistence coefficient
}

transformed parameters {
  vector[N_centuries] theta;

  // Combine CHELSA prior with smooth AR(1) deviations
  theta[1] = chelsa_mean[1] + sigma_innovation * theta_centered[1];

  for (t in 2:N_centuries) {
    theta[t] = chelsa_mean[t]
             + rho * (theta[t-1] - chelsa_mean[t-1])  // AR(1) persistence
             + sigma_innovation * theta_centered[t];   // Stochastic innovation
  }
}

model {
  // Process priors
  theta_centered    ~ std_normal();
  sigma_innovation  ~ exponential(1);
  rho               ~ beta(3, 2);  // mode ~0.67, mean 0.60, sd ~0.20

  // Cutpoint priors: centred on evenly-spaced values, tighter towards zero
  cutpoints[1] ~ normal(-3.0, 0.7);
  cutpoints[2] ~ normal(-1.5, 0.5);
  cutpoints[3] ~ normal(-0.5, 0.3);
  cutpoints[4] ~ normal( 0.5, 0.3);
  cutpoints[5] ~ normal( 1.5, 0.5);
  cutpoints[6] ~ normal( 3.0, 0.7);

  // Likelihood: each documentary event updates its century
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

  // Posterior shift of each century away from the CHELSA prior
  for (t in 1:N_centuries) {
    shift_from_chelsa[t] = theta[t] - chelsa_mean[t];
  }

  // Posterior predictive samples
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
