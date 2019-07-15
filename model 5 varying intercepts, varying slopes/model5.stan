
data {
  int<lower=1> N;
  
  int<lower=1> N_neighborhood;
  
  vector[N] Price;
  
  vector[N] Sqm;
  vector[N] CondGoodDummySqm;
  vector[N] Age;
  vector[N] TwoRoomsDummy;
  vector[N] ThreeRoomsDummy;
  vector[N] FourRoomsOrMoreDummy;
  vector[N] OwnFloor;
  vector[N] SaunaDummy; 
  
  int NeighborhoodAssignment[N]; 
}

parameters{
  // group-invariant coeffients
  real Age_coef;
  real TwoRoomsDummy_coef;
  real ThreeRoomsDummy_coef;
  real FourRoomsOrMoreDummy_coef;
  real SaunaDummy_coef;
  real OwnFloor_coef; 
  
  // variance paramters for the likelihood
  real<lower=0> sigma; 
  real<lower=0> nu; 
  
  // parameters for varying intercept, slopes 
  corr_matrix[3] Rho;
  
  real<lower=0> Sigma_Intercept_coef;
  real<lower=0> Sigma_Sqm_coef;
  real<lower=0> Sigma_CondGoodSqm_coef;
  
  real Mu_Intercept_coef;
  real Mu_Sqm_coef;
  real Mu_CondGoodSqm_coef;
  
  // offsets for non-centered parameterization for the varying intercept, slopes
  vector[3] offset[N_neighborhood];
}
transformed parameters{
  vector<lower=0>[3] sigma_Neighborhood;
  vector[3] Mu_VaryingSlopes;
  
  // https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html
  matrix[3,3] Rho_choleskyFactor; 
  
  /* EV for slopes */ 
  Mu_VaryingSlopes[1] = Mu_Intercept_coef;
  Mu_VaryingSlopes[2] = Mu_Sqm_coef; 
  Mu_VaryingSlopes[3] = Mu_CondGoodSqm_coef; 
  
  /* Sigma for slopes */ 
  sigma_Neighborhood[1] = Sigma_Intercept_coef;
  sigma_Neighborhood[2] = Sigma_Sqm_coef;
  sigma_Neighborhood[3] = Sigma_CondGoodSqm_coef; 
  
  Rho_choleskyFactor = cholesky_decompose(Rho);
}

model{
  vector[N] mu;
  vector[3] VaryingSlopes[N_neighborhood];
  
  /*****************************************************/
  
  // priors for varying intercept, slope
  Rho ~ lkj_corr( 2 ); //     https://mc-stan.org/docs/2_19/stan-users-guide/multivariate-hierarchical-priors-section.html
  
  Mu_Intercept_coef ~ normal(50000, 50000); 
  Mu_Sqm_coef ~ normal(4000, 1000);
  Mu_CondGoodSqm_coef ~ normal(1000, 1000);
  
  Sigma_Intercept_coef ~ cauchy(0, 7000);
  Sigma_Sqm_coef ~ cauchy(0, 1500);
  Sigma_CondGoodSqm_coef ~ cauchy(0, 350);
  
  // non-centered parameterization for the varying intercept, slopes
  for(k in 1:N_neighborhood) {
    offset[k] ~ std_normal();
    
    VaryingSlopes[k] =  Mu_VaryingSlopes + sigma_Neighborhood .* (Rho_choleskyFactor * offset[k]);  
  }
  
  /*****************************************************/
  
  // priors for group-invariant coefficients
  Age_coef  ~ normal(-2000, 1500);
  TwoRoomsDummy_coef  ~ normal(5000, 5000);
  ThreeRoomsDummy_coef ~ normal(7500, 5000);
  FourRoomsOrMoreDummy_coef ~ normal(7500, 5000);
  SaunaDummy_coef ~ normal(5000, 2500);
  OwnFloor_coef ~ normal(1000, 1000); 
  
  // priors for the variance terms in the likelihood 
  sigma ~ cauchy(0, 15000);
  nu ~ gamma(2, 0.1); 

  // calculation of EVs for the likelihood 
  for ( i in 1:N ) {
    mu[i] = VaryingSlopes[NeighborhoodAssignment[i],1] + VaryingSlopes[NeighborhoodAssignment[i],2]*Sqm[i] + VaryingSlopes[NeighborhoodAssignment[i],3]*CondGoodDummySqm[i] + Age_coef*Age[i] + TwoRoomsDummy_coef*TwoRoomsDummy[i] + ThreeRoomsDummy_coef*ThreeRoomsDummy[i] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[i] + SaunaDummy_coef*SaunaDummy[i] + OwnFloor_coef*OwnFloor[i];
  }
  
  // likelihood
  Price ~ student_t(nu, mu, sigma);
}
generated quantities {
  vector[N] log_lik;
  vector[3] VaryingSlopes[N_neighborhood];

  for(k in 1:N_neighborhood) {
    VaryingSlopes[k] =  Mu_VaryingSlopes + sigma_Neighborhood .* (Rho_choleskyFactor * offset[k]);  
  }
  
  // for loo-package
  for(k in 1:N) {
    log_lik[k] = student_t_lpdf(Price[k] | nu, VaryingSlopes[NeighborhoodAssignment[k],1] + VaryingSlopes[NeighborhoodAssignment[k],2]*Sqm[k] + VaryingSlopes[NeighborhoodAssignment[k],3]*CondGoodDummySqm[k] + Age_coef*Age[k] + TwoRoomsDummy_coef*TwoRoomsDummy[k] + ThreeRoomsDummy_coef*ThreeRoomsDummy[k] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[k] + SaunaDummy_coef*SaunaDummy[k] + OwnFloor_coef*OwnFloor[k], sigma); 
  }
}




