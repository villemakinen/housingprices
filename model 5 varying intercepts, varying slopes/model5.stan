

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
  real Age_coef;
  real TwoRoomsDummy_coef;
  real ThreeRoomsDummy_coef;
  real FourRoomsOrMoreDummy_coef;
  real SaunaDummy_coef;
  real OwnFloor_coef; 
  
  // variance paramters
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
  
}

transformed parameters{
  vector[N_neighborhood] Intercept_coef;
  vector[N_neighborhood] Sqm_coef;
  vector[N_neighborhood] CondGoodSqm_coef;
  
  
  vector[3] v_VaryingSlopes[N_neighborhood];
  vector<lower=0>[3] sigma_Neighborhood;
  vector[3] Mu_VaryingSlopes;
  
  cov_matrix[3] SRS_sigma;
  
  /* Variances fro  */
  for ( j in 1:N_neighborhood) {
    v_VaryingSlopes[j,1] = Intercept_coef[j];
    v_VaryingSlopes[j,2] = Sqm_coef[j];
    v_VaryingSlopes[j,3] = CondGoodSqm_coef[j];
  }
  
  sigma_Neighborhood[1] = Sigma_Intercept_coef;
  sigma_Neighborhood[2] = Sigma_Sqm_coef;
  sigma_Neighborhood[3] = Sigma_CondGoodSqm_coef; 
  
  SRS_sigma = quad_form_diag(Rho, sigma_Neighborhood);
}
model{
  vector[N] mu;
  
  Mu_Intercept_coef ~ normal(50000, 50000); 
  Mu_Sqm_coef ~ normal(4000, 1000);
  Mu_CondGoodSqm_coef ~ normal(1000, 1000);
  
  Sigma_Intercept_coef ~ cauchy(7000, 1000);
  Sigma_Sqm_coef ~ cauchy(1000, 500);
  Sigma_CondGoodSqm_coef ~ cauchy(200,150);

  // how to parametrize?
  Rho ~ lkj_corr( 2 );
  
  sigma ~ cauchy(10000, 5000);
  nu ~ gamma(2, 0.1); 

  v_VaryingSlopes ~ multi_normal( Mu_VaryingSlopes , SRS_sigma);
  
  for ( i in 1:N ) {
    //mu[i] = a_cafe[cafe[i]] + b_cafe[cafe[i]] * afternoon[i];
    mu[i] = Intercept_coef[NeighborhoodAssignment[i]] + Sqm_coef[NeighborhoodAssignment[i]]*Sqm[i] + CondGoodSqm_coef[NeighborhoodAssignment[i]]*CondGoodDummySqm[i] + Age_coef*Age[i] + TwoRoomsDummy_coef*TwoRoomsDummy[i] + ThreeRoomsDummy_coef*ThreeRoomsDummy[i] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[i] + SaunaDummy_coef*SaunaDummy[i] + OwnFloor_coef*OwnFloor[i];
  }
  
  // likelihood
  Price ~ student_t(nu, mu, sigma); 
}

