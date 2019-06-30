

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
  
  // "realized", group varying coefficients for each neighborhood
  vector[3] v_VaryingSlopes[N_neighborhood];
}

transformed parameters{
  vector<lower=0>[3] sigma_Neighborhood;
  vector[3] Mu_VaryingSlopes;
  cov_matrix[3] SRS_sigma;
  
  /* EV for slopes */ 
  Mu_VaryingSlopes[1] = Mu_Intercept_coef;
  Mu_VaryingSlopes[2] = Mu_Sqm_coef; 
  Mu_VaryingSlopes[3] = Mu_CondGoodSqm_coef; 
  
  
  /* Sigma for slopes */ 
  sigma_Neighborhood[1] = Sigma_Intercept_coef;
  sigma_Neighborhood[2] = Sigma_Sqm_coef;
  sigma_Neighborhood[3] = Sigma_CondGoodSqm_coef; 
  
  SRS_sigma = quad_form_diag(Rho, sigma_Neighborhood);
}
model{
  vector[N] mu;
  
  /*
    https://mc-stan.org/docs/2_19/stan-users-guide/multivariate-hierarchical-priors-section.html
    
    The basic behavior of the LKJ correlation distribution is similar to that of a beta distribution. For η=1, the result is a uniform distribution. Despite being the identity over correlation matrices, the marginal distribution over the entries in that matrix (i.e., the correlations) is not uniform between -1 and 1. Rather, it concentrates around zero as the dimensionality increases due to the complex constraints. 
    
    For η>1, the density increasingly concentrates mass around the unit matrix, i.e., favoring less correlation. For η<1, it increasingly concentrates mass in the other direction, i.e., favoring more correlation.

The LKJ prior may thus be used to control the expected amount of correlation among the parameters βj. For a discussion of decomposing a covariance prior into a prior on correlation matrices and an independent prior on scales, see Barnard, McCulloch, and Meng (2000).
    
  */
  Mu_Intercept_coef ~ normal(50000, 50000); 
  Mu_Sqm_coef ~ normal(4000, 1000);
  Mu_CondGoodSqm_coef ~ normal(1000, 1000);
  
  
  Rho ~ lkj_corr( 2 );
  
  Sigma_Intercept_coef ~ cauchy(0, 7000);
  Sigma_Sqm_coef ~ cauchy(0, 1500);
  Sigma_CondGoodSqm_coef ~ cauchy(0, 350);
  
  
  // Mu_Varying  compiled from Mu_Intercept_coef, Mu_Sqm_coef, Mu_CondGoodSqm_coef in transformed parameters-block
  // SRS_sigma compiled from Sigma_Intercept_coef, Sigma_Sqm_coef, Sigma_CondGoodSqm_coef, Rho in transformed parameters-block

  v_VaryingSlopes ~ multi_normal( Mu_VaryingSlopes , SRS_sigma);
  
  for ( i in 1:N ) {
    mu[i] = v_VaryingSlopes[NeighborhoodAssignment[i],1] + v_VaryingSlopes[NeighborhoodAssignment[i],2]*Sqm[i] + v_VaryingSlopes[NeighborhoodAssignment[i],3]*CondGoodDummySqm[i] + Age_coef*Age[i] + TwoRoomsDummy_coef*TwoRoomsDummy[i] + ThreeRoomsDummy_coef*ThreeRoomsDummy[i] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[i] + SaunaDummy_coef*SaunaDummy[i] + OwnFloor_coef*OwnFloor[i];
  }
  
  sigma ~ cauchy(0, 15000);
  nu ~ gamma(2, 0.1); 

  // likelihood
  Price ~ student_t(nu, mu, sigma); 
}
