
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
  
  matrix[N_neighborhood, N_neighborhood] Dmat;
}
parameters{
  real Sqm_coef;
  real CondGoodDummySqm_coef; 
  real Age_coef;
  real TwoRoomsDummy_coef;
  real ThreeRoomsDummy_coef;
  real FourRoomsOrMoreDummy_coef;
  real SaunaDummy_coef;
  real OwnFloor_coef; 
  
  real<lower=0> sigma; 
  real<lower=0> nu; 
  
  real<lower=0> etasq;
  real<lower=0> rhosq;
  //real<lower=0> sigmaInterceptDiag; 
  vector[N_neighborhood] offset;
 
}
// priors have to be checked?
model{
  matrix[N_neighborhood, N_neighborhood] SIGMA_Dmat;
  matrix[N_neighborhood, N_neighborhood] SIGMA_Dmat_CholFactor; 
  vector[N_neighborhood] interceptsUnscaled;
  vector[N_neighborhood] intercepts;
  
  vector[N] mu;
  
  
  // GP covariance matrix generation
  rhosq ~ cauchy( 0 , 1 ); /* */
  etasq ~ cauchy( 0 , 300 ); 
  for ( i in 1:(N_neighborhood-1) ) {
    for ( j in (i+1):N_neighborhood ) {
      SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
      SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
    }
  }
    
  //sigmaInterceptDiag ~ cauchy(50000, 100000); 
  for ( k in 1:N_neighborhood ) {
    //SIGMA_Dmat[k,k] = etasq + sigmaInterceptDiag;
    SIGMA_Dmat[k,k] = etasq + 0.01;
  }
  
  
  // for non-centered parameterization 
  SIGMA_Dmat_CholFactor = cholesky_decompose(SIGMA_Dmat);
  offset ~ std_normal(); 
  
  // intercepts with EV 70000, covariance matrix dependend on the distances - non-centered parameterization 
  interceptsUnscaled = rep_vector( 70, N_neighborhood) + SIGMA_Dmat_CholFactor * offset;
  intercepts = 1000*interceptsUnscaled; 

  // priors
  Sqm_coef  ~ normal(6000, 3000);
  CondGoodDummySqm_coef ~ normal(1000, 1500);
  Age_coef  ~ normal(-2000, 2500);
  TwoRoomsDummy_coef  ~ normal(5000, 10000);
  ThreeRoomsDummy_coef ~ normal(7500, 10000);
  FourRoomsOrMoreDummy_coef ~ normal(7500, 10000);
  SaunaDummy_coef ~ normal(5000, 2500);
  OwnFloor_coef ~ normal(1000, 1000); 
  
  // "general intercept"
  //betaGeneralIntercept ~ normal(0, 10000); 

  // betaGeneralIntercept + 
  for ( i in 1:N ) {
    mu[i] = intercepts[NeighborhoodAssignment[i]] + Sqm_coef*Sqm[i] + CondGoodDummySqm_coef*CondGoodDummySqm[i] + Age_coef*Age[i] + TwoRoomsDummy_coef*TwoRoomsDummy[i] + ThreeRoomsDummy_coef*ThreeRoomsDummy[i] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[i] + SaunaDummy_coef*SaunaDummy[i] + OwnFloor_coef*OwnFloor[i];
  }
  
  //sigma ~ cauchy(5000, 10000);
  sigma ~ cauchy(0, 15000);
  nu ~ gamma(2, 0.1); 
  
  // likelihood
  Price ~ student_t(nu, mu, sigma); 
}
generated quantities {
  matrix[N_neighborhood, N_neighborhood] SIGMA_Dmat;
  matrix[N_neighborhood, N_neighborhood] SIGMA_Dmat_CholFactor; 
  vector[N_neighborhood] interceptsUnscaled;
  vector[N_neighborhood] intercepts;
  
  vector[N] log_lik;
  
  for ( i in 1:(N_neighborhood-1) ) {
    for ( j in (i+1):N_neighborhood ) {
      SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
      SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
    }
  }
    
  for ( k in 1:N_neighborhood ) {
    //SIGMA_Dmat[k,k] = etasq + sigmaInterceptDiag;
    SIGMA_Dmat[k,k] = etasq + 0.01;
  }
  
  // for non-centered parameterization 
  SIGMA_Dmat_CholFactor = cholesky_decompose(SIGMA_Dmat);
  
  // intercepts with EV 70000, covariance matrix dependend on the distances - non-centered parameterization 
  interceptsUnscaled = rep_vector( 70, N_neighborhood) + SIGMA_Dmat_CholFactor * offset;
  intercepts = 1000*interceptsUnscaled; 

  // for loo-package
  for(k in 1:N) {
    log_lik[k] = student_t_lpdf(Price[k] | nu, intercepts[NeighborhoodAssignment[k]] + Sqm_coef*Sqm[k] + CondGoodDummySqm_coef*CondGoodDummySqm[k] + Age_coef*Age[k] + TwoRoomsDummy_coef*TwoRoomsDummy[k] + ThreeRoomsDummy_coef*ThreeRoomsDummy[k] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[k] + OwnFloor_coef*OwnFloor[k] + SaunaDummy_coef*SaunaDummy[k], sigma); 
  }
}
