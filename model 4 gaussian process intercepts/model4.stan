
/*
  data{
    int<lower=1> N;
    int<lower=1> N_society;
    int total_tools[N];
    real logpop[N];
    int society[N];
    matrix[N_society,N_society] Dmat;
  }
parameters{
  vector[N_society] g;
  real a;
  real bp;
  real<lower=0> etasq;
  real<lower=0> rhosq;
}
model{
  matrix[N_society,N_society] SIGMA_Dmat;
  vector[N] lambda;
  rhosq ~ cauchy( 0 , 1 );
  etasq ~ cauchy( 0 , 1 );
  bp ~ normal( 0 , 1 );
  a ~ normal( 0 , 10 );
  for ( i in 1:(N_society-1) )
    for ( j in (i+1):N_society ) {
      SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
      SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
    }
  for ( k in 1:N_society )
    SIGMA_Dmat[k,k] = etasq + 0.01;
  g ~ multi_normal( rep_vector(0,N_society) , SIGMA_Dmat );
  for ( i in 1:N ) {
    lambda[i] = a + g[society[i]] + bp * logpop[i];
  }
  total_tools ~ poisson_log( lambda );
}
generated quantities{
  matrix[N_society,N_society] SIGMA_Dmat;
  vector[N] lambda;
  real dev;
  dev = 0;
  for ( i in 1:N ) {
    lambda[i] = a + g[society[i]] + bp * logpop[i];
  }
  dev = dev + (-2)*poisson_log_lpmf( total_tools | lambda );
}
*/   


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
  
  real<lower=0> etasq;
  real<lower=0> rhosq;
  real<lower=0> sigmaInterceptDiag; 
  vector[N_neighborhood] intercepts;
}

// priors have to be checked
model{
  matrix[N_neighborhood, N_neighborhood] SIGMA_Dmat;
  vector[N] mu;
  
  // GP covariance matrix generation
  rhosq ~ cauchy( 0 , 1 ); /* */
  etasq ~ cauchy( 0 , pow(10000,2) ); 
  for ( i in 1:(N_neighborhood-1) ) {
    for ( j in (i+1):N_neighborhood ) {
      SIGMA_Dmat[i,j] = etasq*exp(-rhosq*pow(Dmat[i,j],2));
      SIGMA_Dmat[j,i] = SIGMA_Dmat[i,j];
    }
  }
    
  sigmaInterceptDiag ~ cauchy(50000, 100000); 
  
  for ( k in 1:N_neighborhood ) {
    SIGMA_Dmat[k,k] = etasq + sigmaInterceptDiag;
  }
  
  // intercepts with EV 50000, covariance matrix dependend on the distances 
  intercepts ~ multi_normal( rep_vector( 70000, N_neighborhood) , SIGMA_Dmat );
  
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
  
  sigma ~ cauchy(1000, 10000);
  
  // likelihood
  Price ~ normal(mu, sigma);
}

