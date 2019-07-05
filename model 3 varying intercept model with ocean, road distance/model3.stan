
data {
  int<lower=1> N;
  int<lower=1> N_neighborhood;
  
  vector[N_neighborhood] OceanDistance;
  vector[N_neighborhood] RoadDistance;
  
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
  real Sqm_coef;
  real CondGoodDummySqm_coef; 
  real Age_coef;
  real TwoRoomsDummy_coef;
  real ThreeRoomsDummy_coef;
  real FourRoomsOrMoreDummy_coef;
  real OwnFloor_coef; 
  real SaunaDummy_coef;
  
  real<lower=0> sigma; 
  real<lower=0> nu; 
  
  real Intercept_pop_coef;
  real OceanDistance_pop_coef;
  real RoadDistance_pop_coef; 
  
  real<lower=0> sigma_pop; 
  vector[N_neighborhood] Intercept_offset; 
}
model {
  vector[N] mu;
  vector[N_neighborhood] Intercept_coef;

  // pop distributions
  Intercept_pop_coef ~ normal(150000, 50000); 
  OceanDistance_pop_coef ~ normal(-5, 3); 
  RoadDistance_pop_coef ~ normal(-5, 3); 
  
  //sigma_pop ~ cauchy(10000, 10000); 
  sigma_pop ~ cauchy(0, 20000); 
  
  // intercept draws - non-centered parameterization 
  Intercept_offset ~ normal(0,1); 
  Intercept_coef = Intercept_pop_coef + OceanDistance*OceanDistance_pop_coef + RoadDistance*RoadDistance_pop_coef + Intercept_offset*sigma_pop; 
  
  // priors 
  Sqm_coef ~ normal(5000, 1000);
  CondGoodDummySqm_coef ~ normal(2000, 1000); 
  Age_coef ~ normal(-1000, 1000);
  TwoRoomsDummy_coef ~ normal(5000, 10000);
  ThreeRoomsDummy_coef ~ normal(7500, 10000);
  FourRoomsOrMoreDummy_coef ~ normal(7500, 10000);
  SaunaDummy_coef ~ normal(5000, 2500);
  OwnFloor_coef ~ normal(1000, 1000); 
  
  //sigma ~ cauchy(10000, 5000);
  sigma ~ cauchy(0, 15000);
  nu ~ gamma(2, 0.1); 
  
  // EVs
  for(k in 1:N) {
    mu[k] = Intercept_coef[NeighborhoodAssignment[k]] + Sqm_coef*Sqm[k] + CondGoodDummySqm_coef*CondGoodDummySqm[k] + Age_coef*Age[k] + TwoRoomsDummy_coef*TwoRoomsDummy[k] + ThreeRoomsDummy_coef*ThreeRoomsDummy[k] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[k] + OwnFloor_coef*OwnFloor[k] + SaunaDummy_coef*SaunaDummy[k];
  }
  
  // likelihood
  Price ~ student_t(nu, mu, sigma); 
}
generated quantities {
  vector[N] log_lik;
  vector[N_neighborhood] Intercept_coef;

  // realized intercepts
  Intercept_coef = Intercept_pop_coef + OceanDistance*OceanDistance_pop_coef + RoadDistance*RoadDistance_pop_coef + Intercept_offset*sigma_pop;
  
  // for loo-package
  for(k in 1:N) {
    log_lik[k] = student_t_lpdf(Price[k] | nu, Intercept_coef[NeighborhoodAssignment[k]] + Sqm_coef*Sqm[k] + CondGoodDummySqm_coef*CondGoodDummySqm[k] + Age_coef*Age[k] + TwoRoomsDummy_coef*TwoRoomsDummy[k] + ThreeRoomsDummy_coef*ThreeRoomsDummy[k] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[k] + OwnFloor_coef*OwnFloor[k] + SaunaDummy_coef*SaunaDummy[k], sigma); 
  }
}

