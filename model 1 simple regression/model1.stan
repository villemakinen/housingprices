


data {
  int<lower=0> N;
  vector[N] Price;
  
  vector[N] Sqm;
  vector[N] CondGoodDummySqm;
  vector[N] Age;
  vector[N] TwoRoomsDummy;
  vector[N] ThreeRoomsDummy;
  vector[N] FourRoomsOrMoreDummy;
  vector[N] OwnFloor;
  vector[N] SaunaDummy;
}
parameters {
  real Intercept_coef;
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
}
model {
  // EV calculations 
  vector[N] mu;
  
  for ( i in 1:N ) {
    mu[i] = Intercept_coef + Sqm_coef*Sqm[i] + CondGoodDummySqm_coef*CondGoodDummySqm[i] + Age_coef*Age[i] + TwoRoomsDummy_coef*TwoRoomsDummy[i] + ThreeRoomsDummy_coef*ThreeRoomsDummy[i] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[i] + OwnFloor_coef*OwnFloor[i] + SaunaDummy_coef*SaunaDummy[i];
  }
  
  // priors
  Intercept_coef ~ normal(70000, 50000);
  Sqm_coef  ~ normal(4500, 1000);
  CondGoodDummySqm_coef ~ normal(1000, 1000);
  Age_coef  ~ normal(-1500, 2000);
  TwoRoomsDummy_coef  ~ normal(5000, 10000);
  ThreeRoomsDummy_coef ~ normal(7500, 10000);
  FourRoomsOrMoreDummy_coef ~ normal(7500, 10000);
  OwnFloor_coef ~ normal(7000, 1000); 
  SaunaDummy_coef ~ normal(5000, 2500);
  
  sigma ~ cauchy(10000, 5000);
  nu ~ gamma(2, 0.1);
  
  // likelihood
  Price ~ student_t(nu, mu, sigma); 
}
generated quantities {
  vector[N] log_lik;
  
  for ( i in 1:N ) {
      log_lik[i] = student_t_lpdf(Price[i] | nu, Intercept_coef + Sqm_coef*Sqm[i] + CondGoodDummySqm_coef*CondGoodDummySqm[i] + Age_coef*Age[i] + TwoRoomsDummy_coef*TwoRoomsDummy[i] + ThreeRoomsDummy_coef*ThreeRoomsDummy[i] + FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy[i] + OwnFloor_coef*OwnFloor[i] + SaunaDummy_coef*SaunaDummy[i], sigma); 
  }
  
}

