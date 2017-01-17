################################################################################
###
###
###   Supreme Court bargaining - Estimation 
###   Authors: Brad Smith
###
###
###   Purpose: Estimates recognition probabilities for SC bargaining project
###
################################################################################

rm(list=ls())

library(rstan)
library(foreign)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


setwd("~/Google Drive/Research/Supreme Court Bargaining")
data <- load("Data/stan_ss_data.RData")


N <- nrow(d)
k <- max(id)

stan.data <- list(d  = d,   #justice ideal points (Nx9)
                  id = id,  #justice identifiers 
                  o  = o,   #opinions
                  N  = N,   #number of obs
                  k = k     #number of unique justices
)

scstan <- '
functions{
vector eqoffer(vector x, real q, vector r){
    vector[num_elements(x)] offer;
    real pU;
    real pU2;
    real delta;
    real condition;
    vector[9] B; //analog of expanded pU object
    vector[9] C; //used for intermediate calculations
    vector[9] D; //analog of VmedIter object
    vector[9] E; //used for intermediate calculations
    vector[9] L; //logical
    vector[9] LL; //logical 2
    vector[9] UB; //upper bound
    vector[9] LB; // lower bound 
    vector[9] EO; // equilibrium offer
    delta = 0.9;
    condition = 1;
    // Calculate median justice utility for each other justice ideal policy
      for(i in 1:9)
      B[i] = -(x[5] - x[i])^2;
      for(j in 1:9)
      C[j] = dot_product(B,r);
      C = C * delta;
      for(i in 1:9)
      D[i] = C[i] - (1-delta)*(x[5]-q)^2;
      for(i in 1:9)
      L[i] = B[i]>=D[i];

      while(condition != 0){
          for(i in 1:9)
            E[i] = L[i] * B[i] ;
          pU2 = dot_product(E,r);
          for(i in 1:9)
            E[i] = (delta*pU2)-(1-delta)*((x[5]-q)^2)/(1-delta*dot_product((1-L), r));
        for(i in 1:9)
        LL[i] = B[i] >= E[i];
        
        condition <- sum(1*L-1*LL);
        for(i in 1:9)
         L[i] = B[i]>=E[i];
      }
      
      //upper and lower bound of win set
      for(i in 1:9)
      UB[i] = x[5] + sqrt(-E[i]);
      for(i in 1:9)
      LB[i] = x[5] - sqrt(-E[i]);
      
      //back out proposals, comparing locations to win set
      for(i in 1:9)
      EO[i] = L[i]*x[i] + (1-L[i])*((x[i]<LB[i])*LB[i]+(x[i]>UB[i])*UB[i]);
      
return(EO);
} 
}
data{
int<lower=1> N;   //number of cases
vector[9] d[N];    //justice ideal points
int id[N,9];      //justice identifiers
vector[N] o;    //opinions
int<lower=1> k ;  //unique justices
}
parameters{
real rho[k];    //vector of influence parameters
real quo[N];    //vector of status quo means
}
model{

real ps[9];  //temp for log component densities
vector[9] off; //temp for eq offers 
vector[9] R;  //normalized eq offers step 1
vector[9] r;  //normalized eq offers step 2
rho ~ normal(0,1);
quo ~ normal(0,3);

for(i in 1:N){
  for(j in 1:9){
      R[j] = rho[id[i,j]]; //pull out correct elements of rho 
  }
  for(j in 1:9){
      r[j] = exp(R[j])/sum(exp(R)); //softmax function to normalize
  }

     off = eqoffer(d[i], quo[i], r); //define offers 

     for(j in 1:9){
          ps[j] = log(r[j]) + normal_lpdf(o[i] | off[j], 1) + normal_lpdf(quo[i] | 0, 3);
     }
  target += log_sum_exp(ps);

}
}
'

iter <- 10
fit <- stan(model_code = scstan,
            data = stan.data,
            iter = iter,
            chains = 1,
            warmup = floor(iter/3))
