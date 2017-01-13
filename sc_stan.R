################################################################################
###
###
###   Stan script for 
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

data <- read.csv("Data/AllIdealPoints.csv")

# For now, eliminate all observations without 9 justices 
data <- data[complete.cases(data),]

# Create a series of objects to pass to Stan
X <- as.matrix(data[,c("J1", "J2", "J3", "J4",           # Ideal points
                       "J5", "J6", "J7", "J8", "J9")])

O <- as.matrix(data[,c("Opinion")]) # Opinion

N <- ncol(X)

stan.data <- list(X = X, #justice ideal points (Nx9)
                  O = O, #opinions
                  N = N
                  #number of obs
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
    delta <- 0.9;
    condition <- 1;
    // Calculate median justice utility for each other justice ideal policy
      for(i in 1:9)
      B[i] <- -(x[5] - x[i])^2;
      for(j in 1:9)
      C[j] <- dot_product(B,r);
      C <- C * delta;
      for(i in 1:9)
      D[i] <- C[i] - (1-delta)*(x[5]-q)^2;
      for(i in 1:9)
      L[i] <- B[i]>=D[i];

      while(condition != 0){
          for(i in 1:9)
            E[i] <- L[i] * B[i] ;
          pU2 <- dot_product(E,r);
          for(i in 1:9)
            E[i] <- (delta*pU2)-(1-delta)*((x[5]-q)^2)/(1-delta*dot_product((1-L), r));
        for(i in 1:9)
        LL[i] <- B[i] >= E[i];
        
        condition <- sum(1*L-1*LL);
        for(i in 1:9)
         L[i] <- B[i]>=E[i];
      }
      
      for(i in 1:9)
      UB[i] <- x[5] + sqrt(-E[i]);
      for(i in 1:9)
      LB[i] <- x[5] - sqrt(-E[i]);
      
      for(i in 1:9)
      EO[i] <- L[i]*x[i] + (1-L[i])*((x[i]<LB[i])*LB[i]+(x[i]>UB[i])*UB[i]);
      
return(EO);
} 
}
data{
int<lower=1> N; //number of cases
matrix[N,9] X;  //justice ideal points
matrix[N,1] O;  //opinions 
}
parameters{
vector[9] rho;    //vector of influence parameters
}
model{}
'

expose_stan_functions(stanc(model_code = scstan))
x <- c(-4:4)
q <- 1
r <- c(.1, .1, .1, .1, .1, .2, .1, .1, .1)
eqoffer(x, q, r)

