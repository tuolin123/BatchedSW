library(swCRTdesign)
library(car)
library(MASS)
library(swdpwr)

#############################    Scenario 1      ###############################  
# swCRTdesign
swPwr(swDsn(c(1,1)), distn="gaussian",
      n=18, mu0=1, mu1=1.5, sigma=1, tau=sqrt(1/99), 
      eta=0, rho=0, gamma=0, alpha=0.05)

completedesign <- matrix(c(c(0,1,1),c(0,0,1)),2,3,byrow=TRUE)

# swdpower with time effect
swdpower(K = 18, design = completedesign,
         family = "gaussian", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 1,
         meanresponse_end0 = 1.5,
         effectsize_beta = 0.5,
         sigma = 1,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

# swdpower without time effect
swdpower(K = 18, design = completedesign,
         family = "gaussian", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 1,
         meanresponse_end1 = 1.5,
         sigma = 1,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

# PASS 0.294
# R Shiny 0.293

#############################    Scenario 2      ###############################
swPwr(swDsn(c(1,1)), distn="binomial", 
      n=50, mu0=0.1, mu1=0.3, icc = 0.01, 
      alpha=0.05)

completedesign <- matrix(c(c(0,1,1),c(0,0,1)),2,3,byrow=TRUE)

# swdpower with time effect (For binary outcome, we can only do with time effect with small meanresponse_end0)
swdpower(K = 50, design = completedesign,
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.1,
         meanresponse_end0 = 0.1001, # we can only specify a very small incremental of 0.1 to start for including time effect
         meanresponse_end1 = 0.3,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)


# swdpower without time effect
swdpower(K = 50, design = completedesign,
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.1,
         meanresponse_end0 = 0.1,
         meanresponse_end1 = 0.3,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

## PASS 0.64
## R shiny 0.64

#############################    Scenario 3      ###############################
swPwr(swDsn(c(5,5,5)), distn="binomial",
      n=50, mu0=0.05, mu1=0.1, icc = 0.01, alpha=0.05)

completedesign <- matrix(c(rep(c(0,1,1,1),5),rep(c(0,0,1,1),5),rep(c(0,0,0,1),5)),15,4,byrow=TRUE)

# swdpower with time effect
swdpower(K = 50, design = completedesign,
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.05,
         meanresponse_end0 = 0.0501, 
         effectsize_beta = 0.05,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

# swdpower without time effect
swdpower(K = 50, design = completedesign,
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.05,
         meanresponse_end1 = 0.1,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

## PASS 0.85
## R shiny 0.86

#############################    Scenario 4      ###############################
## Batched design (separate time model)
swPwr(swDsn(c(2,2)), distn="binomial", 
      n=50, mu0=0.1, mu1=0.3, icc = 0.01, 
      alpha=0.05)

batchdesign <- matrix(c(c(0,1,1),c(0,0,1)),4,3,byrow=TRUE)


# swdpower with time effect (For binary outcome, we can only do with time effect with small meanresponse_end0)
swdpower(K = 50, design = batchdesign,
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.1,
         meanresponse_end0 = 0.1001, # we can only specify a very close number to start for including time effect
         meanresponse_end1 = 0.3,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

swdpower(K = 50, design = batchdesign,
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.1,
         meanresponse_end1 = 0.3,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

## PASS 0.904
## R shiny 0.907
