library(Matrix)
library(ggplot2)
library(MASS)
library(swdpwr)
library(swCRTdesign)

############################### 2*3 design #####################################
#### Classic SW ####
mu1 = 0.1
mu2 = 0.2
sigma2 = ((mu1+mu2)/2)*(1-(mu1+mu2)/2) # use pooled variance
K = 4
icc = 0.01
tau2 = icc*sigma2/(1-icc)
t = 3
n = 50
X = rbind(matrix(c(0,0,1,0,1,1), ncol = 3, nrow = 2), 
          matrix(c(0,0,1,0,1,1), ncol = 3, nrow = 2))
U = sum(X)
W1 = apply(X, 2, sum)
W = sum(W1^2)
V1 = apply(X, 1, sum)
V = sum(V1^2)

## calculate variance by closed formula
Var_theta = K*sigma2*(sigma2+t*n*tau2)/((K*U-W)*n*sigma2 + (U^2+K*t*U-t*W-K*V)*n^2*tau2)

## calculate variance by matrix inversion
V0 = tau2*matrix(1,nrow = 3, ncol = 3) + diag(sigma2/n,3)
VV = kronecker(diag(1,4), V0)
Z = cbind(matrix(rep(diag(1,3),4),ncol=3,byrow=TRUE), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta2 = Sigma[t+1,t+1] ## identical to Var_theta

## calculate power
alpha = 0.05
power = pnorm((mu2-mu1)/sqrt(Var_theta) - qnorm(1-alpha/2))

## check using swCRTdesign and swdpower
swPwr(swDsn(c(2,2)), distn="binomial", 
      n=50, mu0=0.1, mu1=0.2, icc = 0.01, 
      alpha=0.05)

batchdesign <- matrix(c(c(0,1,1),c(0,0,1)),4,3,byrow=TRUE)
swdpower(K = 50, design = batchdesign, 
         family = "binomial", model = "marginal",
         link = "identity", type = "cross-sectional",
         meanresponse_start = 0.1,
         meanresponse_end0 = 0.1001, 
         meanresponse_end1 = 0.2,
         typeIerror = 0.05, alpha0 = 0.01, alpha1 = 0.01)

#### 1-unit delay ####
Z1 = cbind(matrix(rep(diag(1,3),2),ncol=3,byrow=TRUE), 0)
Z2 = cbind(0, matrix(rep(diag(1,3),2),ncol=3,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta3 = Sigma[t+2,t+2]
power2 = pnorm((mu2-mu1)/sqrt(Var_theta3) - qnorm(1-alpha/2))

#### 2-unit delay ####
Z1 = cbind(matrix(rep(diag(1,3),2),ncol=3,byrow=TRUE), 0, 0)
Z2 = cbind(0, 0, matrix(rep(diag(1,3),2),ncol=3,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta4 = Sigma[t+3,t+3]
power3 = pnorm((mu2-mu1)/sqrt(Var_theta4) - qnorm(1-alpha/2))

power_matrix <- list(c(power,power2,power3))

############################### 3*4 design #####################################
#### Classic SW ####
K = 6
icc = 0.01
t = 4
n = 50
X = rbind(matrix(c(0,0,0,1,0,0,1,1,0,1,1,1), ncol = 4, nrow = 3), 
          matrix(c(0,0,0,1,0,0,1,1,0,1,1,1), ncol = 4, nrow = 3))
U = sum(X)
W1 = apply(X, 2, sum)
W = sum(W1^2)
V1 = apply(X, 1, sum)
V = sum(V1^2)

## calculate by closed formula
Var_theta = K*sigma2*(sigma2+t*n*tau2)/((K*U-W)*n*sigma2 + (U^2+K*t*U-t*W-K*V)*n^2*tau2)

alpha = 0.05
power = pnorm((mu2-mu1)/sqrt(Var_theta) - qnorm(1-alpha/2))

#### 1-unit delay ####
V0 = tau2*matrix(1,nrow = t, ncol = t) + diag(sigma2/n,t)
VV = kronecker(diag(1,K), V0)

Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0)
Z2 = cbind(0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta3 = Sigma[t+2,t+2]
power2 = pnorm((mu2-mu1)/sqrt(Var_theta3) - qnorm(1-alpha/2))

#### 2-unit delay ####
Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0, 0)
Z2 = cbind(0, 0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta4 = Sigma[t+3,t+3]
power3 = pnorm((mu2-mu1)/sqrt(Var_theta4) - qnorm(1-alpha/2))

#### 3-unit delay ####
Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0, 0, 0)
Z2 = cbind(0, 0, 0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta4 = Sigma[t+4,t+4]
power4 = pnorm((mu2-mu1)/sqrt(Var_theta4) - qnorm(1-alpha/2))

power_matrix <- c(power_matrix, list(c(power,power2,power3, power4)))

############################### 3*4 design #####################################
#### Classic SW ####
K = 4*2
icc = 0.01
t = 5
n = 50
X = rbind(matrix(c(0,0,0,0,1,0,0,0,1,1,0,0,1,1,1,0,1,1,1,1), ncol = 5, nrow = 4), 
          matrix(c(0,0,0,0,1,0,0,0,1,1,0,0,1,1,1,0,1,1,1,1), ncol = 5, nrow = 4))
U = sum(X)
W1 = apply(X, 2, sum)
W = sum(W1^2)
V1 = apply(X, 1, sum)
V = sum(V1^2)

## calculate by closed formula
Var_theta = K*sigma2*(sigma2+t*n*tau2)/((K*U-W)*n*sigma2 + (U^2+K*t*U-t*W-K*V)*n^2*tau2)

alpha = 0.05
power = pnorm((mu2-mu1)/sqrt(Var_theta) - qnorm(1-alpha/2))

#### 1-unit delay ####
V0 = tau2*matrix(1,nrow = t, ncol = t) + diag(sigma2/n,t)
VV = kronecker(diag(1,K), V0)

Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0)
Z2 = cbind(0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta3 = Sigma[t+2,t+2]
power2 = pnorm((mu2-mu1)/sqrt(Var_theta3) - qnorm(1-alpha/2))

#### 2-unit delay ####
Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0, 0)
Z2 = cbind(0, 0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta4 = Sigma[t+3,t+3]
power3 = pnorm((mu2-mu1)/sqrt(Var_theta4) - qnorm(1-alpha/2))

#### 3-unit delay ####
Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0, 0, 0)
Z2 = cbind(0, 0, 0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta5 = Sigma[t+4,t+4]
power4 = pnorm((mu2-mu1)/sqrt(Var_theta5) - qnorm(1-alpha/2))

#### 4-unit delay ####
Z1 = cbind(matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE), 0, 0, 0, 0)
Z2 = cbind(0, 0, 0, 0, matrix(rep(diag(1,t),(t-1)),ncol=t,byrow=TRUE))
Z = cbind(rbind(Z1, Z2), as.vector(t(X)))
Sigma = ginv(t(Z) %*% ginv(VV) %*% Z)
Var_theta6 = Sigma[t+5,t+5]
power5 = pnorm((mu2-mu1)/sqrt(Var_theta6) - qnorm(1-alpha/2))

power_matrix <- c(power_matrix, list(c(power,power2,power3, power4, power5)))
saveRDS(power_matrix, "powerStudy.rds")

# Create a data frame
df <- data.frame(
  x = unlist(lapply(power_matrix, seq_along)),
  y = unlist(power_matrix),
  group = rep(c("2*3 design", "3*4 design", "4*5 design"), sapply(power_matrix, length))
)

# Plot using ggplot
ggplot(df, aes(x = x - 1, y = y, color = factor(group), linetype = factor(group))) +
  geom_line() +
  geom_point() +
  labs(x = "Delayed starting time", 
       y = "Power", 
       color = "Design for single batch", 
       linetype = "Design for single batch",
       title = "Power study for different designs with delayed starting time") +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +  # Set specific line types
  theme_classic()


