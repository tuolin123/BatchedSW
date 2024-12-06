library(swdpwr)

####################    2*3 design with 10 batches      ########################
n_batch = 10
onebatch <- matrix(c(c(0,1,1), c(0,0,1)), nrow = 2, byrow = TRUE)
design_mat <- t(matrix(rep(t(onebatch),n_batch), ncol = n_batch*2))
K <- 20                   
alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.1
mean_end0 <- 0.1001
mean_end1 <- 0.2
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
               family = "binomial", model = "marginal",
               link = "logit", type = "cross-sectional",
               meanresponse_start = mean_start,
               meanresponse_end0 = mean_end0,
               meanresponse_end1 = mean_end1,
               typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.15
mean_end0 <- 0.1501
mean_end1 <- 0.25
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.2
mean_end0 <- 0.2001
mean_end1 <- 0.3
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

####################    3*4 design with 5 batches      ########################
n_batch = 5
onebatch <- matrix(c(c(0,1,1,1), c(0,0,1,1), c(0,0,0,1)), nrow = 3, byrow = TRUE)
design_mat <- t(matrix(rep(t(onebatch),n_batch), ncol = n_batch*3))
K <- 20                   
alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.1
mean_end0 <- 0.1001
mean_end1 <- 0.2
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.15
mean_end0 <- 0.1501
mean_end1 <- 0.25
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.2
mean_end0 <- 0.2001
mean_end1 <- 0.3
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

####################    4*5 design with 3 batches      ########################
n_batch = 3
onebatch <- matrix(c(c(0,1,1,1,1), c(0,0,1,1,1), c(0,0,0,1,1), c(0,0,0,0,1)), 
                   nrow = 4, byrow = TRUE)
design_mat <- t(matrix(rep(t(onebatch),n_batch), ncol = n_batch*4))
K <- 20                   
alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.1
mean_end0 <- 0.1001
mean_end1 <- 0.2
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.15
mean_end0 <- 0.1501
mean_end1 <- 0.25
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.2
mean_end0 <- 0.2001
mean_end1 <- 0.3
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

####################    5*6 design with 2 batches      ########################
n_batch = 2
onebatch <- matrix(c(c(0,1,1,1,1,1), c(0,0,1,1,1,1), c(0,0,0,1,1,1), c(0,0,0,0,1,1),
                     c(0,0,0,0,0,1)), nrow = 5, byrow = TRUE)
design_mat <- t(matrix(rep(t(onebatch),n_batch), ncol = n_batch*5))
K <- 20                   
alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.1
mean_end0 <- 0.1001
mean_end1 <- 0.2
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.15
mean_end0 <- 0.1501
mean_end1 <- 0.25
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.01
alpha1 <- 0.01
mean_start <- 0.2
mean_end0 <- 0.2001
mean_end1 <- 0.3
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

alpha0 <- 0.1
alpha1 <- 0.1
swdpower(K = K, design = design_mat,
         family = "binomial", model = "marginal",
         link = "logit", type = "cross-sectional",
         meanresponse_start = mean_start,
         meanresponse_end0 = mean_end0,
         meanresponse_end1 = mean_end1,
         typeIerror = 0.05, alpha0 = alpha0, alpha1 = alpha1)

