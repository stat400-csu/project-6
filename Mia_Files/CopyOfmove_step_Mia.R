#-----------------------------------------------------------------------------------------------#
# Script Name: move_step                                                                        #
# Author: Hayden Moffat                                                                         #
# email: hayden.moffat@hdr.qut.edu.au                                                           #
#                                                                                               #
# This R script contains code to conduct a MCMC move step on the particles after the resampling #
# step.                                                                                         #                           
#                                                                                               #                                                                                            #
#-----------------------------------------------------------------------------------------------#
# NOT THE ORIGINAL #

print("MOVE STEP")

# If ESS is very low -> use previous covariance matrix
# Otherwise -> compute parameters of MCMC kernel qt, and override previous covariance matrix

if(ESS[M] > tol){
  c <- cov(theta[,,M])
  all_cov_matrices[[M]] <- c
}

cov_matrix <- all_cov_matrices[[M]]

na_find <- which(is.na(theta[,,M]), arr.ind = T)
if (nrow(na_find) > 0){
  cat('Found NAs in theta')
  print(head(na_find, 10))
  
  for (r in unique(na_find[,1])) {
    if (exists('prior_mean') && exists('prior_std')){
      theta[r, , M] <- rnorm(dim(theta)[2], mean = prior_mean, sd = prior_std)
      
    } else{
      theta[r, , M] <- rep(0, dim(theta)[2])
    }
    cat('replaced theta row', r,'\n')
  }
}

# Move each particle theta with MCMC kernel for 1 iteration
n_moves = rep(0, N*K) %>% matrix(nrow = N, ncol = K) # variable that describes the number of particles that have moved after 1 iteration
for (l in 1:N){
  u <- runif(1)
  x <- theta[l,,M] %>% matrix(nrow = 1)
  if (any(!is.finite(x))){
    cat('Skip particle')
    next
  }
  logpri[l,M] = log_prior(x, models[M]) # determine log prior of initial theta values
  loglik[l,M] = loglikelihood_Hollings(exp(x), data_subset, time, models[M]) # determine log likelihood of initial theta values
  if(!is.finite(loglik[l,M])) log_lik[l,M] <- -Inf
  
  px[l,M] = loglik[l,M]+logpri[l,M]

  xs <- rmvnorm(1, x, cov_matrix) # new sample xs based on existing x from proposal pdf.
  xs <- as.numeric(xs)
  if (any(!is.finite(xs)) || any(xs < -50) || any(xs > 50)) {
    next
  }
  
  theta_exp <- exp(xs)
  
  if (any(!is.finite(theta_exp)) || theta_exp[4] < 1e-10 ||theta_exp[4] > 1e6){
    next
  }
  a <- theta_exp[1] * theta_exp[2] * theta_exp[3] # MK
  Th <- theta_exp[4] # MK
  
  if (!is.finite(a) || a < 1e-6 || a > 1e6) next
  if (!is.finite(Th) || Th < 1e-6 || Th > 1e2) next # MK: Ensuring a and Th are not 0, negative, or very large
  # Determine log prior and log likelihood of proposed theta values
  logprior_xs <- log_prior(xs, models[M])
  loglik_xs <- loglikelihood_Hollings(theta_exp, data_subset, time, models[M])
  if(!is.finite(loglik_xs)) loglik_xs <- -Inf
  pxs <- loglik_xs +logprior_xs

  
  if (is.nan(pxs) == 0){
    if (u<min(1,exp(pxs-px[l,M]))){
      theta[l,,M] <- xs
      logpri[l,M] <- logprior_xs
      loglik[l,M] <- loglik_xs
      px[l,M] <- pxs
      n_moves[l,M] = n_moves[l,M]+1
    }
  }
 
}


# Determine acceptance probability and an appropriate number of iterations
# of the MCMC
prob = sum(n_moves[,M])/N
Rt = ceiling(log(0.01)/log(1-prob))

# Displays the acceptance probability and the number of iterations
# (Rt) for each move step
print(paste("Acceptance probability =", prob, ", Rt = ", Rt))

# Move particles the rest of the iterations
for (l in 1:N){
  for (q in 2:Rt){
    u <- runif(1)
    x <- theta[l,,M] %>% matrix(nrow = 1)
    xs <- rmvnorm(1, x, cov_matrix) # new sample xs based on existing x from proposal pdf.
    xs <- as.numeric(xs)
    if (any(!is.finite(xs)) || any(xs < -50) || any(xs > 50)) {
      next
    }
    theta_exp <- exp(xs)
    if (any(!is.finite(theta_exp)) || theta_exp[4] < 1e-10 ||theta_exp[4] > 1e6){
      next
    }
    
    # Determine log prior and log likelihood of proposed theta values
    logprior_xs <- log_prior(xs, models[M])
    loglik_xs <- loglikelihood_Hollings(theta_exp, data_subset, time, models[M]);
    pxs <- loglik_xs +logprior_xs;
    
    if (u<min(1,exp(pxs-px[l,M]))){
      theta[l,,M] <- xs
      logpri[l,M] <- logprior_xs
      loglik[l,M] <- loglik_xs
      px[l,M] <- pxs
      n_moves[l,M] = n_moves[l,M]+1

    }
  }
}
W[,M] = rep(1/N, N) # MK: Reset the weights, remove if unnecessary
ESS[M] = N


