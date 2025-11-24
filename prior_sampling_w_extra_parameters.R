#-----------------------------------------------------------------------------------------------#
# Script Name: prior_sampling                                                                   #
# Author: Hayden Moffat                                                                         #
# email: hayden.moffat@hdr.qut.edu.au                                                           #
#                                                                                               #
# This R script samples N particles from the prior distributions of each model specified.         #
# Here we consider a log normal prior on each of the parameters where the mean and standard     #
# deviation of the natural logarithm of the variables are -1.4 and 1.35, respectively.          #                           
#                                                                                               #                                                                                            #
#-----------------------------------------------------------------------------------------------#
### THIS IS NOT THE ORIGINAL FILE, EDITS HAVE BEEN MADE ###

par_dimension <- 5 # MK: edit to number of parameters needed
theta <- array(0, dim = c(N, par_dimension, K)) # initialise array
prior_mean <- -1.4
prior_std <- 1.35

for (p in 1:K){
  
  if (models[p] %in% c(1,2)){
    theta[,,p] <- rnorm(N*3, prior_mean, prior_std) %>% matrix(N, 3)
  } 
  
  else if (models[p] %in% c(3,4)){
    theta[,,p] <- cbind(rnorm(N, prior_mean, prior_std), rnorm(N, prior_mean, prior_std), rep(0, N))
  }

}
