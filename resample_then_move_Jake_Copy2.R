#-----------------------------------------------------------------------------------------------#
# Script Name: resample_the_move                                                                #
# Author: Hayden Moffat                                                                         #
# email: hayden.moffat@hdr.qut.edu.au                                                           #
#                                                                                               #
# This R script calls the scripts/functions for the resampling and move steps involved in SMC.  #                                                           
#                                                                                               #                                                                                           
#-----------------------------------------------------------------------------------------------#

#This is not the original and pulls from new move step file so that two steps are used

# Resample and move step
theta[,,M] <- residual_resampling(W[,M], N, theta[,,M]) 
source('move_step_Jake_Copy2.R') #JB: uses my copy

# Determine the number of unique particles
n_unique = theta[,,M] %>% as.data.frame %>% as.tbl %>% distinct %>% nrow # d
print(paste("Number of unique particles after resampling:", n_unique))

# Reset weights and ESS
W[,M] = 1/N
ESS[M] = N