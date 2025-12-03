
# Saving Results to compile into tables


# MK: Grabbing posterior probabilities when R = 1 for each model when Model 1 is set to true.
post_prob_model1_R1 <- posterior_model_prob[1]
post_prob_model2_R1 <- posterior_model_prob[2]
post_prob_model3_R1 <- posterior_model_prob[3]
post_prob_model4_R1 <- posterior_model_prob[4]

# MK: Grabbing log evidence and log d-posterior precisions for R=1 and model 1
log_est_evi_R1_model1 <- log_Z[1]
log_dpost_prec_R1_model1 <- -log(det(cov(exp(theta[,,1]))))

# MK: Grabbing posterior probabilities when R = 0 for each model when Model 1 is set to true.

post_prob_model1_R0 <- posterior_model_prob[1]
post_prob_model2_R0 <- posterior_model_prob[2]
post_prob_model3_R0 <- posterior_model_prob[3]
post_prob_model4_R0 <- posterior_model_prob[4]

# MK: Grabbing log evidence and log d-posterior precisions for R=0 and model 1
log_est_evi_R0_model1 <- log_Z[1]
log_dpost_prec_R0_model1 <- -log(det(cov(exp(theta[,,1]))))

# MK: Grabbing posterior probabilities when R = 6 for each model when Model 1 is set to true.

post_prob_model1_R6 <- posterior_model_prob[1]
post_prob_model2_R6 <- posterior_model_prob[2]
post_prob_model3_R6 <- posterior_model_prob[3]
post_prob_model4_R6 <- posterior_model_prob[4]

# MK: Grabbing log evidence and log d-posterior precisions for R=6 and model 1
log_est_evi_R6_model1 <- log_Z[1]
log_dpost_prec_R6_model1 <- -log(det(cov(exp(theta[,,1]))))