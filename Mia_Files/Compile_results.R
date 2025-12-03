
# MK: Saving Results to compile into tables

library(kableExtra)
library(knitr)
# MK: Grabbing posterior probabilities when R = 1 for each model when Model 1 is set to true.
post_prob_model1_R1 <- 0.69354
post_prob_model2_R1 <- 0.30645
post_prob_model3_R1 <- 1.83781e-48
post_prob_model4_R1 <- 3.87094e-64

# MK: Grabbing log evidence and log d-posterior precisions for R=1 and model 1
log_est_evi_R1_model1 <- -30.12135
log_dpost_prec_R1_model1 <- 6.20299

# MK: Grabbing posterior probabilities when R = 0 for each model when Model 1 is set to true.

post_prob_model1_R0 <- 0.526169
post_prob_model2_R0 <- 0.473830
post_prob_model3_R0 <- 5.881768e-62
post_prob_model4_R0 <- 3.439622e-30

# MK: Grabbing log evidence and log d-posterior precisions for R=0 and model 1
log_est_evi_R0_model1 <- -25.8567
log_dpost_prec_R0_model1 <- 7.62085

# MK: Grabbing posterior probabilities when R = 6 for each model when Model 1 is set to true.

post_prob_model1_R6 <- 0.331147
post_prob_model2_R6 <- 0.668852
post_prob_model3_R6 <- 2.06307e-19
post_prob_model4_R6 <- 4.10011e-18

# MK: Grabbing log evidence and log d-posterior precisions for R=6 and model 1
log_est_evi_R6_model1 <- -25.08379
log_dpost_prec_R6_model1 <- 6.68211

# Table of Posterior Model Probabilities
post_probs_combined <- data.frame(Method = c(0,1,6), Posterior_Model_Probabilities= c(post_prob_model1_R0,post_prob_model1_R1,post_prob_model1_R6), Log_Estimate_Evidence = c(log_est_evi_R0_model1,log_est_evi_R1_model1,log_est_evi_R6_model1), Log_dpost = c(log_dpost_prec_R0_model1, log_dpost_prec_R1_model1,log_dpost_prec_R6_model1), Methods_Notes = c('selects design point randomly every experiment and generates data','selects design points for all experiments up front', 'selects design point that maximises utility for parameter estimation and generates data'))
post_probs_table <- post_probs_combined %>%
  kbl(caption = 'Results relating to Model 1 when Model 1 is set to true') %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c('bordered'))
post_probs_table
