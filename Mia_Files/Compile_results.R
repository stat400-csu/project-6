
# MK: Saving Results to compile into tables

library(kableExtra)
library(png)
library(knitr)
library(grid)
library(cowplot)
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
post_probs_combined <- data.frame(Method = c(0,1,6), Post_Probs_M1 = c(post_prob_model1_R0,post_prob_model1_R1,post_prob_model1_R6), Post_Probs_M2 = c(post_prob_model2_R0, post_prob_model2_R1, post_prob_model2_R6), Methods_Notes = c('selects design point randomly every experiment and generates data', 'selects design point that maximises utility for parameter estimation and generates data', 'selects design points for all experiments up front'))
post_probs_table <- post_probs_combined %>%
  kbl(caption = 'Results when Model 1 is set to true',col.names = c('Method','Posterior Probs Model 1', 'Posterior Probs Model 2', 'Method Notes')) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c('bordered'))
post_probs_table

# Description of methods
methods_summary <- data.frame(Method = c('0','1','6'), Description = c('Select design points randomly every experiment', 'Select design points that maximizes the utility for parameter estimation','Select design points for all experiments up front'), Advantages = c('Computationally light; Flexibile; Realistic','Ideal for maximizing parameter estimation', 'Most realistic; Computationally light'), Disadvantages = c('Less Parameters Estimation Accuracy','Computationally Heavy; Less Realistic',
                                                                                                                                                                                                                                                                                                                                                                                                                                'Not Flexible; Lowest Accuracy'))
methods_table <- methods_summary %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c('bordered'))
methods_table

# True Parameters

par_summary <- data.frame(Parameter = c('a', 'Th', 'λ'), True_value = c(0.5, 0.7, 0.5), Description = c('per capita prey consumption (attack rate)','the time it takes to pursue, subdue, and eat a prey (handling time)', 'overdispersion parameter'), Assumptions = (c('> 0', '> 0','> 0')))
par_table <- par_summary %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c('bordered')) 
par_table
                          
# Compiling plots
plot1 <- readPNG('~/Project/project-6/Paper and Presentation/Model_1R_0.png')
plot2 <- readPNG('~/Project/project-6/Paper and Presentation/Model_1R_1.png')
plot3 <- readPNG('~/Project/project-6/Paper and Presentation/Model_1R_6.png')

plt1 <- rasterGrob(plot1)
plt2 <- rasterGrob(plot2)
plt3 <- rasterGrob(plot3)
result_plots <- grid.arrange(plt1, plt2, plt3, ncol = 3)
ggsave(filename = 'results_plot.png', plot = result_plots)

# Table for Model Types

model_types <- data.frame(Model = c(1,2,3,4), Type = c('Beta Binomial Type II', 'Beta Binomial Type III', 'Binomial Type II', 'Binomial Type III'))
model_types_table <-  model_types %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c('bordered')) 
model_types_table



