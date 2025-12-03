#-----------------------------------------------------------------------------------------------#
# Script Name: SMC_plot                                                                         #
# Author: Hayden Moffat                                                                         #
# email: hayden.moffat@hdr.qut.edu.au                                                           #
#                                                                                               #
# This R script plots the marginal posterior distributions for each of the paramters after the  #
# sequential experimental design algorithm is complete.                                         #
#                                                                                               #                                                                                            #
#-----------------------------------------------------------------------------------------------#


# Plot marginal posterior distributions
plot_data <- theta[,,M] %>% 
  exp %>% 
  as.data.frame()

names(plot_data) <- c('a', 'Th', 'lambda')

# Plot for a
figure_A <- ggplot(plot_data, aes(a)) +
  geom_density(lwd = 1.1, fill = 'blue', alpha = 0.3) +
  xlab(TeX('$a$')) +
  theme(axis.title=element_text(size=20,face="bold")) + geom_vline(xintercept = a, lty = 2) + annotate('text',x = a+0.2, y = 1.1, label = 'True a value', angle = 90)

# Plot for Th
figure_B <- ggplot(plot_data, aes(Th)) +
  geom_density(lwd = 1.1, fill = 'red', alpha = 0.3) +
  xlab(TeX('$T_{h}$')) +
  theme(axis.title=element_text(size=20,face="bold")) + geom_vline(xintercept = Th, lty = 2)+ annotate('text',x = Th+ 0.1, y = 0.5, label = TeX('True Th value'), angle = 90)

# Plot for lambda
figure_C <- ggplot(plot_data, aes(lambda)) +
  geom_density(lwd = 1.1, fill = 'green', alpha = 0.3) +
  xlab(expression(lambda)) +
  theme(axis.title=element_text(size=20,face="bold")) + geom_vline(xintercept = lambda, lty = 2)+ annotate('text',x = lambda+ 0.2, y = 0.25, label = 'True λ value', angle = 90)

if (models[M] %in% c(1,2)){
  plot <- grid.arrange(figure_A, figure_B, figure_C, ncol = 3, widths = c(1,1,1)) %>% 
    annotate_figure(top = text_grob(paste("Marginal posterior distributions for ", modeltype[[models[M]]]),
                                    color = "black", face = "bold", size = 10))
# ggarrange(figure_A, figure_B, figure_C, nrow = 1)
  } else{
  plot <- grid.arrange(figure_A, figure_B, ncol= 2, widths = c(1,1)) %>% 
    annotate_figure(top = text_grob(paste("Marginal posterior distributions for", modeltype[[models[M]]]), 
                                    color = "black", face = "bold", size = 10))
  #ggarrange(figure_A,figure_B, nrow = 1)
  }

ggsave(filename = paste0('Model_',models[M],'R_',R,'.png'),
       plot = plot,
       width = 12,
       height = 4,
       dpi = 300)

print(plot)