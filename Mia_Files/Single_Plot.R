## Function to output a single plot at a time
# Input M
  plot_data <- theta[,,M] %>% 
    exp %>% 
    as.data.frame()
  
  names(plot_data) <- c('a', 'Th', 'lambda')
  
  if (models[M] %in% 1:2){
  # Plot for a
  figure_A <- ggplot(plot_data, aes(a)) +
    geom_density(lwd = 1.1, fill = 'blue', alpha = 0.3) +
    xlab(TeX('$a$')) +
    theme(axis.title=element_text(size=20,face="bold")) + geom_vline(xintercept = a, lty = 2) + annotate('text',x = a+0.2, y = 1.1, label = 'True a value', angle = 90)
  
  # Plot for Th
  figure_B <- ggplot(plot_data, aes(Th)) +
    geom_density(lwd = 1.1, fill = 'red', alpha = 0.3) +
    xlab(TeX('$T_{h}$')) +
    theme(axis.title=element_text(size=20,face="bold")) + geom_vline(xintercept = Th, lty = 2)+ annotate('text',x = Th+ 0.1, y = 0.5, label = TeX('True $T_{h}$ value'), angle = 90)
  
  # Plot for Lambda
  figure_C <- ggplot(plot_data, aes(lambda)) +
    geom_density(lwd = 1.1, fill = 'green', alpha = 0.3) +
    xlab(expression(lambda)) +
    theme(axis.title=element_text(size=20,face="bold")) + geom_vline(xintercept = lambda, lty = 2)+ annotate('text',x = lambda+ 0.2, y = 0.25, label = expression(paste('True ',lambda,' value')), angle = 90)
  
  plot <- grid.arrange(figure_A, figure_B, figure_C, ncol = 3, widths = c(1,1,1)) %>% 
    annotate_figure(top = text_grob(paste("Marginal posterior distributions for ", modeltype[[models[M]]]),
                                    color = "black", face = "bold", size = 10))
  } else{
    
    plot <- grid.arrange(figure_A, figure_B, ncol= 2, widths = c(1,1)) %>% 
      annotate_figure(top = text_grob(paste("Marginal posterior distributions for", modeltype[[models[M]]]), 
                                      color = "black", face = "bold", size = 10))
  }
  
  print(plot)