library("ggplot2")

plot_results <- function (results, save = TRUE){
  
  prefix <- gsub("\\.", "_", paste0(results$dataset[1],"_"))
  
  dd <- position_dodge(w = .5)
  unnest(results, est_group) %>%
    ggplot(aes(y = est, x = parameter, 
               col=interaction(method, pooling,package),
               shape=interaction(method, pooling,package))) +
    facet_grid(.~condition) +
    geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd, 
                  width = 0.4)+
    geom_point(position = dd) + ylim(0,1) + 
    theme_bw()
  if(save) ggsave(paste0(prefix,"estimates.pdf"), h = 4.5, w = 10)
  
  
  res_between <-  unnest(results, test_between) 
  if (nrow(res_between) > 0){
    ggplot(res_between, aes(y = est_diff, x = parameter, 
                            col=interaction(method, pooling,package),
                            shape=interaction(method, pooling,package))) +
      facet_grid(condition2~condition1) +
      geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), 
                    position = dd, width = 0.5)+
      geom_point(position = dd) + ylim(-1,1) + 
      theme_bw() + geom_hline(yintercept = 0, lty = 2)
    if(save) ggsave(paste0(prefix,"test_between.pdf"), h = 4.5, w = 8)
  }
  
  
  unnest(results, gof) %>%
    # filter(focus == "mean") %>%
    ggplot(aes(y = p, 
               x = interaction(method, pooling, package))) + 
    geom_point() + ylim(0, 1) + 
    geom_hline(yintercept = .05, lty = 2)+
    theme_bw() + coord_flip() +
    facet_wrap(~focus) +
    ggtitle("Goodness of fit")
  if(save) ggsave(paste0(prefix,"gof.pdf"), h = 4, w = 5)
  
  
  if (nrow(res_between) > 0){
    unnest(results, gof_group) %>%
      ggplot(aes(y = p, 
                 x = interaction(method, pooling, package), 
                 col = condition)) + 
      geom_point() + ylim(0, 1) + 
      geom_hline(yintercept = .05, lty = 2)+
      theme_bw() +
      coord_flip() +
      facet_wrap(~focus) +
      ggtitle("Goodness of fit")
    if(save) ggsave(paste0(prefix,"gof_group.pdf"), h = 4, w = 5)
  }
}
