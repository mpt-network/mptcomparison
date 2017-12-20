library("ggplot2")


dd <- position_dodge(w = .35)
unnest(results, est_group) %>%
  ggplot(aes(y = est, x = parameter, col=method)) +
  facet_grid(.~condition) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), position = dd)+
  geom_point(position = dd) + ylim(0,1)

cor(results$est_indiv)
