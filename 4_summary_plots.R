library("ggplot2")


dd <- position_dodge(w = .5)
unnest(results, est_group) %>%
  ggplot(aes(y = est, x = parameter, 
             col=interaction(method, pooling,package),
             shape=interaction(method, pooling,package))) +
  facet_grid(.~condition) +
  geom_errorbar(aes(ymin = est-se, ymax = est+se), position = dd)+
  geom_point(position = dd) + ylim(0,1) + 
  theme_bw()
ggsave("plots/estimates.pdf", h = 4.5, w = 8)


unnest(results, test_between) %>%
  ggplot(aes(y = est_diff, x = parameter, 
             col=interaction(method, pooling,package),
             shape=interaction(method, pooling,package))) +
  facet_grid(condition2~condition1) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975), position = dd)+
  geom_point(position = dd) + ylim(-1,1) + 
  theme_bw() + geom_hline(yintercept = 0, lty = 2)
ggsave("plots/test_between.pdf", h = 4.5, w = 5)


unnest(results, gof) %>%
  filter(focus == "mean") %>%
  ggplot(aes(y = p, x = method)) + 
  geom_point() + ylim(0, 1) + 
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw()
ggsave("plots/gof.pdf", h = 4, w = 5)


unnest(results, gof_group) %>%
  filter(focus == "mean") %>%
  ggplot(aes(y = p, x = method, col = condition)) + 
  geom_point() + ylim(0, 1) + 
  geom_hline(yintercept = .05, lty = 2)+
  theme_bw()
ggsave("plots/gof_group.pdf", h = 4, w = 5)


