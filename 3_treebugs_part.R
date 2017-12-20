
# suppress output
runjags.options(silent.jags = TRUE, silent.runjags = TRUE)

# overall goodness of fit across between-subject conditions
aggregate_ppp <- function(ppp_list, stat = "T1"){
  obs <- vapply(ppp_list, "[[", paste0(stat, ".obs"),
                FUN.VALUE = ppp_list[[1]]$T1.obs)
  pred <- vapply(ppp_list, "[[", paste0(stat, ".pred"),
                 FUN.VALUE = ppp_list[[1]]$T1.obs)
  s.obs <- rowSums(obs)
  s.pred <- rowSums(pred)
  c(stat_obs = mean(s.obs), stat_pred = mean(s.pred),
    stat_df = NA, p = mean(s.obs < s.pred))
}

mpt_treebugs <- function (dataset, model, method = "trait",
                          column_id = "id", column_condition = "condition"){
  conditions <- levels(factor(data[[column_condition]]))

  result_row <- make_results_row(model = model,
                                 dataset = dataset,
                                 pooling = "partial",
                                 package = "TreeBUGS",
                                 method = method,
                                 data = data,
                                 parameters = parameters)

  gof_group <- list()
  # treebugs_fit <- list()
  for (i in seq_along(freq_list)){
    cond <- factor(conditions[i], conditions)
    data_group <- freq_list[[i]]
    sel_condition <- data[[column_condition]] == conditions[i]
    rownames(data_group) <- data[[column_id]][sel_condition]
    fit_args <- list(eqnfile=model,
                     data = data_group,
                     n.chain = TREEBUGS_MCMC["n.chain"],
                     n.iter = TREEBUGS_MCMC["n.iter"],
                     n.adapt = TREEBUGS_MCMC["n.adapt"],
                     n.burnin = TREEBUGS_MCMC["n.burnin"],
                     n.thin = TREEBUGS_MCMC["n.thin"])
    treebugs_fit <- do.call(paste0(method, "MPT"), args = fit_args)
    summ <- summarizeMCMC(treebugs_fit$runjags$mcmc)

    # continue MCMC sampling
    while (any(na.omit(summ[,"Rhat"]) > TREEBUGS_MCMC["Rhat_max"] )  |
           any(summ[summ[,"n.eff"] > 0,"n.eff"] < TREEBUGS_MCMC["Neff_min"]) ){
      treebugs_fit <- extendMPT(treebugs_fit,
                                n.iter = TREEBUGS_MCMC["n.iter"],
                                n.adapt = TREEBUGS_MCMC["n.adapt"])
      summ <- summarizeMCMC(treebugs_fit$runjags$mcmc)
    }

    # parameter estimates
    summMPT <- summarizeMPT(treebugs_fit$runjags$mcmc,
                            mptInfo = treebugs_fit$mptInfo,
                            probs = CI_SIZE)

    sel_group <- result_row$est_group[[1]]$condition == conditions[i]
    result_row$est_group[[1]][sel_group,-(1:2)] <-
      summMPT$groupParameters$mean[paste0("mean_", parameters),1:6]

    # # old: array filled into data frame
    # result_row$est_indiv[[1]][sel_ind,-(1:3)] <-
    #   summMPT$individParameters[parameters,,1:(2+length(CI_SIZE))]
    sel_ind <- result_row$est_indiv[[1]]$condition == conditions[i]
    dimnames(summMPT$individParameters)$ID <- rownames(data_group)
    tmp <- summMPT$individParameters[parameters,,1:(2+length(CI_SIZE))] %>%
      melt %>% spread("Statistic", "value")
    colnames(tmp) <- c("parameter", column_id, colnames(result_row$est_indiv[[1]])[-(1:3)])
    tmp[[column_condition]] <- cond
    result_row$est_indiv[[1]][sel_ind,] <-
      left_join(result_row$est_indiv[[1]][sel_ind,] %>%
                  select_(column_id, column_condition, "parameter"),
                tmp, by = c("parameter",column_id, column_condition))


    gof_group[[i]] <- PPP(treebugs_fit, M = TREEBUGS_MCMC["n.PPP"], type = "G2",
                          T2 = TRUE, nCPU = TREEBUGS_MCMC["nCPU"])

    sel_gof <- result_row$gof_group[[1]]$condition == conditions[i]
    result_row$gof_group[[1]][sel_gof,] <-
      result_row$gof_group[[1]] %>%
      filter(condition == conditions[i]) %>%
      mutate(condition = conditions[i],
             type = "T1_G2", focus = "mean",
             stat_obs = mean(gof_group[[i]]$T2.obs),
             stat_pred = mean(gof_group[[i]]$T1.pred),
             p = gof_group[[i]]$T1.p)

    result_row$gof_group[[1]] <- add_row(result_row$gof_group[[1]],
                                         condition = cond,
                                         type = "T2", focus = "cov",
                                         stat_obs = mean(gof_group[[i]]$T2.obs),
                                         stat_pred = mean(gof_group[[i]]$T2.pred),
                                         p = gof_group[[i]]$T2.p)

    sel_fog_ind <- result_row$gof_indiv[[1]]$condition == conditions[i]
    result_row$gof_indiv[[1]][sel_fog_ind,] <-
      result_row$gof_indiv[[1]][sel_fog_ind,] %>%
      mutate(condition = conditions[i],
             type = "T1_G2",
             focus = "mean",
             stat_obs = colMeans(gof_group[[i]]$ind.T1.obs),
             stat_pred = colMeans(gof_group[[i]]$ind.T1.pred),
             p = gof_group[[i]]$ind.T1.p)
  }
  result_row$gof[[1]] <- add_row(result_row$gof[[1]])   # T1 & T2
  result_row$gof[[1]]$type <- c("T1_G2", "T2")
  result_row$gof[[1]]$focus <- c("mean", "cov")
  result_row$gof[[1]][1,-(1:2)] <- aggregate_ppp(gof_group)
  result_row$gof[[1]][2,-(1:2)] <- aggregate_ppp(gof_group, stat = "T2")

  result_row
}


