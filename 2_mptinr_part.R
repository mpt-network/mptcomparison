
#source("1_prep_data.R")


cl <- makeCluster(rep("localhost", MPTINR_OPTIONS["nCPU"])) # make cluster
clusterEvalQ(cl, library("MPTinR"))
clusterExport(cl = cl,
              c("MPTINR_OPTIONS",
                "EQN_FILE", 
                "data",
                "column_freq"))

################
## no pooling ##
################

no_pooling <- make_results_row(model = MODEL_NAME, 
                 dataset = DATA_NAME, 
                 pooling = "no", 
                 package = "MPTinR", 
                 method = "PB", 
                 data = data, 
                 parameters = parameters)

fit_mptinr <- fit.mpt(data[,column_freq], 
                      model.filename = EQN_FILE, 
                      n.optim = MPTINR_OPTIONS["n.optim"], 
                      fit.aggregated = FALSE, 
                      show.messages = FALSE)

no_pooling$gof_indiv[[1]]$type <- "pb-G2"
no_pooling$gof_indiv[[1]]$focus <- "mean"
no_pooling$gof_indiv[[1]]$stat_obs <- 
  fit_mptinr$goodness.of.fit$individual$G.Squared
no_pooling$gof_indiv[[1]]$stat_df <- 
  fit_mptinr$goodness.of.fit$individual$df


get_pb_output <- function(i, fit_mptinr) {
  gen_data <- 
    gen.data(fit_mptinr$parameters$individual[,"estimates",i], 
             samples = MPTINR_OPTIONS["bootstrap_samples"], 
             model.filename = EQN_FILE,
             data = unlist(data[i,column_freq]))
  fit.mpt(gen_data, 
            model.filename = EQN_FILE, 
            fit.aggregated = FALSE,
            n.optim = MPTINR_OPTIONS["n.optim"],
            show.messages = FALSE)
}

fit_pb <- clusterApplyLB(cl, seq_len(nrow(data)),
                         get_pb_output, fit_mptinr = fit_mptinr)

## make est_indiv and gof_indiv
for (i in seq_len(nrow(data))) {

  for (p in parameters) {
    no_pooling$est_indiv[[1]][ 
       no_pooling$est_indiv[[1]]$id == data[i,"id"] & 
         no_pooling$est_indiv[[1]]$parameter == p, 
                            "est" ] <- 
      fit_mptinr$parameters$individual[p,"estimates",i]
      
     no_pooling$est_indiv[[1]][ 
       no_pooling$est_indiv[[1]]$id == data[i,"id"] & 
         no_pooling$est_indiv[[1]]$parameter == p, 
                             cols_ci ] <- 
    quantile(fit_pb[[i]]$parameters$individual[p,"estimates",], probs = CI_SIZE)
   no_pooling$est_indiv[[1]][ 
       no_pooling$est_indiv[[1]]$id == data[i,"id"] & 
         no_pooling$est_indiv[[1]]$parameter == p, 
                             "sd" ] <-
     sd(fit_pb[[i]]$parameters$individual[p,"estimates",])
  }
  
  # gof_indiv
  no_pooling$gof_indiv[[1]][ 
    no_pooling$gof_indiv[[1]]$id == data[i,"id"], "p" ] <- 
  (sum(fit_pb[[i]]$goodness.of.fit$individual$G.Squared > 
           fit_mptinr$goodness.of.fit$individual[i,"G.Squared"]) + 1) /
    (MPTINR_OPTIONS["bootstrap_samples"] + 1)
  
}

#### make est_group ####

tmp <- no_pooling$est_indiv[[1]]
tmp$range_ci <- tmp[,cols_ci[length(cols_ci)]][[1]] - tmp[,cols_ci[1]][[1]]

est_group <- tmp %>% 
  filter(range_ci < MAX_CI_INDIV) %>% 
  group_by(condition, parameter) %>%
  summarise(estN = mean(est),
            sd = sd(est),
            quant = list(as.data.frame(t(quantile(est, prob = CI_SIZE))))) %>% 
  unnest(quant) %>% 
  ungroup() %>% 
  rename(est = estN)
colnames(est_group)[ 
  (length(colnames(est_group))-length(CI_SIZE)+1):length(colnames(est_group)) 
  ] <- cols_ci


no_pooling$est_group[[1]] <- 
  right_join(est_group, 
             no_pooling$est_group[[1]][,c("condition", "parameter")], 
             by = c("condition", "parameter"))

#### make gof_group ####
no_pooling$gof_group[[1]]$type <- "pb-G2"
no_pooling$gof_group[[1]]$focus <- "mean"

tmp <- fit_mptinr$goodness.of.fit$individual
tmp$condition <- data$condition
gof_group <- tmp %>% 
  group_by(condition) %>% 
  summarise(stat_obs = sum(G.Squared),
            stat_df = sum(df))
gof_group$p <- NA_real_

g2_all <- vapply(fit_pb, 
       function(x) x$goodness.of.fit$individual$G.Squared, 
       rep(0, MPTINR_OPTIONS["bootstrap_samples"]))

g2_cond <- vector("list", length(conditions))

for (i in seq_along(conditions)) {
  g2_cond[[i]] <- apply(g2_all[, data$condition == conditions[i]], 1, sum)
  no_pooling$gof_group[[1]][ no_pooling$gof_group[[1]]$condition == conditions[i], "stat_obs" ] <- gof_group[ gof_group$condition == conditions[i], "stat_obs"]
  no_pooling$gof_group[[1]][ no_pooling$gof_group[[1]]$condition == conditions[i], "stat_df" ] <- gof_group[ gof_group$condition == conditions[i], "stat_df"]
  no_pooling$gof_group[[1]][ no_pooling$gof_group[[1]]$condition == conditions[i], "p" ] <- 
    (sum(gof_group[ gof_group$condition == conditions[i], "stat_obs"][[1]] < 
           g2_cond[[i]]) + 1) / (MPTINR_OPTIONS["bootstrap_samples"] + 1)
}


#### make gof ####
no_pooling$gof[[1]]$type <- "pb-G2"
no_pooling$gof[[1]]$focus <- "mean"
no_pooling$gof[[1]]$stat_obs <- fit_mptinr$goodness.of.fit$sum$G.Squared
no_pooling$gof[[1]]$stat_df <- fit_mptinr$goodness.of.fit$sum$df

g2_all <- vapply(fit_pb, 
       function(x) x$goodness.of.fit$individual$G.Squared, 
       rep(0, MPTINR_OPTIONS["bootstrap_samples"]))

g2_cond <- apply(g2_all, 1, sum)
no_pooling$gof[[1]]$p <- 
  (sum(no_pooling$gof[[1]]$stat_obs < g2_cond) + 1) / 
  (MPTINR_OPTIONS["bootstrap_samples"] + 1)

stopCluster(cl)

######################
## complete pooling ##
######################

complete_pooling <- make_results_row(model = MODEL_NAME, 
                 dataset = DATA_NAME, 
                 pooling = "complete", 
                 package = "MPTinR", 
                 method = "asymptotic", 
                 data = data, 
                 parameters = parameters)

complete_pooling$est_indiv <- list(NULL)
complete_pooling$gof_indiv <- list(NULL)

#### fully aggregated:

fit_mptinr_agg <- fit.mpt(colSums(data[,column_freq]), 
                      model.filename = EQN_FILE, 
                      n.optim = MPTINR_OPTIONS["n.optim"], 
                      show.messages = FALSE)

## gof

complete_pooling$gof[[1]][1,"type"] <- "G2"
complete_pooling$gof[[1]][1,"focus"] <- "mean"
complete_pooling$gof[[1]][1,"stat_obs"] <- 
  fit_mptinr_agg$goodness.of.fit$G.Squared
complete_pooling$gof[[1]][1,"stat_df"] <- 
  fit_mptinr_agg$goodness.of.fit$df
complete_pooling$gof[[1]][1,"p"] <- 
  fit_mptinr_agg$goodness.of.fit$p

#### aggregated by condition

complete_pooling$gof_group[[1]][,"type"] <- "G2"
complete_pooling$gof_group[[1]][,"focus"] <- "mean"

for (i in seq_along(conditions)) {
  fit_mptinr_tmp <- fit.mpt(colSums(freq_list[[conditions[i]]][,column_freq]), 
                      model.filename = EQN_FILE, 
                      n.optim = MPTINR_OPTIONS["n.optim"], 
                      show.messages = FALSE, 
                      output = "full")
  
  
  complete_pooling$gof_group[[1]][ 
    complete_pooling$gof_group[[1]]$condition == conditions[i] ,"stat_obs"] <- 
    fit_mptinr_tmp$goodness.of.fit$G.Squared
  complete_pooling$gof_group[[1]][
    complete_pooling$gof_group[[1]]$condition == conditions[i], "stat_df"] <- 
    fit_mptinr_tmp$goodness.of.fit$df
  complete_pooling$gof_group[[1]][
    complete_pooling$gof_group[[1]]$condition == conditions[i], "p"] <- 
    fit_mptinr_tmp$goodness.of.fit$p
  
  complete_pooling$est_group[[1]][
    complete_pooling$est_group[[1]]$condition == conditions[i], "est"
    ] <- fit_mptinr_tmp$parameters[  
      complete_pooling$est_group[[1]][
        complete_pooling$est_group[[1]]$condition == conditions[i],
        ]$parameter, "estimates" ]
  
  par_sd <- sqrt(diag(solve(fit_mptinr_tmp$hessian[[1]])))
  names(par_sd) <- rownames(fit_mptinr_tmp$parameters)
  
  complete_pooling$est_group[[1]][
    complete_pooling$est_group[[1]]$condition == conditions[i], "sd"
    ] <- par_sd[  
      complete_pooling$est_group[[1]][
        complete_pooling$est_group[[1]]$condition == conditions[i],
        ]$parameter]
  
}

for (i in seq_along(CI_SIZE)) {
  complete_pooling$est_group[[1]][,cols_ci[i]] <- 
    complete_pooling$est_group[[1]][,"est"] + 
    qnorm(CI_SIZE[i])*complete_pooling$est_group[[1]][,"sd"]
}

  
results <- bind_rows(results, no_pooling, complete_pooling)
