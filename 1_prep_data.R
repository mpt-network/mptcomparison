
library("tidyr")
library("dplyr")
library("tibble")
library("rlang")
library("reshape2")

library("parallel")
library("MPTinR")
library("TreeBUGS")
library("runjags")

source("2_mptinr_part.R")
source("3_treebugs_part.R")
source("9_auxiliary_functions.R")

# suppress output of JAGS sampler
runjags.options(silent.jags = TRUE, silent.runjags = TRUE)


################################### MPT model definition & Data
EQN_FILE <- "2HTSM_Submodel4.eqn"
DATA_FILE <- "Kuhlmann_dl7.csv"

data <- read.csv2(DATA_FILE, fileEncoding = "UTF-8-BOM")
head(data)

COL_CONDITION <- "ExpCond"
COL_ID <- "Subject"

#data <- data[c(1,25),]

# NOTE: experimental condition should be labeled meaningfully!
unique(data[,COL_CONDITION])
data[,COL_CONDITION] <- factor(data[,COL_CONDITION],
                               levels = c(1:2),
                               labels = c("no_load", "load"))


TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 50000, n.adapt = 3000,
                   n.burnin = 2000, n.thin = 10,
                   Rhat_max = 1.05, Neff_min = 100,
                   n.PPP = 2000, nCPU = 8)

MPTINR_OPTIONS <- c(bootstrap_samples = 500,
                    n.optim = 10,
                    nCPU = 8)

CI_SIZE <- c(0.025, 0.1, 0.9, 0.975)
MAX_CI_INDIV <- 0.99


##############################
## DO NOT CHANGE CODE BELOW ##
##############################

################################### data structure  ##########

# COPIED TO mpt_XXX functions

## frequency columns are read from the EQN file and are alphabetically
## ordered within each tree! That is undocumented MPTinR behavior.
# col_freq <- get_eqn_categories(EQN_FILE)
#c(3:5, 7, 6, 8, 11, 9, 10)
#colnames(data)[column_freq]
# data$id <- data[,COL_ID]
# data$condition <- data[,COL_CONDITION]
# data_list <- split(data[,c("id", "condition")], f = data[,COL_CONDITION])
# freq_list <- split(data[,col_freq], f = data[,COL_CONDITION])



# for testing
# MPTINR_OPTIONS <- c(bootstrap_samples = 10, n.optim = 2, nCPU = 8)
# TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 2000, n.adapt = 100,
#                    n.burnin = 100, n.thin = 5,
#                    Rhat_max = 5, Neff_min = 5,
#                    n.PPP = 100, nCPU = 8)
# res_test <- mpt_treebugs(dataset = DATA_FILE, data, model = EQN_FILE, method = "simple", 
#                            col_id = COL_ID, col_condition = COL_CONDITION)

######### run

res_mptinr <- mpt_mptinr(dataset = DATA_FILE, data = data, model = EQN_FILE,
                         col_id = COL_ID, col_condition = COL_CONDITION)
res_treebugs <- lapply(c("simple", "simple_pooling", "trait", "beta"),
                       FUN = mpt_treebugs, 
                       dataset = DATA_FILE, data = data, model = EQN_FILE,
                       col_id = COL_ID, col_condition = COL_CONDITION)

results <- bind_rows(res_mptinr, res_treebugs) #res_simple, res_trait, res_beta)
results
save(results, file = "kuhlmann_sm4.RData")
