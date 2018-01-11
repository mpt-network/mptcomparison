
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

# ### different settings for testing (faster, but inaccurate results)
# MPTINR_OPTIONS <- c(bootstrap_samples = 10, n.optim = 2, nCPU = 8)
# TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 2000, n.adapt = 100,
#                    n.burnin = 100, n.thin = 5,
#                    Rhat_max = 5, Neff_min = 5,
#                    n.PPP = 100, nCPU = 8)

res_mptinr <- mpt_mptinr(dataset = DATA_FILE, data = data, model = EQN_FILE,
                         col_id = COL_ID, col_condition = COL_CONDITION)
res_treebugs <- lapply(c("simple", "simple_pooling", "trait", "beta"),
                       FUN = mpt_treebugs, 
                       dataset = DATA_FILE, data = data, model = EQN_FILE,
                       col_id = COL_ID, col_condition = COL_CONDITION)

results <- bind_rows(res_mptinr, res_treebugs)
results
save(results, file = paste0(EQN_FILE, "-", DATA_FILE, ".RData"))
