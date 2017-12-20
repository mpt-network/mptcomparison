
library("tidyr")
library("dplyr")
library("tibble")
library("MPTinR")
library("TreeBUGS")
library("parallel")
library("rlang")
library("reshape2")
library("tidyr")
library("runjags")

source("3_treebugs_part.R")
source("9_auxiliary_functions.R")

################################### MPT model definition & Data
EQN_FILE <- "2HTSM_Submodel4.eqn"
DATA_FILE <- "Kuhlmann_dl7.csv"

MODEL_NAME <- "2HTSM_4P"
DATA_NAME <- "beatrice"

data <- read.csv2(DATA_FILE, fileEncoding = "UTF-8-BOM")
head(data)

column_condition <- "ExpCond"
column_id <- "Subject"

#data <- data[c(1,25),]

# NOTE: experimental condition should be labeled meaningfully!
unique(data[,column_condition])
data[,column_condition] <- factor(data[,column_condition],
                                  levels = c(1:2),
                                  labels = c("no_load", "load"))


TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 50000, n.adapt = 3000,
                   n.burnin = 2000, n.thin = 10,
                   Rhat_max = 1.05, Neff_min = 100,
                   n.PPP = 2000, nCPU = 4)

MPTINR_OPTIONS <- c(bootstrap_samples = 500,
                    n.optim = 10,
                    nCPU = 3)

CI_SIZE <- c(0.025, 0.1, 0.9, 0.975)
MAX_CI_INDIV <- 0.99


##############################
## DO NOT CHANGE CODE BELOW ##
##############################

################################### data structure  ##########

## frequency columns are read from the EQN file and are alphabetically
## ordered within each tree! That is undocumented MPTinR behavior.
column_freq <- get_eqn_categories(EQN_FILE)
#c(3:5, 7, 6, 8, 11, 9, 10)
#colnames(data)[column_freq]

data$id <- data[,column_id]
data$condition <- data[,column_condition]
data_list <- split(data[,c("id", "condition")], f = data[,column_condition])
freq_list <- split(data[,column_freq], f = data[,column_condition])

######### useful variables

parameters <- check.mpt(EQN_FILE)$parameters
conditions <- levels(data$condition)

cols_ci <- paste0("ci_", CI_SIZE)

#### setup

#### run

# results <- tibble(
#   model = character(),
#   dataset = character(),
#   pooling = character(),
#   package = character(),
#   method = character(),
#   est_group = list(NULL),
#   est_indiv = list(NULL),
#   test_between = list(NULL),
#   #est_cov = est_cov,
#   gof = list(NULL),
#   gof_group = list(NULL),
#   gof_indiv = list(NULL)
# )

# source("2_mptinr_part.R")
results <- bind_rows(mpt_treebugs(DATA_FILE, model = EQN_FILE, method = "trait"),
                     mpt_treebugs(DATA_FILE, model = EQN_FILE, method = "beta"))

results
