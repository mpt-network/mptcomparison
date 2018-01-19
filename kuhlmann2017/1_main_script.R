#############################################
## Install developer version of TreeBUGS   ##
#############################################

### the newest TreeBUGS needs to be installed once using one of the following 3 options:

### (A) on Windows:
# install.packages(c("runjags", "hypergeo","rjags", "logspline","RcppArmadillo"))
# install.packages("../scripts/TreeBUGS_1.2.0.zip", repos = NULL)

### (B) on Mac:
# install.packages(c("runjags", "hypergeo","rjags", "logspline","RcppArmadillo"))
# install.packages("../scripts/TreeBUGS_1.2.0.XXX", repos = NULL)

### (C) compile source package (requires tools for building C++ packages)
# install.packages(c("devtools", "runjags", "hypergeo","rjags", "logspline","RcppArmadillo"))
# library("devtools")
# devtools::install_github("denis-arnold/TreeBUGS")


#################################
## Load packages and scripts   ##
#################################

# load packages
library("tidyr")
library("dplyr")
library("tibble")
library("rlang")
library("reshape2")
library("ggplot2")
library("parallel")
library("MPTinR")
library("TreeBUGS")
library("runjags")
library("purrr")
library("broom")
runjags.options(silent.jags = TRUE, silent.runjags = TRUE)

# load scripts. make sure that working directory is correct via either:
# - setwd() / getwd()
# - Rstudio->Sessions->Set Working Directory->'To Source File Location'
source("../scripts/mptinr.R")
source("../scripts/treebugs.R")
source("../scripts/auxiliary_functions.R")
source("../scripts/summary_plots.R")


#################################
## MPT model definition & Data ##
#################################

EQN_FILE <- "2HTSM_Submodel4.eqn"
DATA_FILE <- "Kuhlmann_dl7.csv"  


### if .csv format uses semicolons ";" (German format)
data <- read.csv2(DATA_FILE, fileEncoding = "UTF-8-BOM")
### if .csv format uses commata "," (international format)
# data <- read.csv(DATA_FILE, fileEncoding = "UTF-8-BOM")
head(data)
plotFreq(data, boxplot = FALSE, eqn = "2HTSM_Submodel4.eqn")


### optional: add a person identifier if missing in data
# data$Subject <- 1:nrow(data)
### optional: add a dummy variable if data do not contain a between-subject factor
# data$ExpCond <- "no_condition"


COL_ID <- "Subject"         # name of the variable encoding subject ID
COL_CONDITION <- "ExpCond"  # name of the variable encoding group membership

# NOTE: experimental condition should be labeled meaningfully!
unique(data[,COL_CONDITION])
data[,COL_CONDITION] <- factor(data[,COL_CONDITION],
                               levels = c(1:2),
                               labels = c("no_load", "load"))

### check input data frame
head(data)


#################################
## Settings                    ##
#################################


TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 50000, n.adapt = 3000, 
                   n.burnin = 2000, n.thin = 10,
                   # = stopping criteria for MCMC:
                   Rhat_max = 1.05, Neff_min = 100, extend_max = 20,
                   n.PPP = 2000, nCPU = 8)

MPTINR_OPTIONS <- c(bootstrap_samples = 500,
                    n.optim = 10,
                    nCPU = 8)

CI_SIZE <- c(0.025, 0.1, 0.9, 0.975)
MAX_CI_INDIV <- 0.99

### different settings for testing (faster, but inaccurate results!)
# MPTINR_OPTIONS <- c(bootstrap_samples = 10, n.optim = 2, nCPU = 8)
# TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 1000, n.adapt = 500,
#                    n.burnin = 100, n.thin = 2,
#                    Rhat_max = 10, Neff_min = 3, extend_max = 2,
#                    n.PPP = 10, nCPU = 8)


##############################
## Analysis (do not change) ##
##############################

res_mptinr <- mpt_mptinr(dataset = DATA_FILE, data = data, model = EQN_FILE,
                         col_id = COL_ID, col_condition = COL_CONDITION)

res_treebugs <- map(c("simple", "simple_pooling", "trait", "beta"),
                       mpt_treebugs_safe, 
                       dataset = DATA_FILE, data = data, model = EQN_FILE,
                       col_id = COL_ID, col_condition = COL_CONDITION)

results <- bind_rows(res_mptinr, res_treebugs)
check_results(results)
results

# requires a subfolder "/results"
save(results, file = paste0(EQN_FILE, "-", DATA_FILE, ".RData"))

plot_results(results, save = TRUE)
