#################################
## Load packages and scripts   ##
#################################

# loading packages
# => check that the newest versions are installed (especially for TreeBUGS)!
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
library("readr")
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
plotFreq(data, boxplot = FALSE, eqn = EQN_FILE)

### Optional:
### (A) add a person identifier if missing in data [uncomment if necessary]
# data$Subject <- 1:nrow(data)

### Optional:
### (B) add a dummy variable if data do not contain a between-subject factor  [uncomment if necessary]
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

LOG_FILE <- TRUE      # if TRUE, all output messages are printed to a logfile, not to the console.
AVAILABLE_CORES <- 4  # Set the number of CPU cores that are available on your computer.
# most modern computers have at least 4. It is not recommended to set higher numbers than available.

TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 50000, n.adapt = 3000, 
                   n.burnin = 2000, n.thin = 10,
                   # = stopping criteria for MCMC:
                   Rhat_max = 1.05, Neff_min = 100, extend_max = 20,
                   n.PPP = 2000, nCPU = AVAILABLE_CORES)

MPTINR_OPTIONS <- c(bootstrap_samples = 500,
                    n.optim = 10,
                    nCPU = AVAILABLE_CORES)

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

if(LOG_FILE) sink(paste0("warnings_", DATA_FILE, ".log"))
res_mptinr <- mpt_mptinr(dataset = DATA_FILE, data = data, model = EQN_FILE,
                         col_id = COL_ID, col_condition = COL_CONDITION)

res_treebugs <- map(c("simple", "simple_pooling", "trait", "beta", "trait_uncorrelated"),
                    mpt_treebugs_safe, 
                    dataset = DATA_FILE, data = data, model = EQN_FILE,
                    col_id = COL_ID, col_condition = COL_CONDITION)
if(LOG_FILE) sink()

results <- bind_rows(res_mptinr, res_treebugs)

# print convergence results
check_results(results)

# store results
save(results, data,
     EQN_FILE, DATA_FILE, TREEBUGS_MCMC, MPTINR_OPTIONS, CI_SIZE, MAX_CI_INDIV,
     file = paste0(EQN_FILE, "-", DATA_FILE, ".RData"))
write_check_results(paste0(EQN_FILE, "-", DATA_FILE), results)
plot_results(results, save = TRUE)
