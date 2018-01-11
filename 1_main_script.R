#############################################
## Install developer version of TreeBUGS   ##
#############################################

## * cf. installation instructions: 
##   - https://github.com/denis-arnold/TreeBUGS 
##
## * on Windows this requires to install "Rtools" for building C++ packages:
##   - https://cran.r-project.org/bin/windows/Rtools/     
##
## * on Mac this requires "Xcode Command Line Tools"
##   - https://www.maketecheasier.com/install-command-line-tools-without-xcode/
##   - it might be necessary to install the library "gfortran" manually:
##        curl -O http://r.research.att.com/libs/gfortran-4.8.2-darwin13.tar.bz2
##        sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /  

## uncomment and run the following three lines once to install dependencies:
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

# load scripts. make sure that working directory is correct via: setwd() / getwd()
source("2_mptinr_part.R")
source("3_treebugs_part.R")
source("9_auxiliary_functions.R")

# suppress output of JAGS sampler
runjags.options(silent.jags = TRUE, silent.runjags = TRUE)


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
                   Rhat_max = 1.05, Neff_min = 100,  # = stopping criteria for MCMC sampling
                   n.PPP = 2000, nCPU = 8)

MPTINR_OPTIONS <- c(bootstrap_samples = 500,
                    n.optim = 10,
                    nCPU = 8)

CI_SIZE <- c(0.025, 0.1, 0.9, 0.975)
MAX_CI_INDIV <- 0.99

### different settings for testing (faster, but inaccurate results!)
# MPTINR_OPTIONS <- c(bootstrap_samples = 10, n.optim = 2, nCPU = 8)
# TREEBUGS_MCMC <- c(n.chain = 4, n.iter = 2000, n.adapt = 100,
#                    n.burnin = 100, n.thin = 5,
#                    Rhat_max = 5, Neff_min = 5,
#                    n.PPP = 100, nCPU = 8)


##############################
## Analysis (do not change) ##
##############################

res_mptinr <- mpt_mptinr(dataset = DATA_FILE, data = data, model = EQN_FILE,
                         col_id = COL_ID, col_condition = COL_CONDITION)

res_treebugs <- lapply(c("simple", "simple_pooling", "trait", "beta"),
                       FUN = mpt_treebugs, 
                       dataset = DATA_FILE, data = data, model = EQN_FILE,
                       col_id = COL_ID, col_condition = COL_CONDITION)

results <- bind_rows(res_mptinr, res_treebugs)
results

# requires a subfolder "/results"
save(results, file = paste0("results/", EQN_FILE, "-", DATA_FILE, ".RData"))
source("4_summary_plots.R")
