# Reanalysis of MPT Data

R scripts for fitting different types of MPT models using MPTinR (maximum 
likelihood) and TreeBUGS (Bayes) using different types of pooling (complete, 
no, partial).

## Instructions

1. Download or clone all material from GitHub 
   (e.g., via the green button "Download" on the top right) 
2. Create a new subfolder that contains the following three files
   (cf. the example subfolders `bayen-kuhlmann2011/` and `jaeger2012/`):
    1. The MPT model in the .eqn-format
        * The model should be parameterized including all equality constraints.
        * To encode fixed parameters (e.g., g=.50), replace the parameter 
          in the eqn-file by constants.
    2. The data with individual frequencies as a .csv-file
    3. The file `1_main_script.R`(copied from one of the example subfolders). 
3. Adjust the input options in `1_main_script.R` in the section 
   "MPT model definition & Data".
    * Note that the script requires two auxiliary variables: 
      a person-ID variable and a dummy variable encoding between-subject membership.
      If your data contain response frequencies only, uncomment the optional 
      lines in the script to add these two variables manually.
4. Install the newest versions of all packages that are listed on top of the main script.
    * Note: The latest of TreeBUGS is required.
5. Set the working directory to your subfolder
    1. Use `setwd()`
    1. in Rstudio:  Rstudio->Sessions->Set Working Directory->'To Source File Location'
6. Run the main script
    * Note: For testing purposes, MCMC and bootstrap options can be changed under "Settings"
7. The script will save an `.RData` file with results and a few summary plots. 
    * Additionally, a few convergence checks will be printed to a text file.
      For instance, for MCMC output, all parameters with `Rhat > 1.05` or `n.eff < 100` are listed.
    * Note: The goodness-of-fit plots show the T1 and T2 statistics for MCMC fits 
      (labels: "mean" and "cov", respectively).

## Example Data Sets

Currently, the project contains two example models and data sets:

1. `bayen-kuhlmann2011` contains a restricted version (submodel 4) of the 2HT source memory model from Bayen, Murnane, and Erfelder (1996) and data from Bayen and Kuhlmann (2011). The data has two conditions (no oad versus load). The model appears to fit the data quite well (maybe with the exception of the complete pooling MPTinR approach).

2. `jaeger2012` contains two versions of the 2HT recognition memory model for 6-point ROC data and the data from Jaeger, Cox, and Dobbins (2012). The currently selected model has a symmetric response mapping for detection states and a completely unrestricted response mapping for uncertainty (i.e., guessing) states. The data only has one condition. The model does not appear to fit the data. Furthermore, there are quite some convergence issues. Especially the Bayesian variants appear unable to convergence on the target distribution. Furthermore, the beta MPT fails completely. This suggests that a different model (i.e., other parameter restrictions) need to be chosen. However, removing a few problematic participants also seems to remove the problems.

## Current limitations

* For the Bayesian models with "no-pooling" and "complete-pooling", no additional 
  MCMC samples are drawn to achieve the desired level of convergence (e.g., $\hat R < 1.05$).
  This might be addressed in future versions of TreeBUGS. 
  As a remedy, the number of MCMC iterations can be increased a priori.

