# Reanalysis of MPT Data

R scripts for fitting different types of MPT models using MPTinR and TreeBUGS.

## Instructions

1. Copy model in .eqn-format into the main folder (cf. `2HTSM_Submodel4.eqn`)
    * The model should be parameterized including all equality constraints
    * To encode fixed parameters (e.g., g=.50), replace the parameter in the eqn-file by the constant
2. Copy the individual frequencies as a .csv-file into the main folder (cf. `Kuhlmann_dl7.csv`)
3. Open the file `1_main_script.R` and adjust the input options in 
   the section "MPT model definition & Data"
4. Set the working directory to the main folder (using either `setwd()` or in Rstudio->Sessions->Set Working Directory)
4. Run the full script
    * Note: For testing purposes, MCMC and bootstrap options can be changed under "Settings""
5. The script will save an `.RData` file with results and a few summary plots in the subfolder `/results`
