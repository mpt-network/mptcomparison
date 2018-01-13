# Reanalysis of MPT Data

R scripts for fitting different types of MPT models using MPTinR and TreeBUGS.

## Instructions

1. Download or clone all material from GitHub (e.g., via the green button "Download" on the top right) 
2. Create a new subfolder with the following three files:
    1. Copy your model in the .eqn-format into your subfolder (cf. `kuhlmann2017/2HTSM_Submodel4.eqn`)
        * The model should be parameterized including all equality constraints
        * To encode fixed parameters (e.g., g=.50), replace the parameter in the eqn-file by the constant
    2. Copy the data with individual frequencies as a .csv-file into your subfolder (cf. `kuhlmann2017/Kuhlmann_dl7.csv`)
    3. Copy the file `kuhlmann2017/1_main_script.R` adjust the input options in the section "MPT model definition & Data".
3. Install all packages that are listed on top of the main script
    * Note: The latest developer version of TreeBUGS is required as explained on top of the main script.
4. Set the working directory to your subfolder (use either `setwd()` or in Rstudio->Sessions->Set Working Directory->'To Source File Location')
5. Run the main script
    * Note: For testing purposes, MCMC and bootstrap options can be changed under "Settings""
6. The script will save an `.RData` file with results and a few summary plots.
