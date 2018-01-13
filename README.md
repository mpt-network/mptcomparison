# Reanalysis of MPT Data

R scripts for fitting different types of MPT models using MPTinR (maximum 
likelihood) and TreeBUGS (Bayes) using different types of pooling (complete, 
no, partial).

## Instructions

1. Download or clone all material from GitHub 
   (e.g., via the green button "Download" on the top right) 
2. Create a new subfolder that contains the following three files
   (cf. the example subfolders `kuhlmann2017/` and `jaeger2012/`):
    1. The MPT model in the .eqn-format
        * The model should be parameterized including all equality constraints.
        * To encode fixed parameters (e.g., g=.50), replace the parameter 
          in the eqn-file by constants.
    2. The data with individual frequencies as a .csv-file
    3. The file `1_main_script.R`(copied from one of the example subfolders). 
3. Adjust the input options in `1_main_script.R` in the section 
   "MPT model definition & Data".
4. Install all packages that are listed on top of the main script.
    * Note: The latest developer version of TreeBUGS is required. It can be 
            installed using the instructions on top of the main script.
5. Set the working directory to your subfolder
    A) Use `setwd()`
    B) in Rstudio:  Rstudio->Sessions->Set Working Directory->'To Source File Location'
6. Run the main script
    * Note: For testing purposes, MCMC and bootstrap options can be changed under "Settings"
7. The script will save an `.RData` file with results and a few summary plots.
