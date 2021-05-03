###########################################################################
# Application: Marcus and Sant'Anna (2020) based on Grooms (2015)
# Main file
###########################################################################
# Start-up
#----------------------------------------------------------------------------
# Install and load package from github
devtools::install_github("bcallaway11/did")
devtools::install_github("pedrohcgs/MarcusSantAnna2020")
library(foreign)
library(stats)
library(MarcusSantAnna2020)
library(did)
library(DRDID)
library(haven)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(lfe)
library(fastDummies)
library(panelView)
library(here)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Set seed
set.seed(1234)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Set the Working Directory
#address should be the subfolder with the data
address <- here()
setwd(address)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
nboot = 1000 # Number of bootstrap draws
alp = 0.10 # Significance level
maxe = 19 # Number of post-treatment periods to analyze
mine = -21
#----------------------------------------------------------------------------
###########################################################################
cv.pointwise <-  stats::qnorm(1 - alp/2)
##############################################
#----------------------------------------------------------------------------
# Get Event study for baseline specification
source(here("R_files/grooms-baseline-violrate.R"))
# Get aggregated estimators using baseline specification
source(here("R_files/grooms-baseline-violrate-aggregation.R"))

# Get Event study allowing corruption-specific trends
source(here("R_files/grooms-corrupt-violrate.R"))
# Get aggregated estimators allowing corruption-specific trends
source(here("R_files/grooms-corrupt-violrate-aggregation.R"))

# Get Event study ruling out corruption-specific trends
source(here("R_files/grooms-corrupt-stronger-pta-violrate.R"))
# Get aggregated estimators ruling out corruption-specific trends
source(here("R_files/grooms-corrupt-stronger-pta-violrate-aggregation.R"))
