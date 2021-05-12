
#################################################################################
# File Name:  	Goodness-of-match_sector.R 
# Project:			EU ETS effectiveness paper
# Purpose:      Creates estimates and 95% CIs for sectoral analysis
# Data input:   ../Data/ETS_analysis_sector.RData
# Output File: 		
# Author: 		  Patrick Bayer
# Date:         13 April 2020
#################################################################################


# Load required packages
library(foreign)
library(gsynth)
library(tidyverse)
library(broom)

# Placeholder for by-sector estimates
OUT <- matrix(NA,nrow=5,ncol=3)

# Loop through all five regulated sectors
for(i in (1:5)){
  
# Load data
load("../Data/ETS_analysis_sector.RData")

# Goodness-of-match analysis by sector (levels)
temp <- carbon[as.numeric(carbon$sector)==i & carbon$year>=2005 & carbon$unemit>0,] %>%
  group_by(country) %>% 
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
          emit.total= sum(unemit, na.rm=TRUE))

print(temp, n=Inf)
OUT[i,1] <- summary(temp$ratio.byc)[["Mean"]]

OUT[i,2] <- weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)  

# Goodness-of-match analysis by sector (trends)
temp2 <- carbon[as.numeric(carbon$sector)==i & carbon$year>=2005 & is.na(carbon$ratio)==FALSE,] %>%
  group_by(country) %>% 
  do(fitSector = lm(etsemit ~ unemit-1, data=.))
#}


R2.list <- glance(temp2, fitSector)[,c(1,2)]
print(R2.list, n=Inf)
OUT[i,3] <- mean(R2.list$r.squared)

}

# Plot results table
str(carbon$sector)
OUT

#################################################################################
#                                END OF FILE
#################################################################################