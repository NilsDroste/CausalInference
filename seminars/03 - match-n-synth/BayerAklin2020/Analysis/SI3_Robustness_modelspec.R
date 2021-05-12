
#################################################################################
# File Name:  	Robustness_modelspec.R 
# Project:			EU ETS effectiveness paper
# Purpose:      Creates estimates and 95% CIs for alternative model specifications
# Data input:   ../Data/ETS_analysis.RData
# Output File: 		
# Author: 		  Patrick Bayer
# Date:         13 April 2020
#################################################################################

# Load required packages
library(foreign)
library(gsynth)
library(tidyverse)


# Load data
load("../Data/ETS_analysis.RData")

# Restrict data to ETS regulated and unregulated emissions
carbon <- carbon[carbon$sector=="Regulated" | carbon$sector=="Unregulated",]
carbon <- carbon[with(carbon, order(carbon$country,carbon$year,carbon$sector)),]

# Treatment assignment
D <- 2008

# Create treatment indicator
carbon$ets <- 0
carbon$ets[carbon$sector=="Regulated" & carbon$year>=D] <- 1

# Create indicator for regulated/unregulated fixed effects
carbon$ctrysector <- paste(carbon$countryname,carbon$sector)


#################################################################################
# Generalized synthetic control
#################################################################################


# Estimates for ETS 2008: GDP per capita added
e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2 + loggdppc, data=carbon, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234,
             na.rm=TRUE)

# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))


# Estimates for ETS 2008: Population added
e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2 + logpop, data=carbon, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234,
             na.rm=TRUE)

# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))


# Estimates for ETS 2008: Renewable electricity production added
e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2 + logrenew, data=carbon, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234,
             na.rm=TRUE)

# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))


# Estimates for ETS 2008: Carbon tax indicator added
e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2 + carbontax, data=carbon, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234,
             na.rm=TRUE)

# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))


# Estimates for ETS 2008: full model
e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2 + loggdppc + logpop + logrenew + carbontax, data=carbon, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234,
             na.rm=TRUE)

# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))





#################################################################################
#                                END OF FILE
#################################################################################