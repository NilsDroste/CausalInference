
#################################################################################
# File Name:  	GSynth_sector.R 
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


# Placeholder for by-sector estimates
OUT <- list()

# Loop through all five regulated sectors
for(i in (1:5)){
  
# Load data
load("../Data/ETS_analysis_sector.RData")


# Restrict data to ETS regulated and unregulated emissions
carbon <- carbon[carbon$sector==levels(carbon$sector)[i] | carbon$sector=="Residual",]
carbon <- carbon[with(carbon, order(carbon$country,carbon$year,carbon$sector)),]


# Treatment assignment
D <- 2008

# Create treatment indicator
carbon$ets <- 0
carbon$ets[carbon$sector==levels(carbon$sector)[i] & carbon$year>=D] <- 1

# Create indicator for regulated/unregulated fixed effects
carbon$ctrysector <- paste(carbon$countryname,carbon$sector)


#################################################################################
# Generalized synthetic control
#################################################################################

# Gsynth does not tolerate missing data
carbon.new <- carbon[is.na(carbon$loggdp)==FALSE & is.na(carbon$logUNemit)==FALSE,]

e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new, index=c("ctrysector","year"), force="two-way", se=TRUE, nboots=1000, seed=1234)


# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))



#################################################################################
# Results plot: Each sector separately
#################################################################################


year <- seq(min(carbon.new$year), max(carbon.new$year),1)

OUT[[i]] <- e0$est.att[,1]

# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ATT plot
p <- ggplot() +
  geom_ribbon(aes(x=year,ymin=(exp(e0$est.att[,3])-1)*100,ymax=(exp(e0$est.att[,4])-1)*100),fill=cols[1]) +
  geom_line(aes(x=year,y=(exp(e0$est.att[,1])-1)*100),color=cols[3], size=1.2) +
  labs(x="Year",y="Difference in CO2 emissions (percent)", title="ATT Estimates for EU ETS, 2008-2016", subtitle=paste("Sectoral emissions:",levels(carbon$sector)[i])) +
  geom_vline(aes(xintercept=2005)) +
  geom_vline(aes(xintercept=D), size=1.5) +
  geom_hline(aes(yintercept=0),lty=2)+
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
p

ggsave(p,filename=paste("../Figures/main/sector_",tolower(levels(carbon$sector)[i]),".pdf",sep=""),width=6.5, height=6)

}


#################################################################################
#                                END OF FILE
#################################################################################