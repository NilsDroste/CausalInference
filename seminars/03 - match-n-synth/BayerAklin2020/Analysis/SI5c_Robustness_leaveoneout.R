
#################################################################################
# File Name:    Robustness_leaveoneout.R
# Project:			EU ETS effectiveness paper
# Purpose:      Creates gsynth estimates when one country is left out at a time
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

ctry.list <- unique(carbon$country)
OUT <- list()

# Loop through countries and plot outputs separately
for(i in (1:length(ctry.list))){

# Treatment assignment
D <- 2008

# Create treatment indicator
carbon$ets <- 0
carbon$ets[carbon$sector=="Regulated" & carbon$year>=D] <- 1

# Create indicator for regulated/unregulated fixed effects
carbon$ctrysector <- paste(carbon$countryname,carbon$sector)


#################################################################################
# Generalized synthetic control: Leave one out estimates
#################################################################################

# Gsynth does not tolerate data with missings
carbon.new <- carbon[is.na(carbon$loggdp)==FALSE & is.na(carbon$logUNemit)==FALSE,]

e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new[carbon.new$country!=ctry.list[i],], index=c("ctrysector","year"), force="two-way", se=TRUE, nboots=1000, seed=1234)
e0


OUT[[i]] <- e0$est.att[,1]
}

#################################################################################
# Generalized synthetic control: All country estimates
#################################################################################

eall <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new, index=c("ctrysector","year"), force="two-way", se=TRUE, nboots=1000, seed=1234)
eall




#################################################################################
# Robustness plot: Leave one out
#################################################################################

library(plyr)

# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

year <- seq(min(carbon.new$year), max(carbon.new$year),1)
df <- ldply(OUT, data.frame)
df$year <- rep(year,length(ctry.list))
df$ctry <- sort(rep(ctry.list,length(year)))


 
# ATT plot
p <- ggplot() +
  geom_line(data=df,aes(x=year,y=(exp(df[,1])-1)*100,group=ctry),color=cols[1]) +
  geom_line(aes(x=year,y=(exp(eall$est.att[,1])-1)*100),color=cols[3], size=1.2) +
  labs(x="Year",y="Difference in CO2 emissions (percent)", title="ATT Estimates for EU ETS, 2008-2016", subtitle="Leave-One-Out Robustness") +
  geom_vline(aes(xintercept=D), size=1.5) +
  geom_vline(aes(xintercept=2005)) +
  geom_hline(aes(yintercept=0),lty=2)+
  ylim(-30,10) +
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
p

ggsave(p,filename=paste("../Figures/si/robust_leaveoneout.png",sep=""),width=6.5, height=6)


#################################################################################
#                                END OF FILE
#################################################################################