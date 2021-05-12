
#################################################################################
# File Name:  	Robustness_eu.R
# Project:			EU ETS effectiveness paper
# Purpose:      Creates gsynth estimates and 95% CIs for EU15 and EU10 countries separately
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
set.seed(1234)
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


# Gsynth does not tolerate missing data
carbon.new <- carbon[is.na(carbon$loggdp)==FALSE & is.na(carbon$logUNemit)==FALSE,]


#################################################################################
# Plot ATT for EU15 countries
#################################################################################

e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new[carbon.new$eu15==1,], index=c("ctrysector","year"), force="two-way", se=TRUE, min.T0=7, nboots=1000, seed=1234)
e0

year <- seq(min(carbon.new$year), max(carbon.new$year),1)

# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ATT plot
p <- ggplot() +
  geom_ribbon(aes(x=year,ymin=(exp(e0$est.att[,3])-1)*100,ymax=(exp(e0$est.att[,4])-1)*100),fill=cols[1]) +
  geom_line(aes(x=year,y=(exp(e0$est.att[,1])-1)*100),color=cols[3],size=1.2) +
  labs(x="Year",y="Difference in CO2 emissions (percent)", title="ATT Estimates for EU ETS, 2008-2016", subtitle="Generalized synthetic control: EU15 Countries") +
  geom_vline(aes(xintercept=D), size=1.5) +
  geom_vline(aes(xintercept=2005)) +
  geom_hline(aes(yintercept=0),lty=2)+
  ylim(-50,40) +
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
p

ggsave(p,filename=paste("../Figures/si/robust_EU15.png",sep=""),width=6.5, height=6)



#################################################################################
# Plot ATT for EU10 countries
#################################################################################

e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new[carbon.new$eu10==1,], index=c("ctrysector","year"), force="two-way", se=TRUE, min.T0=7, nboots=1000, seed=1234)
e0

year <- seq(min(carbon.new$year), max(carbon.new$year),1)


# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ATT plot
p <- ggplot() +
  geom_ribbon(aes(x=year,ymin=(exp(e0$est.att[,3])-1)*100,ymax=(exp(e0$est.att[,4])-1)*100),fill=cols[1]) +
  geom_line(aes(x=year,y=(exp(e0$est.att[,1])-1)*100),color=cols[3],size=1.2) +
  labs(x="Year",y="Difference in CO2 emissions (percent)", title="ATT Estimates for EU ETS, 2008-2016", subtitle="Generalized synthetic control: EU10 Countries") +
  geom_vline(aes(xintercept=D), size=1.5) +
  geom_vline(aes(xintercept=2005)) +
  geom_hline(aes(yintercept=0),lty=2)+
  ylim(-50,40) +
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
p

ggsave(p,filename=paste("../Figures/si/robust_EU10.png",sep=""),width=6.5, height=6)



#################################################################################
#                                END OF FILE
#################################################################################