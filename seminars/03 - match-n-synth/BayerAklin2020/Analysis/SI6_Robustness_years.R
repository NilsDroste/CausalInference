
#################################################################################
# File Name:  	Robustness_years.R
# Project:			EU ETS effectiveness paper
# Purpose:      Creates gsynth estimates and 95% CIs for different years of treatment assignment
# Data input:   ../Data/ETS_analysis.RData
# Output File: 		
# Author: 		  Patrick Bayer
# Date:         12 April 2020
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
# Treatment window of +/-5 years around 2008
D <- 2008
window.size <- 5
window <- seq(D-window.size,D+window.size,1)


# Loop through different years of treatment assignment
for(i in (1:length(window))){

#i <- 1
# Create treatment indicator
carbon$ets <- 0
carbon$ets[carbon$sector=="Regulated" & carbon$year>=window[i]] <- 1

# Create indicator for regulated/unregulated fixed effects
carbon$ctrysector <- paste(carbon$countryname,carbon$sector)


#################################################################################
# Generalized synthetic control
#################################################################################

# Gsynth does not tolerate data with missings
carbon.new <- carbon[is.na(carbon$loggdp)==FALSE & is.na(carbon$logUNemit)==FALSE,]

e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new, index=c("ctrysector","year"), force="two-way", se=TRUE, min.T0=7, nboots=1000, seed=1234)
e0

# #################################################################################
# # Plot ATT over time for different treatment years
# #################################################################################

year <- seq(min(carbon.new$year), max(carbon.new$year),1)

# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ATT plot
p <- ggplot() +
  geom_ribbon(aes(x=year,ymin=(exp(e0$est.att[,3])-1)*100,ymax=(exp(e0$est.att[,4])-1)*100),fill=cols[1]) +
  geom_line(aes(x=year,y=(exp(e0$est.att[,1])-1)*100),color=cols[3],size=1.2) +
  labs(x="Year",y="Difference in CO2 emissions (percent)", title="ATT Estimates for EU ETS, 2008-2016", subtitle=paste("Placebo test: Treatment assignments in year",window[i])) +
  geom_vline(aes(xintercept=window[i])) +
  geom_vline(aes(xintercept=D), size=1.5) +
  geom_hline(aes(yintercept=0),lty=2)+
  ylim(-40,20) +
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
p

ggsave(p,filename=paste("../Figures/si/treat_",window[i],".png",sep=""),width=6.5, height=6)
}


#################################################################################
#                                END OF FILE
#################################################################################