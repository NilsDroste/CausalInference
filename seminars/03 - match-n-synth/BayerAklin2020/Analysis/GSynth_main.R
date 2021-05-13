
#################################################################################
# File Name:  	GSynth_main.R 
# Project:			EU ETS effectiveness paper
# Purpose:      Creates estimates and 95% CIs for generalized synthetic control
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
load(here("BayerAklin2020/Data/ETS_analysis.RData"))

#################################################################################
# Assess goodness-of-match
#################################################################################

# Goodness-of-match analysis
temp <- carbon %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(unemit[carbon$year>=2005 & carbon$sector=="Total"], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)

# Goodness-of-match plot

library(reshape2)
library(ggplot2)

# Set up country counter and exclude Switzerland and Turkey as non-ETS countries
final.ex <- carbon
ctry <- unique(final.ex$country)

# Color settings: colorblind-friendly palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- final.ex[final.ex$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("etsemit","unemit"))
  plot.df <- plot.df[plot.df$sector!="Unregulated",]
  
  plot.df$group[is.na(plot.df$value)==FALSE & plot.df$variable=="etsemit"] <- 2
  #plot.df$group[plot.df$sector=="Total" & plot.df$variable=="unemit"] <- 1
  plot.df$group[plot.df$sector=="Regulated" & plot.df$variable=="unemit"] <- 1
  
  
  plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2),
                          labels=c("UN regulated","EU ETS"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="etsemit" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="etsemit" & plot.df$sector=="Regulated" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="unemit" & plot.df$sector=="Regulated" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1 
  
  clean <- plot.df[is.na(plot.df$value)==FALSE & is.na(plot.df$group)==FALSE,]
  
  p <- ggplot() +
    geom_line(data=clean,aes(x=year, y=value, group=group, color=group)) +
    labs(x="Year",y="CO2 Emissions",z="",title=paste("Emissions for",ctry[i])) + 
    #xlim(min(clean$year),max(clean$year)) +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
    scale_color_manual(values = cols) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 5)) +
    labs(color="Sectors") +
    annotate(geom='text',label=paste0("R^2==",m1),x=2013,y=Inf,vjust=2,hjust=0,parse=TRUE,cex=3) +
    annotate(geom='text',label=paste0("Mean==",m2),x=2013,y=Inf,vjust=4.5,hjust=0,parse=TRUE,cex=3)
  p
  ggsave(p,filename=paste("../Figures/goodness/",ctry[i],".png",sep=""),width=6.5, height=6)
}

R2.list


#################################################################################
# Main analysis
#################################################################################


# Keep only total carbon emissions data
total <- carbon[carbon$sector=="Total",]

# Restrict data to ETS regulated and unregulated emissions
carbon <- carbon[carbon$sector=="Regulated" | carbon$sector=="Unregulated",]
carbon <- carbon[with(carbon, order(carbon$country,carbon$year,carbon$sector)),]

# Treatment assignment
D <- 2008
D2005 <- 2005

# Create treatment indicator
carbon$ets <- 0
carbon$ets[carbon$sector=="Regulated" & carbon$year>=D] <- 1

carbon$ets2005 <- 0
carbon$ets2005[carbon$sector=="Regulated" & carbon$year>=D2005] <- 1

# Create indicator for regulated/unregulated fixed effects
carbon$ctrysector <- paste(carbon$countryname,carbon$sector)




#################################################################################
# Generalized synthetic control
#################################################################################

# Gsynth does not tolerate missing data
carbon.new <- carbon[is.na(carbon$logUNemit)==FALSE & is.na(carbon$loggdp)==FALSE,]

# Estimates for ETS 2008
e0 <- gsynth(logUNemit ~ ets + loggdp + loggdp2, data=carbon.new, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234)


# Main results
gsynth.mean <- (exp(e0$est.avg[1])-1)*100
gsynth.mean.ci <- c((exp(e0$est.avg[3])-1)*100,(exp(e0$est.avg[4])-1)*100)
GSynth <- c(gsynth.mean,gsynth.mean.ci)
names(GSynth) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth,1))

# Estimates for ETS 2005
e1 <- gsynth(logUNemit ~ ets2005 + loggdp + loggdp2, data=carbon.new, 
             index=c("ctrysector","year"),
             force="two-way", 
             se=TRUE, nboots=1000, seed=1234)

# Main results
gsynth.mean.2005 <- (exp(e1$est.avg[1])-1)*100
gsynth.mean.ci.2005 <- c((exp(e1$est.avg[3])-1)*100,(exp(e1$est.avg[4])-1)*100)
GSynth.2005 <- c(gsynth.mean.2005, gsynth.mean.ci.2005)
names(GSynth.2005) <- c("GSynth","2.5% Quantile","97.5% Quantile")
print(round(GSynth.2005,1))


#################################################################################
# Number of observations
#################################################################################


# Full sample without missings
dim(carbon.new)[1]

# For 2008
# Pre-treament number of observations
dim(carbon.new[carbon.new$year<2008,])[1]

# Post-treatment number of observations
dim(carbon.new)[1]-dim(carbon.new[carbon.new$year<2008,])[1]

# For 2005
# Pre-treament number of observations
dim(carbon.new[carbon.new$year<2005,])[1]

# Post-treatment number of observations
dim(carbon.new)[1]-dim(carbon.new[carbon.new$year<2005,])[1]


#################################################################################
# Main results plot
#################################################################################

year <- seq(min(carbon.new$year), max(carbon.new$year),1)


# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ATT plot
p <- ggplot() +
  geom_ribbon(aes(x=year,ymin=(exp(e0$est.att[,3])-1)*100,ymax=(exp(e0$est.att[,4])-1)*100),fill=cols[1]) +
  geom_line(aes(x=year,y=(exp(e0$est.att[,1])-1)*100),color=cols[3],size=1.2) +
  labs(x="Year",y="Difference in CO2 emissions (percent)", title="ATT Estimates for EU ETS, 2008-2016", subtitle="Generalized synthetic control") +
  geom_vline(aes(xintercept=D2005)) +
  geom_vline(aes(xintercept=D), size=1.5) +
  geom_hline(aes(yintercept=0),lty=2)+
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  ylim(-40,20) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
p

ggsave(p,filename=paste("../Figures/main/gsynth_main.pdf",sep=""),width=6.5, height=6)



#################################################################################
# Plot: Comparison of treated and counterfactual emission paths 
#################################################################################

# Average emission path

path.dt <- plot(e0,type="counterfactual")$data



p <- ggplot() +
  geom_line(aes(x=path.dt$time,y=path.dt$outcome,color=path.dt$type), size=1.2)+
  scale_color_manual(values=c(cols[2],"black"),
                     name="",
                     breaks=c("co", "tr"),
                     labels=c("Counterfactual average Y(0)", "Treated average Y(1)")) +
  labs(x="Year",y="Log CO2 emissions", title="Treated and Counterfactual Emission Paths", subtitle="Sample averages") +
  geom_vline(aes(xintercept=D2005)) +
  geom_vline(aes(xintercept=D), size=1.5) +
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"), legend.position=c(0.25,0.17), legend.background=element_rect("transparent"), legend.key = element_rect(colour = NA, fill = NA))
        
p

ggsave(p,filename="../Figures/main/main_y0y1.pdf",width=6.5, height=6)


#################################################################################
# Plots: Comparison of treated and counterfactual emission paths by country (Appendix)
#################################################################################

ctry <- e0$id.tr
ctry.clean <- str_remove(ctry, " Regulated")

OUT <- list()

for(i in (1:length(ctry))){

#i <- 1
path.dt <- plot(e0,type="counterfactual",id=ctry[i])$data

OUT[[i]] <- cbind(path.dt,ctry.clean[i])

p <- ggplot() +
  geom_line(aes(x=path.dt$time,y=path.dt$outcome,color=path.dt$type), size=1.2)+
  scale_color_manual(values=c(cols[2],"black"),
                     name="",
                     breaks=c("ct", "tr"),
                     labels=c("Counterfactual average Y(0)", "Treated average Y(1)")) +
  labs(x="Year",y="Log CO2 emissions", title="Treated and Counterfactual Emission Paths", subtitle=paste(ctry.clean[i],": Sample averages", sep="")) +
  geom_vline(aes(xintercept=D2005)) +
  geom_vline(aes(xintercept=D), size=1.5) +
  scale_x_continuous(breaks=seq(min(year), max(year), by=5)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"), legend.position="bottom")
p

ggsave(p,filename=paste("../Figures/bycountry/y0y1_",tolower(ctry.clean[i]),".png",sep=""),width=6.5, height=6)

}

# Warnings can be ignored as some countries have missing values for emissions in their initial years #


#################################################################################
# Plot: Treatment status (Appendix)
#################################################################################

# Color settings: colorblind-friendly palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


library(panelView)
p <- panelView(logUNemit~ets+loggdp+loggdp2, data=carbon, index=c("ctrysector","year"), pre.post=TRUE,
          xlab="Year", ylab="Country-sector",by.timing=TRUE,by.group=TRUE,axis.lab.gap=c(4,0),background="white",color=c(cols[1],cols[2],cols[3],cols[6]),cex.axis=7,cex.legend=7, cex.lab=10)

ggsave(p,file ="../Figures/si/treatment_status.png",width=6.5,height=6)
        
         
#################################################################################
# Calculate emissions reductions by country
#################################################################################

library(plyr)

# Get emissions data
emit.dt <- ldply(OUT, data.frame)
colnames(emit.dt) <- c("year","emit","type","country")

# Function for calculating difference of Y(1)-Y(0)
mydiff <- function(x) {
  emit.dt <- x
  exp(emit.dt$emit[emit.dt$type=="tr"]) - exp(emit.dt$emit[emit.dt$type=="ct"])
}

# Calculate differences by country and year
byctry.diff <- by(emit.dt[emit.dt$year>=D,],emit.dt$country[emit.dt$year>=D],FUN=function(x) mydiff(x),simplify=TRUE)

# Create data frame
df <- ldply(byctry.diff, data.frame)
df$year <- rep(seq(D,max(year),1),length(ctry.clean))   

colnames(df) <- c("country", "diff", "year")

# Calculate the cumalative amount of emissions saved
emit.diff <- aggregate(diff~country, data=df, FUN=sum)
emit.reg <- aggregate(unemit~country, data=carbon[carbon$sector=="Regulated" & carbon$year>=2008,], FUN=sum)
colnames(emit.reg) <- c("country", "reg")
emit.total <- aggregate(unemit~country, data=total[total$sector=="Total" & total$year>=2008,], FUN=sum)   
colnames(emit.total) <- c("country", "total")


full <- join_all(list(emit.diff,emit.reg,emit.total), by="country")
full$pct.reg <- (round(full$diff/full$reg,3)*100)
full$pct.total <- (round(full$diff/full$total,3)*100)

# Produce output table
library(xtable)
full.tab <- full
full.tab[,c(2:4)] <- round(full.tab[,c(2:4)]/1000000,1)
colnames(full.tab) <- c("Country", "Difference", "Regulated", "Total", "Regulated (percent)", "Total (percent)")
eu.sum <- c("Total (EU25)", round(sum(full.tab[,2]),1), round(sum(full.tab[,3]),1), round(sum(full.tab[,4],1)), round(sum(full.tab[,2])/sum(full.tab[,3]),3)*100, round(sum(full.tab[,2])/sum(full.tab[,4]),3)*100)
                                                                                              
full.tab <- rbind(full.tab,eu.sum)
full.tab[,2:6] <- lapply(full.tab[,2:6], function(x) as.numeric(as.character(x)))

# print(xtable(full.tab, type="latex", digits=c(0,0,1,1,1,1,1)), file="./reduction.tex", booktabs=TRUE, hline.after=c(-1,0,25,nrow(full.tab)))

# Produce bar plot showing total emission reductions by country, Y(1)-Y(0) cumulative 2008-2016
p <- ggplot() +
     geom_bar(data=full, aes(x=country, y=diff/1000000),stat="identity") +
     labs(x="Country", y="Cumulative difference in million tons CO2 emissions", title="Difference in Treated and Counterfactual Emissions, 2008-2016", subtitle="Cumulative difference in million tons CO2 emissions by country") + 
     theme(axis.text.x=element_text(angle=90, hjust=1))
p              

ggsave(p,file ="../Figures/si/reductions_diff.png",width=6.5,height=6)

# Produce point plot showing percent emission reductions by country, Y(1)-Y(0) cumulative 2008-2016
p <- ggplot() +
  geom_point(data=full, aes(x=country, y=pct.reg),stat="identity") +
  labs(x="Country", y="Cumulative difference in CO2 emissions as\n percentage of regulated emissions", title="Difference in Treated and Counterfactual Emissions, 2008-2016", subtitle="Cumulative difference in CO2 emissions relative to regulated emissions") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_hline(aes(yintercept=0),lty=2) +
  geom_hline(aes(yintercept=as.numeric(full.tab[26,5])),lty=1,color=cols[2], size=1.2)+
  annotate(geom="text", x=23, y=-4, label=paste("EU Total:",as.numeric(full.tab[26,5]),"%"), color=cols[2],cex=3.5)
p 
              
ggsave(p,file ="../Figures/si/reductions_regpercent.png",width=6.5,height=6)


# Produce point plot showing percent emission reductions by country, Y(1)-Y(0) cumulative 2008-2016
p <- ggplot() +
  geom_point(data=full, aes(x=country, y=pct.total),stat="identity") +
  labs(x="Country", y="Cumulative difference in CO2 emissions as\n percentage of total emissions", title="Difference in Treated and Counterfactual Emissions, 2008-2016", subtitle="Cumulative difference in CO2 emissions relative to total emissions") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_hline(aes(yintercept=0),lty=2) +
  geom_hline(aes(yintercept=as.numeric(full.tab[26,6])),lty=1,color=cols[2], size=1.2)+
  annotate(geom="text", x=23, y=-2, label=paste("EU Total:",as.numeric(full.tab[26,6]),"%"), color=cols[2],cex=3.5)
p 


ggsave(p,file ="../Figures/si/reductions_totalpercent.png",width=6.5,height=6)


#################################################################################
# Raw data by country (Appendix)
#################################################################################

library(reshape2)
library(ggplot2)

# Color settings: colorblind-friendly palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ctry <- unique(carbon$country)

raw <- carbon[carbon$sector!="Total",]
raw <- raw[raw$year>2004 & raw$year<2017,]
levels(raw$sector)[levels(raw$sector)=="Regulated"] <- "EU ETS"
levels(raw$sector)[levels(raw$sector)=="Unregulated"] <- "non EU ETS"

for(i in (1:length(ctry))){

p <- ggplot() +
  geom_line(data=raw[raw$country==ctry[i],],aes(x=year, y=unemit,group=sector, color=sector), size=1.2) +
  labs(x="Year",y="CO2 Emissions",z="",title=paste("Raw Emissions Data for",ctry[i])) +  
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"), legend.position="bottom") +
  scale_color_manual(values = cols[2:3], name="") +
  scale_y_continuous(labels = scales::comma, limits=c(0,max(raw$unemit[raw$country==ctry[i]])))

ggsave(p,filename=paste("../Figures/bycountry/raw_",tolower(ctry[i]),".png",sep=""),width=6.5, height=6)

}



#################################################################################
#                                END OF FILE
#################################################################################