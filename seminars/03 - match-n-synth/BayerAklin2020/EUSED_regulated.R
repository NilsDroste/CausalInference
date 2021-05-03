
#################################################################################
# FILE INFORMATION
# File Name:  	EUSED_regulated.R
# Purpose:      Creates EUSED data from UN and EU ETS data by regulated/unregulated emissions 
# 
# Data input:   ./data
# Output File: 	EUSED_simple.csv
# Last update:  27 September 2019
#
# TECHNICAL DISCLAIMER
# R version 3.6.1 (2019-07-05) using RStudio Version 1.2.1335 on Windows 10, 64 bit
#	Intel(R) Core(TM) i7-4650U CPU @ 1.70Ghz 2.30Ghz with 8GB RAM
#
# FUNDING INFORMATION
# Funder:       British Academy Small Grant, SG171349
# Grant title:  The Effectiveness of Carbon Markets in Europe, 2005-2012
# PI:           Patrick Bayer, University of Glasgow
# Email:        patrick.bayer@strath.ac.uk
# Note:         Constantin Brod provided excellent research assistance 

# CITATION
# When using the EUSED data, please cite the following paper in which the data was initially used
# The data and paper is available from my website www.patrickbayer.com/publications/
# CITE PAPER
#################################################################################

library(foreign)
library(countrycode)
library(dplyr)
library(tidyr)

#################################################################################
# Load, clean, and prepare UNFCCC data
#################################################################################

# Notes:
# UNFCCC data (version 21, created 31/5/2018)
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14#tab-european-data

# Load data
un <- read.csv("./data/UNFCCC_v21.csv",as.is=TRUE)

# Restrict data to 'CO2' emissions and post 1990 years
table(un$Pollutant_name)
un <- un[un$Pollutant_name=="CO2",]
un <- un[un$Year!="1985-1987",]
un$Year <- as.numeric(as.character(un$Year))
un <- un[un$Year>=1990,]

# Correct UK country label
un$Country[un$Country=="United Kingdom (Convention)"] <- c("United Kingdom")
un$Country_code[un$Country_code=="UK"] <- c("GB")

# Drop EU aggregates
un <- un[un$Country!="EU28 (Convention)",]
un <- un[un$Country!="EU (KP)",]

# Convert emissions from Gg to mt
un$emissions <- un$emissions*1000

# Order dataframe
un <- un[with(un, order(un$Country,un$Year,un$Sector_name)),]



#################################################################################
# Create UN data that distinguishes between EU ETS-regulated and unregulated emissions
#################################################################################

# Notes:
# Construct a mapping of UN sectors to EU ETS sectors based on official guidance
# EU ETS data (version 31, created 11/9/2018)
# https://www.eea.europa.eu/data-and-maps/data/european-union-emissions-trading-scheme-7#tab-european-data

# ETS sector 45 (Capture of GHGs under Directive 2009/31/EC) does not have a clear UN corresponding sector. See comments in UN document: "Capture of emissions would be reported under the respective inventory sector e.g. 1.A.1.a Public electricity and heat production."
#
# ETS sector 99 (Other activity opted-in under Art 24 of EU Directive) does not have a clear UN corresponding sector. See comments in UN document: "CRF category depending on type of activity opted-in."

# The mapping from UN to EU ETS sectors is the same as the one used in the by-sector breakdown
# Energy: 1.A.1 (UN) --- 20/21 (EU ETS)
# Metals: 1.A.2.a; 1.A.2.b; 2.C (UN) --- 22-28 (EU ETS)
# Minerals: 1.A.2.f; 2.A (UN) --- 29-34 (EU ETS)
# Chemicals: 1.A.2.c; 2.B (UN) --- 37-44 (EU ETS)
# Paper: 1.A.2.d (UN) --- 35/36 (EU ETS)


reg.sec <- list("1.A.1", "1.A.2.a", "1.A.2.b", "1.A.2.c", "1.A.2.d", "1.A.2.f", "2.A", "2.B", "2.C")        


# Create a new "regulated" variable that is coded as follows:
# regulated=1 if sector is regulated under EU ETS
# regulated=0 for country total emissions
# regulated=NA o/w
un$regulated <- NA
un[un$Sector_code %in% unlist(reg.sec), "regulated"] <- 1
un[un$Sector_code=="Sectors/Totals_excl_excl", "regulated"] <- 0

# Create aggregated dataset
un.new <- aggregate(un$emissions,by=list(un$Country,un$Year,un$regulated),FUN=sum)

# Impose column names
colnames(un.new) <- c("country","year","sector","emissions")

un.new <- un.new[with(un.new, order(un.new$country,un.new$year,un.new$sector)),]

# Check that total emissions are larger than regulated emissions
# // Should only return 'TRUE' //
table(un.new$emissions[un.new$sector==0]>un.new$emissions[un.new$sector==1])


# Calculate unregulated emissions and add to dataframe
diff.unreg <- un.new$emissions[un.new$sector==0]-un.new$emissions[un.new$sector==1]
ctry.name <- unique(un.new$country)
year <- unique(un.new$year)

unreg <- data.frame(sort(rep(ctry.name,length(year))),rep(year,length(ctry.name)),2,diff.unreg)
colnames(unreg) <- colnames(un.new)
un.new <- as.data.frame(rbind(un.new,unreg))
un.new <- un.new[with(un.new, order(un.new$country,un.new$year,un.new$sector)),]


# Impose value labels
un.new$sector <- factor(un.new$sector, 
                         levels=c(0,1,2),
                         labels=c("Total","Regulated","Unregulated"))

# Check that total emissions add up
# // Should only return 'TRUE' //
total.check <- round(un.new$emissions[un.new$sector=="Total"],0)
sum.check <- round(un.new$emissions[un.new$sector=="Regulated"]+un.new$emissions[un.new$sector=="Unregulated"],0)


# Add iso codes to UN data
un.new$iso2 <- countrycode(un.new$country, 'country.name', 'iso2c')
colnames(un.new)[colnames(un.new)=="emissions"] <- "UN"


#################################################################################
# Load, clean, and prepare EU ETS data
#################################################################################

# Notes:
# EU ETS data (version 31, created 11/9/2018)
# https://www.eea.europa.eu/data-and-maps/data/european-union-emissions-trading-scheme-7#tab-european-data

# Load data
EUETS <- read.delim("./data/ETS_Database_v31.csv",as.is=TRUE)

# Only keep verified emissions from stationary sources
ets <- EUETS[EUETS$ETS.information=="2. Verified emissions",]
ets <- ets[ets$main.activity.sector.name=="20-99 All stationary installations",]

# Only keep annual emission information and drop totals
ets <- ets[ets$year!="Total 1st trading period (05-07)",]
ets <- ets[ets$year!="Total 2nd trading period (08-12)",]
ets <- ets[ets$year!="Total 3rd trading period (13-20)",]
ets$year <- as.numeric(ets$year)

# Order dataset
ets <- ets[with(ets, order(ets$country,ets$year)),]
ets <- subset(ets, select=c(country,country_code,value,year))
colnames(ets) <- c("country", "iso2", "ETS", "year")
ets$sector <- "Regulated"


#################################################################################
# Create combined data set
#################################################################################

# Merge data sets
merge.df <- merge(un.new, ets, by=c("iso2", "year", "sector"), all=TRUE, suffixes=c("",".y"))
merge.df$country[is.na(merge.df$country)==TRUE] <- merge.df$country.y[is.na(merge.df$country)==TRUE]
merge.df <- merge.df[,colnames(merge.df)!="country.y"]  


# Expand data frame to complete panel
final <- complete(merge.df, country, year, sector)
final$iso2[final$year==2017] <- final$iso2[final$year==2016]

# Final touches
colnames(final)[colnames(final)=="UN"] <- c("un")
colnames(final)[colnames(final)=="ETS"] <- c("ets")

# Create goodness-of-match variable
final$ratio <- final$ets / final$un


eused_regulated <- final

# Produce output files
write.csv(eused_regulated, file="./eused/EUSED_regulated.csv", row.names=FALSE)
save(eused_regulated, file="./eused/EUSED_regulated.RData")


# Goodness-of-match analysis (Information for Table 2 in the Codebook)
temp <- final %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(un[final$year>=2005 & final$sector=="Total"], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)



#################################################################################
# Produce by-country plots
#################################################################################

library(reshape2)
library(ggplot2)

# Set up country counter and exclude Switzerland and Turkey as non-ETS countries
final.ex <- final[final$country!="Switzerland" & final$country!="Turkey",]
ctry <- unique(final.ex$country)

# Color settings: colorblind-friendly palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- final.ex[final.ex$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("ets","un"))
  plot.df <- plot.df[plot.df$sector!="Unregulated",]
  
  plot.df$group[is.na(plot.df$value)==FALSE & plot.df$variable=="ets"] <- 3
  plot.df$group[plot.df$sector=="Total" & plot.df$variable=="un"] <- 1
  plot.df$group[plot.df$sector=="Regulated" & plot.df$variable=="un"] <- 2
  
  
 plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2,3),
                          labels=c("UN total","UN regulated","EU ETS"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="ets" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="ets" & plot.df$sector=="Regulated" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="un" & plot.df$sector=="Regulated" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1 
    
  clean <- plot.df[is.na(plot.df$value)==FALSE,]
  
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
  ggsave(p,filename=paste("./figures_regulated/",ctry[i],".png",sep=""),width=6.5, height=6)
}

R2.list


#################################################################################
#                                 END OF FILE
#################################################################################
