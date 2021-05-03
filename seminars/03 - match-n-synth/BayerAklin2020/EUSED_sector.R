
#################################################################################
# FILE INFORMATION
# File Name:  	EUSED_sector.R
# Purpose:      Creates EUSED data from UN and EU ETS data by sector
# 
# Data input:   ./data
# Output File: 	EUSED_sector.csv
# Last update:  27 September 2019
#
# TECHNICAL DISCLAIMER
# R version 3.5.3 (2019-03-11) using RStudio Version 1.1.453 on Windows 10, 64 bit
#	Intel(R) Core(TM) i7-4650U CPU @ 1.70Ghz 2.30Ghz with 8GB RAM
#
# FUNDING INFORMATION
# Funder:       British Academy Small Grant, SG171349
# Grant title:  The Effectiveness of Carbon Markets in Europe, 2005-2012
# PI:           Patrick Bayer, University of Glasgow
# Email:        patrick.bayer@glasgow.ac.uk
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
# Create separate data frames for different UN sectors
#################################################################################

# 1 (Energy)
UN_energy <- un[un$Sector_code=="1.A.1",]
UN_energy$sector <- "Energy"
colnames(UN_energy)[colnames(UN_energy)=="emissions"] <- "UN"
colnames(UN_energy)[colnames(UN_energy)=="Country_code"] <- "country"
colnames(UN_energy)[colnames(UN_energy)=="Year"] <- "year"
UN_energy <- UN_energy[,c("country", "year", "sector", "UN")]

# 2 (Metals)
UN_metals <- un[un$Sector_code=="2.C" | un$Sector_code=="1.A.2.a" | un$Sector_code=="1.A.2.b",]
UN_metals$sector <- "Metals"
colnames(UN_metals)[colnames(UN_metals)=="emissions"] <- "UN"
colnames(UN_metals)[colnames(UN_metals)=="Country_code"] <- "country"
colnames(UN_metals)[colnames(UN_metals)=="Year"] <- "year"
UN_metals <- UN_metals[,c("country", "year", "sector", "UN")]
UN_metals <- aggregate(UN~year + country + sector, UN_metals, sum)

# 3 (Minerals)
UN_minerals <- un[un$Sector_code=="2.A" | un$Sector_code=="1.A.2.f",]
UN_minerals$sector <- "Minerals"
colnames(UN_minerals)[colnames(UN_minerals)=="emissions"] <- "UN"
colnames(UN_minerals)[colnames(UN_minerals)=="Country_code"] <- "country"
colnames(UN_minerals)[colnames(UN_minerals)=="Year"] <- "year"
UN_minerals <- UN_minerals[,c("country", "year", "sector", "UN")]
UN_minerals <- aggregate(UN~year + country + sector, UN_minerals, sum)

# 4 (Chemicals)
UN_chemicals <- un[un$Sector_code=="2.B" | un$Sector_code=="1.A.2.c",]
UN_chemicals$sector <- "Chemicals"
colnames(UN_chemicals)[colnames(UN_chemicals)=="emissions"] <- "UN"
colnames(UN_chemicals)[colnames(UN_chemicals)=="Country_code"] <- "country"
colnames(UN_chemicals)[colnames(UN_chemicals)=="Year"] <- "year"
UN_chemicals <- UN_chemicals[,c("country", "year", "sector", "UN")]
UN_chemicals <- aggregate(UN~year + country + sector, UN_chemicals, sum)

# 5 (Paper)
UN_paper <- un[un$Sector_code=="1.A.2.d",]
UN_paper$sector <- "Paper"
colnames(UN_paper)[colnames(UN_paper)=="emissions"] <- "UN"
colnames(UN_paper)[colnames(UN_paper)=="Country_code"] <- "country"
colnames(UN_paper)[colnames(UN_paper)=="Year"] <- "year"
UN_paper <- UN_paper[,c("country", "year", "sector", "UN")]


# Total emissions (w/o LULUCF and w/o indirect CO2)
UN_total <- un[un$Sector_code=="Sectors/Totals_excl_excl",]
UN_total$sector <- "Total"
colnames(UN_total)[colnames(UN_total)=="emissions"] <- "UN"
colnames(UN_total)[colnames(UN_total)=="Country_code"] <- "country"
colnames(UN_total)[colnames(UN_total)=="Year"] <- "year"
UN_total <- UN_total[,c("country", "year", "sector", "UN")]


# Transport emissions
UN_transport <- un[un$Sector_code=="1.A.3.b",]
UN_transport$sector <- "Transport"
colnames(UN_transport)[colnames(UN_transport)=="emissions"] <- "UN"
colnames(UN_transport)[colnames(UN_transport)=="Country_code"] <- "country"
colnames(UN_transport)[colnames(UN_transport)=="Year"] <- "year"
UN_transport <- UN_transport[,c("country", "year", "sector", "UN")]




#################################################################################
# Create final UN data
#################################################################################

UN_ <- rbind(UN_energy, UN_metals, UN_minerals, UN_chemicals, UN_paper, UN_total)

# Calculate residuals by merged columns
agg <- within(merge(aggregate(UN ~ country + year, data = subset(UN_, sector!='Total'), sum),
                    aggregate(UN ~ country + year, data = subset(UN_, sector=='Total'), sum),
                    by=c("country", "year")),
              {UN <- UN.y - UN.x
              sector = 'Residual'})

# Create new full data frame
UN_df <- rbind(subset(UN_, sector!='Total'),
                  agg[c("country", "year", "sector", "UN")],
                  subset(UN_, sector=='Total'))

UN_df <- rbind(UN_df,UN_transport)

# Order data frame
UN_df <- with(UN_df, UN_df[order(country, year, as.character(sector)),])
row.names(UN_df) <- NULL



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

# Only keep annual emission information and drop totals
ets <- ets[ets$year!="Total 1st trading period (05-07)",]
ets <- ets[ets$year!="Total 2nd trading period (08-12)",]
ets <- ets[ets$year!="Total 3rd trading period (13-20)",]
ets$year <- as.numeric(ets$year)

# Order dataset
ets <- ets[with(ets, order(ets$country,ets$year)),]
ets <- subset(ets, select=c(country_code,main.activity.sector.name,value,year))
colnames(ets) <- c("country", "sector", "ETS", "year")

# Fix double spaces in main ETS data
ets$sector[ets$sector=="21  Refining of mineral oil"] <- c("21 Refining of mineral oil")
ets$sector[ets$sector=="22  Production of coke"] <- c("22 Production of coke")
ets$sector[ets$sector=="24  Production of pig iron or steel"] <- c("24 Production of pig iron or steel")
ets$sector[ets$sector=="10  Aviation"] <- c("10 Aviation")


#################################################################################
# Create separate data frames for different ETS sectoral categories
#################################################################################

# Notes:
# Correspondence of sectors from UNFCCC to EU ETS is taken from 'EU GHG Inventory May 2018.pdf', Table 1.10
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14


# 1 (Energy)
ETS_energy <- ets[ets$sector=="20 Combustion of fuels" | ets$sector=="21 Refining of mineral oil", ]
ETS_energy$sector <- "Energy"
ETS_energy <- ETS_energy[,c("country", "year", "sector", "ETS")]
ETS_energy <- aggregate(ETS~year + country + sector, ETS_energy, sum)

# 2 (Metals)
ETS_metals <- ets[ets$sector=="22 Production of coke" | ets$sector=="23 Metal ore roasting or sintering" | ets$sector== "24 Production of pig iron or steel" | ets$sector== "25 Production or processing of ferrous metals" | ets$sector== "26 Production of primary aluminium" | ets$sector== "27 Production of secondary aluminium" | ets$sector== "28 Production or processing of non-ferrous metals" ,]
ETS_metals$sector <- "Metals"
ETS_metals <- ETS_metals[,c("country", "year", "sector", "ETS")]
ETS_metals <- aggregate(ETS~year + country + sector, ETS_metals, sum)

# 3 (Minerals)
ETS_minerals <- ets[ets$sector=="29 Production of cement clinker" | ets$sector=="30 Production of lime, or calcination of dolomite/magnesite" | ets$sector=="31 Manufacture of glass" | ets$sector=="32 Manufacture of ceramics" | ets$sector=="33 Manufacture of mineral wool" | ets$sector=="34 Production or processing of gypsum or plasterboard",]
ETS_minerals$sector <- "Minerals"
ETS_minerals <- ETS_minerals[,c("country", "year", "sector", "ETS")]
ETS_minerals <- aggregate(ETS~year + country + sector, ETS_minerals, sum)

# 4 (Chemicals)
ETS_chemicals <- ets[ets$sector=="37 Production of carbon black" | ets$sector=="38 Production of nitric acid" | ets$sector=="39 Production of adipic acid" | ets$sector=="40 Production of glyoxal and glyoxylic acid" | ets$sector=="41 Production of ammonia"| ets$sector=="42 Production of bulk chemicals" | ets$sector=="43 Production of hydrogen and synthesis gas" | ets$sector=="44 Production of soda ash and sodium bicarbonate",]
ETS_chemicals$sector <- "Chemicals"
ETS_chemicals <- ETS_chemicals[,c("country", "year", "sector", "ETS")]
ETS_chemicals <- aggregate(ETS~year + country + sector, ETS_chemicals, sum)

# 5 (Paper)
ETS_paper <- ets[ets$sector=="35 Production of pulp" | ets$sector== "36 Production of paper or cardboard",]
ETS_paper$sector <- "Paper"
ETS_paper <- ETS_paper[,c("country", "year", "sector", "ETS")]
ETS_paper <- aggregate(ETS~year + country + sector, ETS_paper, sum)

# Totals
ETS_total <- ets[ets$sector=="20-99 All stationary installations",]
ETS_total$sector <- "Total"
ETS_total <- ETS_total[,c("country", "year", "sector", "ETS")]


#################################################################################
# Create final EU ETS data
#################################################################################

ETS_ <- rbind(ETS_energy, ETS_metals, ETS_minerals, ETS_chemicals, ETS_paper, ETS_total)

# Calculate residuals by merged columns
agg_ets <- within(merge(aggregate(ETS ~ country + year, data = subset(ETS_, sector!='Total'), sum),
                    aggregate(ETS ~ country + year, data = subset(ETS_, sector=='Total'), sum),
                    by=c("country", "year")),
              {ETS <- ETS.y - ETS.x
              sector = 'Residual'})

# Bind data frame
ETS_df <- rbind(subset(ETS_, sector!='Total'),
               agg_ets[c("country", "year", "sector", "ETS")],
               subset(ETS_, sector=='Total'))

# Order data frame and reset row titles
ETS_df <- with(ETS_df, ETS_df[order(country, year, as.character(sector)),])
row.names(ETS_df) <- NULL


# Check if sums are correct
new <- ETS_df[ETS_df$sector=="Total",]
new.s <- aggregate(ETS~country+year, data=ETS_df[ETS_df$sector!="Total",],FUN=sum)
new.s <- new.s[with(new.s, order(new.s$country,new.s$year)),]

# // Should be all `TRUE' //
table(new$ETS==new.s$ETS)

# // Should be all 'zero' //
summary(new$ETS-new.s$ETS)
test <- merge(new,new.s, by=c("country","year"))
test$diff <- test$ETS.x-test$ETS.y

# // Should be all 'zero' //
summary(test$diff)


#################################################################################
# Merge UN and EU ETS data for EUSED_sector data
#################################################################################

# Merge data into single data frame
merge.df <- merge(UN_df, ETS_df, by=c("country", "year", "sector"), all=TRUE)

# Expand data frame to complete panel
final <- complete(merge.df, country, year, sector)

# Construct value labels
final$sector[final$sector=="Energy"] <- 1
final$sector[final$sector=="Metals"] <- 2
final$sector[final$sector=="Minerals"] <- 3
final$sector[final$sector=="Chemicals"] <- 4
final$sector[final$sector=="Paper"] <- 5
final$sector[final$sector=="Residual"] <- 6
final$sector[final$sector=="Transport"] <- 7
final$sector[final$sector=="Total"] <- 8


final$sector <- factor(final$sector, 
                        levels=seq(1,8,1),
                        labels=c("Energy", "Metals", "Minerals", "Chemicals", "Paper", "Residual", "Transport", "Total"))

final <- final[with(final, order(final$country, final$year, final$sector)),]

# Final touches
colnames(final)[colnames(final)=="country"] <- c("iso2")
colnames(final)[colnames(final)=="UN"] <- c("un")
colnames(final)[colnames(final)=="ETS"] <- c("ets")

final$country <- countrycode(final$iso2, 'iso2c', 'country.name')

# Ratio variable of emissions coverage by country-year
final$ratio <- final$ets / final$un

eused_sector <- final


# Produce output files
write.csv(eused_sector, file="./eused/EUSED_sector.csv", row.names=FALSE)
save(eused_sector, file="./eused/EUSED_sector.RData")



#################################################################################
# Produce by-country, by-sector plots
#################################################################################

library(reshape2)
library(ggplot2)

# Set up country counter and exclude Switzerland and Turkey as non-ETS countries
final.ex <- final[final$country!="Switzerland" & final$country!="Turkey",]

# Color settings: colorblind-friendly palette
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#################################################################################
# Sector: Energy
#################################################################################

# Notes:
# Correspondence of sectors from UNFCCC to EU ETS is taken from 'EU GHG Inventory May 2018.pdf', Table 1.10
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14
# Sectors are also mapped to ensure a close match
# UN 1.A.1; EU ETS 20/21


# Plots for energy sector
energy <- final.ex[final.ex$sector=="Energy",]
ctry <- unique(energy$country)

# Exclude countries with no emissions from given sector
ctry <- unique(energy$country)
ctry.check <- aggregate(data=energy, ets~country, FUN=sum)
ctry <- ctry[ctry %in% ctry.check[,1]]


# Goodness-of-match analysis (Information for Table 3 in the Codebook)
temp <- energy %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(un[energy$year>=2005], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)

R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- energy[energy$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("ets","un"))
  
  plot.df$group[plot.df$variable=="ets"] <- 2
  plot.df$group[plot.df$variable=="un"] <- 1
  
  plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2),
                          labels=c("UN 1.A.1","EU ETS 20/21"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="ets" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="ets" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="un" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1
  
  clean <- plot.df[is.na(plot.df$value)==FALSE,]
  
  p <- ggplot() +
    geom_line(data=clean,aes(x=year, y=value, group=group, color=group)) +
    labs(x="Year",y="CO2 Emissions",z="",title=paste("Emissions for",ctry[i]),subtitle=paste("Sector:",unique(clean$sector))) + 
    #xlim(min(clean$year),max(clean$year)) +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
    scale_color_manual(values = c(cols[2],cols[3])) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 5)) +
    labs(color="Sectors") +
    annotate(geom='text',label=paste0("R^2==",m1),x=2013,y=Inf,vjust=2,hjust=0,parse=TRUE,cex=3) +
    annotate(geom='text',label=paste0("Mean==",m2),x=2013,y=Inf,vjust=4.5,hjust=0,parse=TRUE,cex=3)
  p
  ggsave(p,filename=paste("./figures_energy/",ctry[i],".png",sep=""),width=6.5, height=6)
}

mean(R2.list)

#################################################################################
# Sector: Metals
#################################################################################

# Notes:
# Correspondence of sectors from UNFCCC to EU ETS is taken from 'EU GHG Inventory May 2018.pdf', Table 1.10
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14
# Sectors are also mapped to ensure a close match
# UN 2.C, 1.A.2.a/b; EU ETS 22-28


# Plots for metals sector
metals <- final.ex[final.ex$sector=="Metals",]

# Exclude countries with no emissions from given sector
ctry <- unique(metals$country)
ctry.check <- aggregate(data=metals, ets~country, FUN=sum)
ctry <- ctry[ctry %in% ctry.check[,1]]


# Goodness-of-match analysis (Information for Table 2 in the Codebook)
temp <- metals %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(un[metals$year>=2005], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)


R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- metals[metals$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("ets","un"))
  
  plot.df$group[plot.df$variable=="ets"] <- 2
  plot.df$group[plot.df$variable=="un"] <- 1
  
  plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2),
                          labels=c("UN 2.C+1.A.2.a/b","EU ETS 22-28"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="ets" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="ets" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="un" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1
  
  clean <- plot.df[is.na(plot.df$value)==FALSE,]
  
  p <- ggplot() +
    geom_line(data=clean,aes(x=year, y=value, group=group, color=group)) +
    labs(x="Year",y="CO2 Emissions",z="",title=paste("Emissions for",ctry[i]),subtitle=paste("Sector:",unique(clean$sector))) + 
    #xlim(min(clean$year),max(clean$year)) +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
    scale_color_manual(values = c(cols[2],cols[3])) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 5)) +
    labs(color="Sectors") +
    annotate(geom='text',label=paste0("R^2==",m1),x=2013,y=Inf,vjust=2,hjust=0,parse=TRUE,cex=3) +
    annotate(geom='text',label=paste0("Mean==",m2),x=2013,y=Inf,vjust=4.5,hjust=0,parse=TRUE,cex=3)
  p
  ggsave(p,filename=paste("./figures_metals/",ctry[i],".png",sep=""),width=6.5, height=6)
}

mean(R2.list)

#################################################################################
# Sector: Minerals
#################################################################################

# Notes:
# Correspondence of sectors from UNFCCC to EU ETS is taken from 'EU GHG Inventory May 2018.pdf', Table 1.10
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14
# Sectors are also mapped to ensure a close match
# UN 2.A, 1.A.2.f; EU ETS 29-34


# Plots for metals sector
minerals <- final.ex[final.ex$sector=="Minerals",]

# Exclude countries with no emissions from given sector
ctry <- unique(minerals$country)
ctry.check <- aggregate(data=minerals, ets~country, FUN=sum)
ctry <- ctry[ctry %in% ctry.check[,1]]

# Goodness-of-match analysis (Information for Table 3 in the Codebook)
temp <- minerals %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(un[minerals$year>=2005], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)

R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- minerals[minerals$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("ets","un"))
  
  plot.df$group[plot.df$variable=="ets"] <- 2
  plot.df$group[plot.df$variable=="un"] <- 1
  
  plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2),
                          labels=c("UN 2.A+1.A.2.f","EU ETS 29-34"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="ets" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="ets" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="un" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1
  
  clean <- plot.df[is.na(plot.df$value)==FALSE,]
  
  p <- ggplot() +
    geom_line(data=clean,aes(x=year, y=value, group=group, color=group)) +
    labs(x="Year",y="CO2 Emissions",z="",title=paste("Emissions for",ctry[i]),subtitle=paste("Sector:",unique(clean$sector))) + 
    #xlim(min(clean$year),max(clean$year)) +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
    scale_color_manual(values = c(cols[2],cols[3])) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 5)) +
    labs(color="Sectors") +
    annotate(geom='text',label=paste0("R^2==",m1),x=2013,y=Inf,vjust=2,hjust=0,parse=TRUE,cex=3) +
    annotate(geom='text',label=paste0("Mean==",m2),x=2013,y=Inf,vjust=4.5,hjust=0,parse=TRUE,cex=3)
  p
  ggsave(p,filename=paste("./figures_minerals/",ctry[i],".png",sep=""),width=6.5, height=6)
}

mean(R2.list)

#################################################################################
# Sector: Chemicals
#################################################################################

# Notes:
# Correspondence of sectors from UNFCCC to EU ETS is taken from 'EU GHG Inventory May 2018.pdf', Table 1.10
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14
# Sectors are also mapped to ensure a close match
# UN 2.B, 1.A.2.c; EU ETS 21; 37-44


# Plots for chemicals sector
chemicals <- final.ex[final.ex$sector=="Chemicals",]

# Exclude countries with no emissions from given sector
ctry <- unique(chemicals$country)
ctry.check <- aggregate(data=chemicals, ets~country, FUN=sum)
ctry <- ctry[ctry %in% ctry.check[,1]]

# Goodness-of-match analysis (Information for Table 3 in the Codebook)
temp <- chemicals %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(un[chemicals$year>=2005], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)

R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- chemicals[chemicals$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("ets","un"))
  
  plot.df$group[plot.df$variable=="ets"] <- 2
  plot.df$group[plot.df$variable=="un"] <- 1
  
  plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2),
                          labels=c("UN 2.B+1.A.2.c","EU ETS 21; 37-44"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="ets" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="ets" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="un" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1
  
  clean <- plot.df[is.na(plot.df$value)==FALSE,]
  
  p <- ggplot() +
    geom_line(data=clean,aes(x=year, y=value, group=group, color=group)) +
    labs(x="Year",y="CO2 Emissions",z="",title=paste("Emissions for",ctry[i]),subtitle=paste("Sector:",unique(clean$sector))) + 
    #xlim(min(clean$year),max(clean$year)) +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
    scale_color_manual(values = c(cols[2],cols[3])) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 5)) +
    labs(color="Sectors") +
    annotate(geom='text',label=paste0("R^2==",m1),x=2013,y=Inf,vjust=2,hjust=0,parse=TRUE,cex=3) +
    annotate(geom='text',label=paste0("Mean==",m2),x=2013,y=Inf,vjust=4.5,hjust=0,parse=TRUE,cex=3)
  p
  ggsave(p,filename=paste("./figures_chemicals/",ctry[i],".png",sep=""),width=6.5, height=6)
}

mean(R2.list)


#################################################################################
# Sector: Paper
#################################################################################

# Notes:
# Correspondence of sectors from UNFCCC to EU ETS is taken from 'EU GHG Inventory May 2018.pdf', Table 1.10
# https://www.eea.europa.eu/data-and-maps/data/national-emissions-reported-to-the-unfccc-and-to-the-eu-greenhouse-gas-monitoring-mechanism-14
# Sectors are also mapped to ensure a close match
# UN 1.A.2.d; EU ETS 35+36


# Plots for paper sector
paper <- final.ex[final.ex$sector=="Paper",]

# Exlude Sweden for 2015 because of data reporting problem
paper <- paper[paper$iso2!="EE" & paper$iso2!="DE" & paper$iso2!="SE",]

# Exclude countries with no emissions from given sector
ctry <- unique(paper$country)
ctry.check <- aggregate(data=paper, ets~country, FUN=sum)
ctry <- ctry[ctry %in% ctry.check[,1]]

# Goodness-of-match analysis (Information for Table 3 in the Codebook)
temp <- paper %>%
  group_by(country) %>%
  summarise(ratio.byc = mean(ratio, na.rm=TRUE),
            emit.total= sum(un[paper$year>=2005], na.rm=TRUE))

print(temp, n=Inf)
summary(temp$ratio.byc)

weighted.mean(temp$ratio.byc,temp$emit.total, na.rm=TRUE)

R2.list <- rep(NA,length(ctry))

for(i in (1:length(ctry))){
  
  plot.df <- paper[paper$country==ctry[i],]
  plot.df <- melt(plot.df, id.var=c("country", "year", "sector"), measure.var=c("ets","un"))
  
  plot.df$group[plot.df$variable=="ets"] <- 2
  plot.df$group[plot.df$variable=="un"] <- 1
  
  plot.df$group <- factor(plot.df$group, 
                          levels=c(1,2),
                          labels=c("UN 1.A.2.d","EU ETS 35/36"))
  
  ets.initial <- min(plot.df$year[plot.df$variable=="ets" & is.na(plot.df$value)==FALSE])
  y <- plot.df$value[plot.df$variable=="ets" & plot.df$year>=ets.initial]
  x <- plot.df$value[plot.df$variable=="un" & plot.df$year>=ets.initial]
  m1 <- round(summary(lm(y~x-1))$r.squared,3)
  m2 <- round(mean(y/x,na.rm=TRUE),2)
  R2.list[i] <- m1
  
  clean <- plot.df[is.na(plot.df$value)==FALSE,]
  
  p <- ggplot() +
    geom_line(data=clean,aes(x=year, y=value, group=group, color=group)) +
    labs(x="Year",y="CO2 Emissions",z="",title=paste("Emissions for",ctry[i]),subtitle=paste("Sector:",unique(clean$sector))) + 
    #xlim(min(clean$year),max(clean$year)) +
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
    scale_color_manual(values = c(cols[2],cols[3])) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 5)) +
    labs(color="Sectors") +
    annotate(geom='text',label=paste0("R^2==",m1),x=2013,y=Inf,vjust=2,hjust=0,parse=TRUE,cex=3) +
    annotate(geom='text',label=paste0("Mean==",m2),x=2013,y=Inf,vjust=4.5,hjust=0,parse=TRUE,cex=3)
  p
  ggsave(p,filename=paste("./figures_paper/",ctry[i],".png",sep=""),width=6.5, height=6)
}

mean(R2.list)

#################################################################################
#                                END OF FILE
#################################################################################


