#source dependent code here
library(Matching)
#need:
#Balance5.R
#caclBy.R
#countif.R
#mech function multi impute.R

#mount data
costa <- read.csv(file="YOUR DIRECTORY/2000_mydata_mech_12_13.csv")


#Misc
#get rid of Hitoy Cerrera
costa$vis_07 <- ifelse(costa$vis_07 == 9999, 0, costa$vis_07)
costa$pov2 <-  costa$clpovindex.00^2
costa$for2 <-  costa$for60per^2
costa$luc1 <-  costa$luc123per^2
costa$luc2 <-  costa$luc4per^2
costa$luc3 <-  costa$luc567per^2
costa$rv2 <-   costa$rdlesskm^2
costa$dmct2 <- costa$dist.mcitykm^2
costa$d.rv.69.91 <- costa$d.rv.69.91/1000000000
costa$all.schl <- costa$num.col + costa$num.schl

#entrances (which should include Arenal) plus Guayabo and Cabo Blanco
costa$entov.gu <- ifelse((costa$entrance == 1 | costa$park.id == 3 | costa$park.id == 4 | costa$park.id == 36) & costa$prot_1 == 1,1,0)

#matching covariates
covs <- c("for60per", "luc123per", "luc4per", "luc567per", "rdlesskm", "dist.mcitykm", "clpovindex.73")
covs1 <- costa[covs]

#Summary statistics by protection status 
summaryBy(for60per ~ prot_1, data=costa, FUN=c(mean, median, sd, range))
summaryBy(luc123per ~ prot_1, data=costa, FUN=c(mean, median, sd, range))
summaryBy(luc4per ~ prot_1, data=costa, FUN=c(mean, median, sd, range))
summaryBy(luc567per ~ prot_1, data=costa, FUN=c(mean, median, sd, range))
summaryBy(rdlesskm ~ prot_1, data=costa, FUN=c(mean, median, sd, range))
summaryBy(dist.mcitykm ~ prot_1, data=costa, FUN=c(mean, median, sd, range))
summaryBy(clpovindex.73 ~ prot_1, data=costa, FUN=c(mean, median, sd, range))

#primary matching specification
m1 <- Match(Tr=costa$prot_1, Y=costa$clpovindex.00, X=covs1, BiasAdjust=T, Var.calc=1, Weight=2)
summary(m1)

#My balance function
bal1 <- balance(costa, m1, Y="clpovindex.00", "prot_1", covs, matchout=TRUE, impute=T)
#Balance Table
bal1$b
#Pull the matched set of units from the Balance function results
matched <- bal1$match.set

#Create subsets of treated and matched control units
matched.t <- subset(matched, prot_1 == 1)
matched.c <- subset(matched, prot_1 == 0)

#effect size
2.39/sd(matched.c$clpovindex.00)
#####################################################################################################

###############################################################################################################################################
#MAIN SPECIFICATION
#entrances (which should include Arenal) plus Guayabo and Cabo Blanco
costa$entov.gu <- ifelse((costa$entrance == 1 | costa$park.id == 3 | costa$park.id == 4 | costa$park.id == 36) & costa$prot_1 == 1,1,0)

#ENTRANCES and visitation with other mechs (MAIN ANALYSIS)
tour <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91 + d.pct.for.60.86 + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="entov.gu", beta=T, unique="codseg00")

#ENTRANCES and visitation without other mechs
tour <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="entov.gu", beta=T, unique="codseg00")


#LNATE
t <- countif("prot_1==1 & entov.gu == 0", data=matched.t, store=T)
mean(t$clpovindex.00)- mean(t$yhat)
#imples that LMATT=-2.137



############################################################################################################################################################

#Roadless Volume with other mechanisms with mechanism imputation (MAIN ANALYSIS)
rv <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91 + d.pct.for.60.86 + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.rv.69.91", beta=T, unique="codseg00", impute.mech=T)
								 
#Roadless Volume without other mechanisms with imputation
rv <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.rv.69.91", beta=T, unique="codseg00", impute.mech=T)
							 
#############################################################################################################################################################
#Roadless Volume with other mechanisms using entrance and visitation without imputing the Roadless volume portion of the mech analysis (this is not the main analysis)
rv <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91 + d.pct.for.60.86 + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.rv.69.91", beta=T, unique="codseg00", impute.mech=F)

#Roadless Volume without other mechanisms without imputation
rv <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.rv.69.91", beta=T, unique="codseg00")


##############################################################################################################################################
#change in forest cover 60-86 with other mechanisms and mechanism imputation (MAIN ANALYSIS)
forest <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91 + entov.gu + d.pct.for.60.86,
							 data=costa, match=NULL, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.pct.for.60.86", BiasAdjust=T, Weight=2, betas=T, unique="codseg00", impute.mech=T)

#change in forest cover 60-86 with other mechanisms no mechanism imputation
forest <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rv.69.91 + entov.gu + d.pct.for.60.86,
							 data=costa, match=NULL, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.pct.for.60.86", BiasAdjust=T, Weight=2, betas=T, unique="codseg00", impute.mech=F)


#change in forest cover 60-86 without other mechanisms with imputation
forest <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.pct.for.60.86,
							 data=costa, match=NULL, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.pct.for.60.86", BiasAdjust=T, Weight=2, betas=T, unique="codseg00", impute.mech=T)

#change in forest cover 60-86 without other mechanisms without imputation
forest <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.pct.for.60.86,
							 data=costa, match=NULL, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.pct.for.60.86", BiasAdjust=T, Weight=2, betas=T, unique="codseg00", impute.mech=F)


################################################################


#      THE REST OF THIS CODE IS FOR VARIOUS ROBUSTNESS CHECKS






################################################################################################################
#OTHER PROXIES FOR INFRASTRUCTURE DEVELOPMENT
#distance to road  with other mechanisms
rd <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + d.rd.69.91 + d.pct.for.60.86 + entrance,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="d.rd.69.91", beta=T, unique="codseg00")

#number of clinics with other mechanisms
clin <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + num.clin + d.pct.for.60.86 + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="num.clin", beta=T, unique="codseg00")

#upper bound estimate on clinics
#set all control clinics to 0 
costa$num.clin.c <- ifelse(costa$prot_1==0, 0, costa$num.clin)

#number of clinics UPPER BOUND 	with other mechanisms
clin <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + num.clin.c + d.pct.for.60.86 + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="num.clin.c", beta=T, unique="codseg00")

#number of schools and colegios with other mechanisms
schools <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + all.schl + d.pct.for.60.86 + entov.gu,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="all.schl", beta=T, unique="codseg00")

#upper bound estimate on cloegios
#set all control clinics to 0 
costa$num.col.c <- ifelse(costa$prot_1==0, 0, costa$num.col)

#number of colegios UPPER BOUND with other mechanisms
col <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + num.col.c + d.pct.for.60.86 + entrance,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="num.col.c", beta=T, unique="codseg00")

#number of schools with other mechanisms
schl <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + num.schl + d.pct.for.60.86 + entrance,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="num.schl", beta=T, unique="codseg00")

#upper bound estimate on schools
#set all control clinics to 0 
costa$num.schl.c <- ifelse(costa$prot_1==0, 0, costa$num.schl)

#number of schools with other mechanisms
#add schools and coegios together
schl <- mech(model=clpovindex.00 ~  clpovindex.73 + for60per + luc123per + luc4per + luc567per + rdlesskm + dist.mcitykm + num.schl.c + d.pct.for.60.86 + entrance,
								 data=costa, match=m1, Y="clpovindex.00", Tr="prot_1", covs=covs, mech="num.schl.c", beta=T, unique="codseg00")


#################################################################################################################################

#LSEM for the RV mechanism
#First (redundant equation) to estimate ATE
a <- lm(clpovindex.00 ~ prot_1, matched)
summary(a)

#with controls
a.c <- lm(clpovindex.00 ~ prot_1 + as.matrix(matched[names.m]), matched)
summary(a.c)

#second equation to check for effect of treatment on RV
b.rv <- lm(d.rv.69.91 ~ prot_1, matched)
summary(b.rv)

#second equation with controls
b.rv.c <- lm(d.rv.69.91 ~ prot_1 + clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm + rdlesskm , matched)
summary(b.rv.c)

#third equation with treatment and mechanism on outcome
c.rv <- lm(clpovindex.00 ~ prot_1 + d.rv.69.91, matched)
summary(c.rv)

#third equation with controls
c.rv.c <- lm(clpovindex.00 ~ prot_1 + d.rv.69.91 +  clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm +rdlesskm, matched)
summary(c.rv.c)

#Mechanism effect without controls and without interaction
m.rv1 <- b.rv$coeff[2] * c.rv.c$coeff[3]
m.rv1

m.rv2 <- b.rv.c$coeff[2] * c.rv.c$coeff[3]
m.rv2

###Now test it against the mediate Function

rv.mediate <-  mediate(b.rv, c.rv, sims=500, treat="prot_1", mediator="d.rv.69.91")
summary(rv.mediate)

rv.mediate.c <-  mediate(b.rv.c, c.rv.c, sims=500, treat="prot_1", mediator="d.rv.69.91")
summary(rv.mediate.c)

#LSEM for the entrance mechanism

#second equation to check for effect of treatment on RV
b.ent <- lm(entov.gu ~ prot_1, matched)
summary(b.ent)

#second equation with controls
b.ent.c <- lm(entov.gu ~ prot_1 + clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm , matched)
summary(b.ent.c)

#third equation with treatment and mechanism on outcome
c.ent <- lm(clpovindex.00 ~ prot_1 + entov.gu, matched)
summary(c.ent)

#third equation with controls
c.ent.c <- lm(clpovindex.00 ~ prot_1 + entov.gu +  clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm +rdlesskm, matched)
summary(c.ent.c)

#Mechanism effect without controls and without interaction
m.ent1 <- b.ent$coeff[2] * c.ent.c$coeff[3]
m.ent1

m.ent2 <- b.ent.c$coeff[2] * c.ent.c$coeff[3]
m.ent2

###Now test it against the mediate Function

ent.mediate <-  mediate(b.ent.c, c.ent.c, sims=500, treat="prot_1", mediator="entov.gu")
summary(ent.mediate)

#sensitivity
sens.ent <- medsens(ent.mediate, rho=.01)

plot(sens.ent, "rho", sign.prod="positive")
#LSEM for the forest cover mechanism

#second equation to check for effect of treatment on RV
b.for <- lm(d.pct.for.60.86 ~ prot_1, matched)
summary(b.for)

#second equation with controls
b.for.c <- lm(d.pct.for.60.86 ~ prot_1 + clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm + rdlesskm , matched)
summary(b.for.c)

#third equation with treatment and mechanism on outcome
c.for <- lm(clpovindex.00 ~ prot_1 + d.pct.for.60.86, matched)
summary(c.for)

#third equation with controls
c.for.c <- lm(clpovindex.00 ~ prot_1 + d.pct.for.60.86 +  clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm +rdlesskm, matched)
summary(c.for.c)

#Mechanism effect without controls and without interaction
m.for1 <- b.for$coeff[2] * c.for.c$coeff[3]
m.for1

m.for2 <- b.for.c$coeff[2] * c.for.c$coeff[3]
m.for2

###Now test it against the mediate Function

for.mediate <-  mediate(b.for, c.for.c, sims=500, treat="prot_1", mediator="d.pct.for.60.86")
summary(for.mediate)

#sensitivity
sens.for <- medsens(for.mediate, rho=.01)
summary(sens.for)

plot(sens.for, "rho", sign.prod="positive")

#LSEM for the visitation mechanism

#second equation to check for effect of treatment on RV
b.vis <- lm(vis_07_tot ~ prot_1, matched)
summary(b.vis)

#second equation with controls
b.vis.c <- lm(vis_07_tot ~ prot_1 + clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm + rdlesskm , matched)
summary(b.vis.c)

#third equation with treatment and mechanism on outcome
c.vis <- lm(clpovindex.00 ~ prot_1 + vis_07_tot, matched)
summary(c.vis)

#third equation with controls
c.vis.c <- lm(clpovindex.00 ~ prot_1 + vis_07_tot +  clpovindex.73 + for60per + luc123per + luc4per + luc567per + dist.mcitykm +rdlesskm, matched)
summary(c.vis.c)

#Mechanism effect without controls and without interaction
m.vis1 <- b.vis$coeff[2] * c.vis.c$coeff[3]
m.vis1

m.vis2 <- b.vis.c$coeff[2] * c.vis.c$coeff[3]
m.vis2

###Now test it against the mediate Function

vis.mediate <-  mediate(b.vis, c.vis.c, sims=500, treat="prot_1", mediator="vis_07_tot")
summary(vis.mediate)

#sensitivity
sens.vis <- medsens(vis.mediate, rho=.01)
summary(sens.vis)

plot(sens.ent, "rho", sign.prod="positive")







###############################################################################################
#Check for migration effects 


#on population density
#matching covariates
covs <- c("for60per", "luc123per", "luc4per", "luc567per", "rdlesskm", "dist.mcitykm", "clpovindex.73", "popden73")
covs1 <- costa[covs]


mm1 <- Match(Tr=costa$entov.gu, Y=costa$popden2000, X=covs1, BiasAdjust=T, Var.calc=1, Weight=2)
summary(mm1)

#on population 
#matching covariates
covs <- c("for60per", "luc123per", "luc4per", "luc567per", "rdlesskm", "dist.mcitykm", "clpovindex.73", "pobtot1973")
covs1 <- costa[covs]


mm1 <- Match(Tr=costa$entov.gu, Y=costa$popgrowth, X=covs1, BiasAdjust=T, Var.calc=1, Weight=2)
summary(mm1)
