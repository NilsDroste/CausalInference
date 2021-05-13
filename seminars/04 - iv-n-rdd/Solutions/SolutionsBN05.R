library(tidyverse)
library(here)
library(haven)
library(lmtest)
library(sandwich)
library(plm)
library(AER)


# Load data -----

smoke <- read_dta(here("BinderNeumayer2005", "Binder Smoke.dta"))
so2 <- read_dta(here("BinderNeumayer2005", "Binder SO2.dta"))
spm <- read_dta(here("BinderNeumayer2005", "Binder spm.dta"))


# TODO: TASK: Estimate Task Table 3 with instruments 

# by hand

ivmod1_1stage <- lm(lnengopc ~ lningopc + lningoparticip + lnengopc72 + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2,na.action=na.exclude)

Fstage_preds <- predict(ivmod1_1stage)

ivmod1_2ndstage <- lm(lnso2med ~ Fstage_preds + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2)

summary(ivmod1_2ndstage)


# Tip, use AER:: for non-panel estimates and plm:: (or fixest::)

ivmod1 <- ivreg(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) | lningopc + lningoparticip + lnengopc72 + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2)
summary(ivmod1)

# Tests
# on aspects of the IV (i.e. first stage)
first_stage <- lm(lnengopc ~ lningopc + lningoparticip + lnengopc72 + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2, na.action=na.exclude)

# F-test for validity
instrFtest <- waldtest(first_stage,.~.-lningopc-lningoparticip)
instrFtest # we clearly reject the NULL of irrelevance

# testing for exogeneity
# Hausman-Wu test
# adding in the residuals from first stage into second stage
Hausman_reg <- lm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) + residuals(first_stage), data=so2)
summary(Hausman_reg)
HausWutest <- waldtest(Hausman_reg,.~.-residuals(first_stage))
HausWutest # we clearly reject endogeneity

# there also is a Sargan test for instrument validity, see e.g.
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R or
# https://www.r-bloggers.com/2013/09/detecting-weak-instruments-in-r/ for the  Stock and Yogo (2005) approach

# or, as easy as using inbuild diagnostics of the AER package
summary(ivmod1, vcov = sandwich, diagnostics = TRUE)

# continuing the replication with these easy tests where possible, but you are welcome to dig deeper.


ivmod2 <- plm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) | lningopc + lningoparticip + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data=so2)
summary(ivmod2, vcov=vcovHC(ivmod2,method = "arellano"))

ivmod3 <- ivreg( lnsmoke ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + areaunkn + indus + residential + cityunkn + centcity + lndens + coast + as.factor(year) | . - lnengopc + ingoparticip + lnengopc72, data = smoke)
summary(ivmod3, vcov = sandwich, diagnostics = TRUE)

ivmod4 <-
  ivreg(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + lngdpcu + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year) |
          . - lnengopc + ingoparticip + lnengopc72, data= spm)
summary(ivmod4, vcov = sandwich, diagnostics = TRUE)

ivmod5 <-
  plm(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + lngdpcu + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year) |
        . - lnengopc + ingoparticip + lnengopc72, index = c("ctrcode"), effect = "individual" , model = "random", data = spm)
summary(ivmod5, vcov=vcovHC(ivmod5,method = "arellano")) # this fails.
# check https://stackoverflow.com/questions/11404141/problems-with-within-and-random-models-in-plm-package
# one potential failure is ill specified data.


# So, lets assess the data and the models graphically ----
par(mfrow=c(2,2))
plot(mod1)

# Is that a good fit?

# let's check what caused the strange pattern
pairs(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + year, data = so2)

# what do you think it is?
so2 %>% group_by(ctrcode) %>% ggplot(aes(y= lnso2med, x = year, color=ctrcode)) + geom_point()

# let us check what the min values are
which.min(so2$lnso2med)

# so what is the minimum value? 
min(so2$lnso2med)

# that is logaritmized so let us check out the original value
min(so2$so2med)

# let us again plot 
so2 %>% group_by(ctrcode) %>% ggplot(aes(y= so2med, x = year, color=ctrcode)) + geom_point()

which(so2$so2med==0) # this gives you the row numbers where so2med is equal to 0
# let us check which countries these values are from
so2 %>% filter(so2med==0) %>% select(ctrcode) %>% table()

# # what if we transformed those values and reran the regression?
# let us respecify with  ateslog transformation

so2_new <- so2 %>% mutate(lnso2med_new = log1p(so2med)) 

# and plot again
so2_new %>% group_by(ctrcode) %>% ggplot(aes(y= lnso2med_new, x = year, color=ctrcode)) + geom_point()

so2_new %>% group_by(ctrcode) %>% ggplot(aes(x=lnso2med_new, color=ctrcode)) + geom_density()

# so let us check, what if we re-run the analysis? 

mod1_new <- lm(lnso2med_new ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2_new) 
summary(mod1_new, vcov = vcovHC(mod1_new,"HC1"))

mod2_new <- plm(lnso2med_new ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data = so2_new)
summary(mod2_new, vcov=vcovHC(mod2_new,method = "arellano"))

ivmod1_new <- ivreg(lnso2med_new ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) | lningopc + lningoparticip + + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2_new)
summary(ivmod1_new,cov=vcovHC(ivmod1_new,"HC1"))

ivmod2_new <- plm(lnso2med_new ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) | lningopc + lningoparticip + + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data=so2_new)
summary(ivmod2_new, vcov=vcovHC(ivmod2_new,method = "arellano"))

# plot mod1_new and discuss whether the model quality has improved, you may also use information from the model summary()

par(mfrow=c(2,2))
plot(mod1_new)

