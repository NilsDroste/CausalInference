library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)
library(plm)
library(AER)



# Load data -----

smoke <- read_dta(here("R", "BN05", "Binder Smoke.dta"))
so2 <- read_dta(here("R", "BN05", "Binder SO2.dta"))
spm <- read_dta(here("R", "BN05", "Binder spm.dta"))


# Table 2 (twoway) fixed effects (without instruments) ----

mod1 <- lm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2) 
summary(mod1, vcov = vcovHC(mod1,"HC1"))

mod2 <- plm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data = so2)
summary(mod2, vcov=vcovHC(mod2,method = "arellano"))

mod4 <- lm(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + lngdpcu + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year), data=spm) 
mod4 %>% summary(vcov=vcovHC(mod4,"HC1"))

mod5 <- plm(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data = spm)
summary(mod5, vcov=vcovHC(mod5,method = "arellano"))


# TASK 1 reproduce model 3 from table 2, leave the code below, don't forget to store it in an appropriate object, e.g. mod3
# here are the variables used for the regression: 
# dependent variable: lnsmoke
# independent variables: lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + areaunkn + indus + residential + cityunkn + centcity + lndens + coast + as.factor(year)
# try both a standard OLS with lm(), and a fixed effects model with plm()
# finish by summarizing the results and checking whether the results in the paper have been acurately reproduced




# Table  ----

ivmod1 <- ivreg(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) | lningopc + lningoparticip + + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2)
summary(ivmod1,cov=vcovHC(ivmod1,"HC1"))

ivmod2 <- plm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year) | lningopc + lningoparticip + + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data=so2)
summary(ivmod2, vcov=vcovHC(ivmod2,method = "arellano"))

ivmod4 <-
  ivreg(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + lngdpcu + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year) |
      . - lnengopc + ingoparticip + lnengopc72, data= spm)
summary(ivmod4,cov=vcovHC(ivmod1,"HC1"))

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

# # what if we excluded those values and reran the regression?
# let us respecify the log transformation

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

# Task 2 plot mod1_new and discuss whether the model quality has improved, you may also use information from the model summary()

