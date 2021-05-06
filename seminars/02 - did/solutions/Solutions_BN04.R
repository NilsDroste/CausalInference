library(tidyverse)
library(here)
library(haven)
library(lmtest)
library(sandwich)
library(plm)


# Load data -----

smoke <- read_dta(here("BinderNeumayer2005", "Binder Smoke.dta"))
so2 <- read_dta(here("BinderNeumayer2005", "Binder SO2.dta"))
spm <- read_dta(here("BinderNeumayer2005", "Binder spm.dta"))


# Table 2 (twoway) fixed effects (without instruments) ----

mod1 <- lm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), data=so2) 
summary(mod1, vcov = vcovHC(mod1,"HC1"))

mod2 <- plm(lnso2med ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + area + indust + residential + lndens + cencity + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data = so2)
summary(mod2, vcov=vcovHC(mod2,method = "arellano"))

mod4 <- lm(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + lngdpcu + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year), data=spm) 
mod4 %>% summary(vcov=vcovHC(mod4,"HC1"))

mod5 <- plm(lnspm ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + resid + cityunkn + centcity + lndens + coast + as.factor(year), index = c("ctrcode"), effect = "individual" , model = "random", data = spm)
summary(mod5, vcov=vcovHC(mod5,method = "arellano"))


# TODO TASK: reproduce model 3 from table 2, leave the code below, don't forget to store it in an appropriate object, e.g. mod3
# try both a standard OLS with lm(), and a fixed effects model with plm(), try to find out why the second may not be running and suggest a fix

mod3_lm <- lm(lnsmoke ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + areaunkn + indus + residential + cityunkn + centcity + lndens + coast + as.factor(year), data=smoke)
mod3_lm %>% summary(vcov=vcovHC(mod3_lm,"HC1"))

mod3_plm <- plm(lnsmoke ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + areaunkn + indus + residential + cityunkn + centcity + lndens + coast + as.factor(year), effect = "individual" , model = "random", data=smoke)
# table(index(smoke), useNA = "ifany")

smoke %>% select(ctrcode, year) %>% table() # sometimes multiple measurements for one year.

# let us try a multi-level model 
library(lme4)
library(lmerTest)
smoke_plus <- smoke %>% group_by(ctrcode, year) %>% summarise(avg_lnsmoke = mean(lnsmoke, na.rm = T)) %>% right_join(smoke)
mod3_multilev <- lmer(lnsmoke ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + centcity + lndens + coast + areaunkn + indus + residential + cityunkn + (1|ctrcode:year), data = smoke_plus, REML = T)
summary(mod3_multilev)
# with group averages to approximate the demeaned fixed effect approach
mod3_multilev_avg <- lmer(lnsmoke ~ lnengopc + lnenergy + lngdp + lngdpsq + polity + lnliter + centcity + lndens + coast + areaunkn + indus + residential + cityunkn + (1 + avg_lnsmoke |ctrcode:year), data = smoke_plus, REML =F)
summary(mod3_multilev_avg)
# which has a singular fit, meaning an overfitted model, see: https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models




