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


