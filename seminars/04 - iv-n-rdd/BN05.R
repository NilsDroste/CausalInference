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


# TODO: TASK: Estimate Task Table 3 with instruments ----

# Tip, use AER:: for non-panel estimates and plm:: (or fixest::)




# TODO: Did you get model 5 to work? Why do you think that is?


