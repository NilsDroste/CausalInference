library(tidyverse)
library(plm)

# get data ----
df <- haven::read_dta("./data/PES_data_public.dta")

# start analysis -----
# subsetting
df <- df %>% filter(sample_gps==1)

# tidying the data according to tidy data principles

# Each variable must have its own column.
# Each observation must have its own row.
# Each value must have its own cell.

# see https://cfss.uchicago.edu/notes/tidy-data/ 

df_tidy <- df %>% mutate(before = base_fcover_vil, after = end_fcover_vil, treatstat_vil = ifelse(treatstat_vil=="Control", 0, 1)) %>% select(partid, before, after, treatstat_vil, dist_road, l1_area, subcounty, village, photo1991_act, photo2011_act)  %>% pivot_longer(c(before, after), names_to = "time", values_to = "fcover_vil") %>% mutate(treatstat_vil= ifelse(treatstat_vil == 1 & time == "before", 0, treatstat_vil)) %>% select(partid, village, subcounty, time, treatstat_vil, fcover_vil, dist_road, l1_area, photo1991_act, photo2011_act) %>% arrange(partid,village,subcounty,time)

# now we can run a first-difference model in plm, which will basically omit all time-constant variables, and thus account for time-constant difference across individuals, but will compute the effect of being treated on the change in forest cover.

# This is a more tidy approach, in my view. It is also robust to spatio-temporal autocorrelations if standard errors are estimated by the vcovSCC function as developed by Discoll and Kraay.

fd_mod1 <- plm(fcover_vil ~ treatstat_vil + dist_road + l1_area + photo1991_act + photo2011_act, index = c("partid", "time"), effect = "individual", model = "fd", data = df_tidy)

summary(fd_mod1, vcov=vcovSCC(fd_mod1, type="sss", cluster="group"))
plot(fd_mod1)