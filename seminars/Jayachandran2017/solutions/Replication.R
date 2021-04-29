#________________________________________________
# Replication Seminar I: Randomized Controlled Trials
# Reproduction of main results from Jayachandran
# et al. 2017. Cash for Carbon. Science 357
# doi: https://doi.org/10.1126/science.aan0568 
# Script Authors: YOUR NAME
#________________________________________________

# if not done already, we'd need to install
# software packages that we will need for analysis
# now, we don't so I leave this uncommented.

# install.packages("tidyverse")

# load packages ----
library(tidyverse)
library(lmtest)
library(multiwayvcov)
library(ggpubr)


# get data ----
df <- haven::read_dta("./data/PES_data_public.dta")

# start analysis -----

# preparatory subsetting 
df <- df %>% filter(sample_gps==1)

############################################################################
# Task 1

# calculate difference in forest cover per village (between end and base dates) by adding a new variable to the data set
# e.g. we can do so by using the mutate() function and call it "change_fcover_vil" 
# additionally let us also compute a logarithmized change in forest cover (e.g. log_change_fcover_vil)

df <- df %>% mutate(
  change_fcover_vil = end_fcover_vil - base_fcover_vil,
  log_change_fcover_vil = log(end_fcover_vil) - log(base_fcover_vil)
)


############################################################################
# Task 2
# run linear regression for model 1 in table 3
tab3_mod1 <- lm(change_fcover_vil~treatstat_vil
                # + num_PFOs_vil # not found
                # + inc_pc_vil # not found
                + dist_road
                + l1_area
                + as.factor(subcounty), 
                df
)
summary(tab3_mod1, vcov=function(x) cluster.vcov(tab3_mod1, df[, "village"]))
# We have now approximately reproduced model one from table 3 in Jayachandran et al. 2017.
# The effect of paying private forest owners is the coefficient of the treamentstat_vil variable 

# To calculate the average tree cover change for the control group we can compute
df %>% filter(treatstat_vil=="Control") %>% summarise(MeanForestCoverChange = weighted.mean(change_fcover_vil, SPct_Data_vil))
# this is not exact and has to do with the weighted computation of an average value in the orginal stata file.


# What does the coefficient for the treatment from our first linear regression imply?

# ANSWER: that is the treatment effect (DiD estimator). Why is this not the canonical DiD, because we estimate the effect of treatment on the change (!) in forest cover (first difference) making the treatment dummy effect the DiD estimate. If this is still incomprehensible, tomorrows lecture should hopefully remedy this. 

############################################################################
# Task 3

# estimate model 2 from table 3 in Jayachandran, too.

tab3_mod2 <- lm(change_fcover_vil~treatstat_vil
                # + num_PFOs_vil # not found
                # + inc_pc_vil # not found
                + dist_road
                + l1_area
                + photo1991_act
                + photo2011_act
                # + pfo_satdate_* # not found
                + as.factor(subcounty),
                df
)
summary(tab3_mod2, vcov=function(x) cluster.vcov(tab3_mod2, df[, "village"]))

############################################################################
# Task 4

# estimate model 3

# it uses a logarithmized value of the forest cover change variable

tab3_mod3 <- lm(log_change_fcover_vil~treatstat_vil
                # + num_PFOs_vil # not found
                # + inc_pc_vil # not found
                + dist_road
                + l1_area
                + photo1991_act
                + photo2011_act
                # + pfo_satdate_* # not found
                + as.factor(subcounty),
                df
)
summary(tab3_mod3, vcov=function(x) cluster.vcov(tab3_mod3, df[, "village"]))


############################################################################
# Task 4

# visualize the results

# plot the data in box plot
plot1 <- ggboxplot(df, x = "treatstat_vil", y = "change_fcover_vil",
               color = "treatstat_vil", palette = "lancet",
               add = "jitter", title = "Effect of PES on forest cover",
               xlab = "Treatment status", ylab = "Change in forest cover")
#  Add p-value
plot1 + stat_compare_means(method = "t.test") + rremove("legend")

# calculate a t-test to compare the means
t.test(change_fcover_vil~treatstat_vil, data = df)

# Why are the means in the t-test different from the coefficient
# in the simple OLS regressions?

# Additional plots 
# plotting a density plot
ggdens <- ggdensity(df, x = "change_fcover_vil",
                    add = "mean", rug = TRUE,
                    color = "treatstat_vil", fill = "treatstat_vil",
                    palette = "lancet")

# plotting a histogram
gghist <- gghistogram(df, x = "change_fcover_vil",
                      add = "mean", rug = TRUE,
                      color = "treatstat_vil", fill = "treatstat_vil",
                      palette = "lancet", bins=100)

cowplot::plot_grid(ggdens, gghist, nrow = 1)

###########################################################################
# GROUP DISCUSSION: 
# What can be a conclusion from our regressions, the paper, and the graphs



############################################################################
# ADD-ON: Let us discuss the model quality based on analytic plots
par(mfrow=c(3,4))
plot(tab3_mod1, main = "model 1")
plot(tab3_mod2, main = "model 2")
plot(tab3_mod3, main = "model 3")


# DONE
