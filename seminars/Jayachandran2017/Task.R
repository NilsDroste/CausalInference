#________________________________________________
# Seminar II: Randomized Controlled Trials
# Reproduction of main results from Jayachandran
# et al. 2017. Cash for Carbon. Science 357
# doi: https://doi.org/10.1126/science.aan0568 
# Script Authors: YOUR NAME
#________________________________________________

# if I had not done already, we'd need to install
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
# we do so by using the mutate() function and call it "change_fcover_vil" 
# additionally let us also compute a logarithmized change in forest cover (log_change_fcover_vil)


############################################################################
# Task 2

# run linear regression for model 1 in table 3

 

# What does the coefficient for the treatment from our first linear regression imply?


############################################################################
# Task 3

# estimate model 2 from table 3 in Jayachandran, too.


# What is the difference between model 1 and model 2 either according to Jayachandran
# et al (2017) or based on our regressions?


############################################################################
# Task 4

# estimate model 3


############################################################################
# Task 4

# visualize the results



###########################################################################
# GROUP DISCUSSION: What can be a conclusion from our regressions, the paper, and the graphs
 


############################################################################
# GROUP DISCUSSION: Let us discuss the model quality 



# DONE
