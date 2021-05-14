rm(list = ls())

packages <- c("randomizr", "dbarts", "dplyr", "plyr", "ggplot2", "reshape2", "bartMachine", "foreign")

package_installed <-
  sapply(packages, function(pack)
    pack %in% rownames(installed.packages()))

if (any(!package_installed)) {
  sapply(packages[!package_installed], install.packages)
}

sapply(packages, require, character.only = TRUE)

rm(packages,package_installed)

setwd(here::here("Kim2017/dataverse_files/"))

collapsed_data <- haven::read_dta("Roll Call Votes on Environmental Issues (collapsed).dta") 
full_data <- haven::read_dta("Roll Call Votes on Environmental Issues.dta")

merged_data <- dplyr::left_join(full_data, collapsed_data)

subset_data <- merged_data[which(merged_data$dem_share>48.5 & merged_data$dem_share<51.5),]
subset_data <- subset_data[c("mean_pro_env", "dem_win", "z2_dem_oilgas_share", "z2_diff_pro", "z2_fossil_resource")]
subset_data <- na.omit(subset_data)

# Bart Machine

training_set <- na.omit(subset_data)

n_test <- nrow(training_set)
test_set <- training_set[rep(1:n_test, 2),]
test_set[1:n_test, "dem_win"] <-1
test_set[(n_test+1):(2*n_test), "dem_win"] <-0

train <- select(training_set, -mean_pro_env) %>% as.data.frame()
test <- select(test_set, -mean_pro_env) %>% as.data.frame()

bart_m_fit <- bartMachine(X = train, y = training_set$mean_pro_env)

predicted <- predict(bart_m_fit, new_data = test)

subset_data <- within(subset_data,{
  ate_hats <- predicted[1:n_test] - predicted[(n_test+1):(2*n_test)]
})

  # Figure for Oil/Gas PAC (Figure 4 in Main Manuscript)

ggplot(aes(x=z2_dem_oilgas_share, y=ate_hats), data=subset_data) + geom_point(shape=20) + stat_smooth() + theme_bw() +ylim(0,1) + xlab("Oil and Gas PAC for Democrat") + ylab("BART estimated treatment effects")

  # Figure for Public Opinion (Figure 4 in Main Manuscript)

ggplot(aes(x=z2_diff_pro, y=ate_hats), data=subset_data) + geom_point(shape=20) + stat_smooth() + theme_bw() +ylim(0,1) + xlab("Polarization in Public Opinion") + ylab("BART estimated treatment effects")

  # Figure for Natural Resource Endowment (Figure 4 in Main Manuscript)

ggplot(aes(x=z2_fossil_resource, y=ate_hats), data=subset_data) + geom_point(shape=20) + stat_smooth() + theme_bw() +ylim(0,1)+ xlim(-0.3,2.5) + xlab("State-Level Natural Resource Endowment") + ylab("BART estimated treatment effects")

# relative importance 

relative = interaction_investigator(bart_m_fit,plot=FALSE)

relative_importance <- data.frame(
  variable=factor(c("Oil and Gas PAC", "Polarization", "Natural Resources"), levels = c("Oil and Gas PAC", "Natural Resources", "Polarization"))
)

relative_importance$importance <- relative$interaction_counts_avg[2:4,1]
relative_importance <- relative_importance[order(relative_importance$importance),] 

# not working so far (empty plot)

ggplot(data=relative_importance, aes(x=variable, y=importance)) +
  geom_bar(stat="identity",width=.7) + xlab("Interaction Variable") + ylab("Relative Importance") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave("Analysis/BART/RelativeImportance.pdf", width=8.5, height=6)

