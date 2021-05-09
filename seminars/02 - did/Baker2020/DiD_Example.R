# I stole this from Andre Baker: https://github.com/andrewchbaker/DiD_Slides/blob/master/DiD.Rmd 

library(tidyverse)
library(here)
library(ggthemes)
library(lfe)
library(did2) # devtools::install_github("pedrohcgs/did2")
# library(xaringan) # nice one though if you want to do presentations in R!
library(patchwork)
library(bacondecomp)
library(multcomp)
library(fastDummies)
library(magrittr)
library(MCPanel) #  devtools::install_github("susanathey/MCPanel")

select <- dplyr::select
theme_set(theme_clean() + theme(plot.background = element_blank()))

# make data
data <- tibble(
  Y = c(2, 5, 1, 2),
  Unit = c("Treat", "Treat", "Control", "Control"),
  T = c(0, 1, 0, 1)
)
# plot
data %>% 
  ggplot(aes(x = T, y = Y, group = Unit, color = Unit)) + geom_line(size = 2) + 
  labs(x = "Time", y = "Outcome") + 
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_colour_brewer(palette = 'Set1') + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.background = element_blank())


# Two-Way Differencing ----
data %>% 
  group_by(Unit) %>% 
  mutate(Y2 = Y - Y[which(T == 0)]) %>% 
  ggplot(aes(x = T, y = Y, group = Unit, color = Unit)) + 
  geom_line(size = 2) + 
  geom_line(aes(x = T, y = Y2, group = Unit, color = Unit), linetype = "dashed", size = 2) + 
  labs(x = "Time", y = "Outcome") + 
  scale_x_continuous(breaks = c(1, 2)) +
  scale_colour_brewer(palette = 'Set1') + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.background = element_blank()) + 
  annotate("label", y = 2, x = 0.85, label = "Treatment \n Effect") + 
  annotate("segment", x = 1, xend = 1, y = 1, yend = 3, color = "black", size = 1) + 
  annotate("segment", x = 0.92, xend = 1, y = 2, yend = 3, color = "black", 
           linetype = "dashed", size = 2) + 
  annotate("segment", x = 0.92, xend = 1, y = 2, yend = 1, color = "black", 
           linetype = "dashed", size = 2) + 
  annotate("segment", x = 0, xend = 0, y = 2, yend = 0, color = "#377EB8", 
           arrow = arrow(length = unit(0.1, "inches")), size = 2) + 
  annotate("segment", x = 0.5, xend = 0.5, y = 1.5, yend = 0.5, color = "#E41A1C", 
           arrow = arrow(length = unit(0.1, "inches")), size = 2)

# Bias with TWFE - Goodman-Bacon (2019) ----
data <- tibble(
  time = 0:100,
  U = seq(5, 12, length.out = 101),
  l = seq(10, 17, length.out = 101) + c(rep(0, 85), rep(15, 16)),
  k = seq(18, 25, length.out = 101) + c(rep(0, 34), rep(10, 67))
) %>% 
  pivot_longer(-time, names_to = "series", values_to = "value")
data %>% 
  ggplot(aes(x = time, y = value, group = series, color = series, shape = series)) + 
  geom_line(size = 2) + geom_point(size = 2) +
  geom_vline(xintercept = c(34, 85)) +
  labs(x = "Time", y = "Units of y") +
  scale_x_continuous(limits = c(0, 100), breaks = c(34, 85), 
                     labels = c(expression('t'['k']^'*'), expression('t'['l']^'*')), 
                     expand = c(0, 0)) + 
  annotate("text", x = 10, y = 21, label = expression('y'['it']^'k'), size = 9) +
  annotate("text", x = 50, y = 16, label = expression('y'['it']^'l'), size = 9) +
  annotate("text", x = 90, y = 14, label = expression('y'['it']^'U'), size = 9) +
  annotate('label', x = 17, y = 3, label = 'PRE(k)') +
  annotate('label', x = 60, y = 3, label = 'MID(k, l)') +
  annotate('label', x = 93, y = 3, label = 'POST(l)') +
  annotate("segment", x = 1, xend = 33, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 33, xend = 1, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 35, xend = 84, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 84, xend = 35, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) + 
  annotate("segment", x = 86, xend = 99, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 99, xend = 86, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_colour_brewer(palette = 'Set1') + 
  theme(axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        plot.background = element_blank())

# function to make subplots
make_subplot <- function(omit, keep_dates, colors, breaks, break_expressions, series, 
                         series_x, series_y, break_names, break_loc, arrow_start, arrow_stop, title){
  
  data %>% 
    filter(series != omit & time >= keep_dates[1] & time <= keep_dates[2]) %>% 
    ggplot(aes(x = time, y = value, group = series, color = series, shape = series)) + geom_line() + geom_point() +
    geom_vline(xintercept = breaks) + 
    labs(x = "Time", y = "Units of y") +
    scale_x_continuous(limits = c(0, 105), breaks = breaks, 
                       labels = break_expressions, 
                       expand = c(0, 0)) + 
    annotate("text", x = series_x[1], y = series_y[1], label = series[1]) +
    annotate("text", x = series_x[2], y = series_y[2], label = series[2]) +
    annotate('label', x = break_loc[1], y = 5, label = break_names[1]) +
    annotate('label', x = break_loc[2], y = 5, label = break_names[2]) +
    annotate("segment", x = arrow_start[1], xend = arrow_stop[1], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("segment", x = arrow_stop[1], xend = arrow_start[1], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("segment", x = arrow_start[2], xend = arrow_stop[2], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("segment", x = arrow_stop[2], xend = arrow_start[2], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) + 
    scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
    scale_color_manual(values = c(colors[1], colors[2])) +  
    ggtitle(title) + 
    theme(axis.ticks.x = element_blank(),
          legend.position = 'none',
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.background = element_blank()) 
}
p1 <- make_subplot(omit = "l", keep_dates = c(0, 100), colors = c('#E41A1C', '#377EB8'), breaks = 34, 
                   break_expressions = expression('t'['k']^'*'), 
                   series = c(expression('y'['it']^'k'), expression('y'['it']^'U')),
                   series_x = c(10, 90), series_y = c(23, 16), 
                   break_names = c('Pre(k)', 'Post(k)'), break_loc = c(17, 66), 
                   arrow_start = c(1, 35), arrow_stop = c(33, 99), 
                   title = paste('A. Early Group vs. Untreated Group'))
p2 <- make_subplot(omit = "k", keep_dates = c(0, 100), colors = c('#4DAF4A', '#377EB8'), breaks = 85, 
                   break_expressions = expression('t'['l']^'*'), 
                   series = c(expression('y'['it']^'l'), expression('y'['it']^'U')),
                   series_x = c(50, 90), series_y = c(18, 16), 
                   break_names = c('Pre(l)', 'Post(l)'), break_loc = c(50, 95), 
                   arrow_start = c(1, 86), arrow_stop = c(84, 99), 
                   title = paste('B. Late Group vs. Untreated Group'))
p3 <- make_subplot(omit = "U", keep_dates = c(0, 84), colors = c('#E41A1C', '#4DAF4A'), breaks = c(34, 85), 
                   break_expressions = c(expression('t'['k']^'*'), expression('t'['l']^'*')), 
                   series = c(expression('y'['it']^'k'), expression('y'['it']^'l')),
                   series_x = c(10, 50), series_y = c(23, 18), 
                   break_names = c('Pre(k)', 'Mid(k, l)'), break_loc = c(17, 60), 
                   arrow_start = c(1, 35), arrow_stop = c(33, 84), 
                   title = bquote(paste('C. Early Group vs. Late Group, before ', 't'['l']^'*', sep = " ")))
p4 <- make_subplot(omit = "U", keep_dates = c(34, 100), colors = c('#E41A1C', '#4DAF4A'), breaks = c(34, 85), 
                   break_expressions = c(expression('t'['k']^'*'), expression('t'['l']^'*')), 
                   series = c(expression('y'['it']^'k'), expression('y'['it']^'l')),
                   series_x = c(60, 50), series_y = c(36, 18), 
                   break_names = c('Mid(k, l)', 'Post(l)'), break_loc = c(60, 95), 
                   arrow_start = c(35, 86), arrow_stop = c(84, 99), 
                   title = bquote(paste('D. Late Group vs. Early Group, after ', 't'['k']^'*', sep = " ")))
# combine plots
p1 + p2 + p3 + p4 + plot_layout(nrow = 2)


# Goodman-Bacon Decomposition ----
# unit fixed effects
unit <- tibble(
  unit = 1:1000, 
  unit_fe = rnorm(1000, 0, 1),
  # generate state
  state = sample(1:50, 1000, replace = TRUE))
# year fixed effects 
year <- tibble(
  year = 1980:2015,
  year_fe = rnorm(36, 0, 1))
# Trend Break
# Put the states into treatment groups
treat_taus <- tibble(
  # sample the states randomly
  state = sample(1:50, 50, replace = FALSE),
  # place the randomly sampled states into five treatment groups G_g
  cohort_year = sort(rep(c(1985, 1991, 1997, 2003, 2009), 10)),
  # assign them a mean treatment effect from 0.5 to 0.1
  mu = sort(rep(c(.5, .4, .3, .2, .1), 10), decreasing = TRUE))
# make main dataset
# full interaction of unit X year 
data <- expand_grid(unit = 1:1000, year = 1980:2015) %>% 
  left_join(., unit) %>% 
  left_join(., year)
# bring in the treatment indicators and values
get_treat <- function(u) {
  # get the state for the unit
  st <- unit %>% filter(unit == u) %>% pull(state)
  
  # find the treatment year for the state
  treat_yr <- treat_taus %>% filter(state == st) %>% pull(cohort_year)
  
  # treatment effect tau_g
  mu <- treat_taus %>% filter(state == st) %>% pull(mu)
  
  # Make a data set with the results 
  tibble(unit = rep(u, 36), 
         year = 1980:2015,
         # get a treatment cohort indicator
         # make treatment indicator
         treat = ifelse(year < treat_yr, 0, 1),
         # get the treatment effect \tau_i for post-treatment years
         cohort_year = treat_yr,
         static_tau = rep(rnorm(1, mu, .2), 36),
         tau = ifelse(year < treat_yr, 0, static_tau),
         # cumulate the effect
         tau_cum = cumsum(tau))
}
# call the function over our 1000 firms
treatments <- map_dfr(1:1000, get_treat)
# merge in the treatment effect data
data <- left_join(data, treatments) %>% 
  # simulate error and generate the dependent variable
  mutate(error = rnorm(36000, 0, 0.5),
         dep_var = unit_fe + year_fe + tau_cum + error)
# calculate the bacon decomposition without covariates
bacon_out <- bacon(dep_var ~ treat,
                   data = data,
                   id_var = "unit",
                   time_var = "year")
bacon_out %>% 
  ggplot(aes(x = weight, y = estimate, shape = factor(type), color = factor(type))) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  scale_colour_brewer(palette = 'Set1') + 
  labs(x = "Weight", y = "Estimate") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))


# Callaway & Sant'Anna ----


# create a lead/lag indicators
data <- data %>% 
  # variable with relative year from treatment
  mutate(rel_year = year - cohort_year) %>% 
  # drop observations after 2008 bc all treated 
  filter(year <= 2008) %>% 
  dplyr::arrange(cohort_year, unit, year)
# first get percentage contribution to each lead/lag indicator by treatment cohort for weights
# we will need this for the Abraham/Sun method, as well as the true treatment indicator
# calculate weights
weights <- data %>% 
  mutate(rel_year = year - cohort_year) %>% 
  # drop covariates for 2009 adopters
  filter(cohort_year != 2009) %>% 
  group_by(cohort_year, rel_year) %>% 
  count %>% 
  ungroup() %>% 
  group_by(rel_year) %>% 
  mutate(total = sum(n),
         perc = n / total) %>% 
  # keep just the variables we need
  dplyr::select(rel_year, cohort_year, perc) %>% 
  ungroup() %>% 
  rowwise() %>% 
  # add variable equal to coefficient from regression
  mutate(term = paste("cohort_year_", cohort_year, "_", rel_year + 29, sep = "")) %>% 
  ungroup()
# make a dataset with the theoretical values to merge in
true_effect <- weights %>% 
  # add in the multiples
  mutate(
    multiple = case_when(
      rel_year < 0 ~ 0,
      rel_year >= 0 ~ rel_year + 1),
    # add in the tau_g values 
    tau_g = case_when(
      cohort_year == 1985 ~ .5,
      cohort_year == 1991 ~ .4,
      cohort_year == 1997 ~ .3,
      cohort_year == 2003 ~ .2),
    # multiply the two 
    effect = multiple*tau_g) %>% 
  #collapse by  time period 
  group_by(rel_year) %>% 
  summarize(true_tau = weighted.mean(effect, w = perc)) %>% 
  # make the time variable for merging
  mutate(t = rel_year)
# run the CS algorithm
CS_out <- att_gt("dep_var", data = data,
                 first.treat.name="cohort_year",
                 idname="unit", tname="year", aggte = T,
                 clustervars = "state",
                 bstrap=T, cband=T,
                 maxe = 6,
                 mine = -4,
                 nevertreated = F,
                 printdetails = F)
# plot
tibble(
  t = -5:5,
  estimate = CS_out$aggte$dynamic.att.e,
  se = CS_out$aggte$dynamic.se.e,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se,) %>% 
  left_join(true_effect) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(aes(x = t, y = true_tau, color = "True Effect"), size = 1.5, linetype = "dashed") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups),
              color = "lightgrey", alpha = 1/4) + 
  #geom_point(aes(color = "Estimated Effect")) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") +
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) 


# Abraham and Sun ----


## Make cohort-relative time dummies
# relative year dummies
rel_year <- data %>% dplyr::select(rel_year) %>% 
  dummy_cols %>% dplyr::select(-1) %>% 
  set_colnames(as.numeric(str_remove(colnames(.), "rel_year_")) + 29) %>% 
  as.data.frame
# cohort dummies
cohorts <- data %>% dplyr::select(cohort_year) %>% 
  dummy_cols %>% dplyr::select(-1) %>% 
  as.data.frame
# combine matrix functions
combine_mat <- function(i) {
  cohorts[, i] * rel_year %>% 
    set_colnames(paste(colnames(cohorts)[i], colnames(rel_year), sep = "_"))
}
# combine dummies and merge into our data
dummies <- map_dfc(1:4, combine_mat)
data <- data %>% bind_cols(dummies)
# put the covariates into a vector form
covs <- paste("cohort_year_", rep(c(1985, 1991, 1997, 2003), 51), "_", c(-28:-2, 0:23) + 29, sep = "")
# estimate the saturated model
fit <- felm(as.formula(paste("dep_var ~ ", paste(covs, collapse = "+"), "| unit + year | 0 | state")), 
            data = data, exactDOF = TRUE)
# rerun without the NA covariates because glmt won't run otherwise
# new set of covariates without the na
covs <- broom::tidy(fit) %>% filter(!is.na(estimate)) %>% pull(term)
fit <- felm(as.formula(paste("dep_var ~ ", paste(covs, collapse = "+"), "| unit + year | 0 | state")), 
            data = data, exactDOF = TRUE)
# get the coefficients and make a dataset for plotting
coefs <- fit$coefficients %>%
  # add in coefficient name to tibble
  as_tibble(rownames = "term") %>% 
  # bring in weights
  left_join(., weights)
# get the relevant coefficients and weights into a string to get the linear combination
get_lincom <- function(ll) {
  # get just the coefficients for a specific lead lag
  cf2 <- coefs %>% filter(rel_year == ll)
  # paste the function that goes into the linear combination function
  F <- paste(paste(cf2$perc, cf2$term, sep = " * ", collapse = " + "), " = 0")
  # take linear combination and put into a data frame
  broom::tidy(
    confint(glht(fit, linfct = F)),
    conf.int = TRUE
  ) %>% mutate(rel_year = ll)
}
# run over all lead/lags
AS_plot <- map_df(c(-5:-2, 0:5), get_lincom) %>% 
  # add time variable
  mutate(t = c(-5:-2, 0:5))
#Plot the results
AS_plot %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  left_join(true_effect) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(aes(x = t, y = true_tau, color = "True Effect"), size = 1.5, linetype = "dashed") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups),
              color = "lightgrey", alpha = 1/4) + 
  #geom_point(aes(color = "Estimated Effect")) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") +
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

