# Matching example
# SAGM002 course
# sources:
# https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html
# https://kosukeimai.github.io/MatchIt/articles/assessing-balance.html
# https://mran.microsoft.com/snapshot/2017-08-06/web/packages/cobalt/vignettes/cobalt_basic_use.html
# https://humboldt-wi.github.io/blog/research/applied_predictive_modeling_19/matching_methods/ 

# load libraries
library(tidyverse)
library(MatchIt)
library(cobalt)
library(party)
library(twang)
library(Zelig)
# library(WeightIt)

# load data
data("lalonde",  package = "cobalt") 
lalonde <- lalonde %>% as_tibble()
lalonde

# matchit intro ----
m.out <- matchit(treat ~ age + educ + race + married + 
                   nodegree + re74 + re75, data = lalonde,
                 method = "full")
m.out

# visuals ----

#eCDF plot
plot(m.out, type = "ecdf", which.xs = c("age", "re74", "married"))

# Generating ATT weights as specified in Austin (2011)
psFormula <- formula(treat ~ age + educ + race + married + 
                       nodegree + re74 + re75)

p.score <- 
  glm(psFormula, data = lalonde, 
      family = "binomial")$fitted.values

PS.weights <- 
  with(lalonde, treat + (1-treat)*p.score/(1-p.score))

# plot
bal.plot(psFormula, 
         data = lalonde, weights = PS.weights, distance = p.score, 
         method = "weighting", which = "both")



# model comparison ----

psFormula <- formula(treat ~ age + educ + race + married + 
                       nodegree + re74 + re75)

#CEM --
cemMatching <- matchit(psFormula, data = lalonde, method = "cem")
cemMatching
plot(cemMatching)

# MDM --
MDMatching <-  matchit(psFormula, data = lalonde, method = "nearest",
                       distance = "glm", caliper = .1,
                       mahvars = ~ age + educ + race + married + nodegree + re74 + re75)
MDMatching

bal.plot(psFormula, 
         data = lalonde, var.name = "distance",
         weights = MDMatching$weights, distance = MDMatching$distance, 
         method = "weighting", which = "both")


# PSM (with replacement) --
PSMMatching <- matchit(psFormula, distance = p.score,
                       data = lalonde, method = "nearest", 
                       ratio = 1, replace = T, caliper = 0.25)
PSMMatching

bal.plot(psFormula, data = lalonde, var.name = "distance", 
         weights = PSMMatching$weights, distance = PSMMatching$distance,
         method = "weighting", which = "both")


# # RF --
#estimate propensity scores with random forests
mycontrols <- cforest_unbiased(ntree=10000, mtry=5)
mycforest <- cforest(psFormula, data=lalonde, controls=mycontrols)

#obtain a list of predicted probabilities
RFp.scores <- predict(mycforest, type="prob") %>% unlist()

RFMatching <- matchit(psFormula, distance = RFp.scores,
                      data = lalonde, method = "nearest",
                      ratio = 1, replace = T, caliper = 0.25)
RFMatching

bal.plot(psFormula, data = lalonde, var.name = "distance",
         weights = RFMatching$weights, distance = RFp.scores,
         method = "weighting", which = "both")


# XGBoost --
XGBp.scores  <- ps(psFormula, data = lalonde %>% as.data.frame(), n.trees=10000, interaction.depth=4,
                   shrinkage=0.01, stop.method=c("es.max"), estimand = "ATT",
                   verbose=TRUE)$ps %>% unlist()

XGBMatching <- matchit(psFormula, distance = XGBp.scores,
                       data = lalonde, method = "nearest",
                       ratio = 1, replace = T, caliper = 0.25)
XGBMatching

bal.plot(psFormula, data = lalonde, var.name = "distance",
         weights = XGBMatching$weights, distance = XGBp.scores,
         method = "weighting", which = "both")


# Genetic Matching
GenMatching <- matchit(psFormula, data = lalonde, method = "genetic", pop.size=1000)
GenMatching

bal.plot(psFormula, data = lalonde, var.name = "distance",
         weights = GenMatching$weights, distance = GenMatching$distance, 
         method = "weighting", which = "both")


#comparison
ageplot <- bal.plot(psFormula, data = lalonde, 
                    weights = list(CEM = cemMatching,
                                   MDM = MDMatching,
                                   PSM = PSMMatching,
                                   RF = RFMatching,
                                   XGB = XGBMatching,
                                   GEN = GenMatching),
                    var.name = "age",
                    which = "both")

educplot <- bal.plot(psFormula, data = lalonde, 
                     weights = list(CEM = cemMatching,
                                    MDM = MDMatching,
                                    PSM = PSMMatching,
                                    RF = RFMatching,
                                    XGB = XGBMatching,
                                    GEN = GenMatching),
                     var.name = "educ",
                     which = "both")

raceplot <- bal.plot(psFormula, data = lalonde, 
                     weights = list(CEM = cemMatching,
                                    MDM = MDMatching,
                                    PSM = PSMMatching,
                                    RF = RFMatching,
                                    XGB = XGBMatching,
                                    GEN = GenMatching),
                     var.name = "race",
                     which = "both")

marriedplot <- bal.plot(psFormula, data = lalonde, 
                        weights = list(CEM = cemMatching,
                                       MDM = MDMatching,
                                       PSM = PSMMatching,
                                       RF = RFMatching,
                                       XGB = XGBMatching,
                                       GEN = GenMatching),
                        var.name = "married",
                        which = "both")

nodegreeplot <- bal.plot(psFormula, data = lalonde, 
                         weights = list(CEM = cemMatching,
                                        MDM = MDMatching,
                                        PSM = PSMMatching,
                                        RF = RFMatching,
                                        XGB = XGBMatching,
                                        GEN = GenMatching),
                         var.name = "nodegree",
                         which = "both")

re74plot <- bal.plot(psFormula, data = lalonde, 
                     weights = list(CEM = cemMatching,
                                    MDM = MDMatching,
                                    PSM = PSMMatching,
                                    RF = RFMatching,
                                    XGB = XGBMatching,
                                    GEN = GenMatching),
                     var.name = "re74",
                     which = "both")

re75plot <- bal.plot(psFormula, data = lalonde, 
                     weights = list(CEM = cemMatching,
                                    MDM = MDMatching,
                                    PSM = PSMMatching,
                                    RF = RFMatching,
                                    XGB = XGBMatching,
                                    GEN = GenMatching),
                     var.name = "re75",
                     which = "both")

cowplot::plot_grid(ageplot, educplot, raceplot, marriedplot, nodegreeplot, re74plot, re75plot, ncol = 1)

# Estimating ATT (using Zelig::)
# see also https://imai.fas.harvard.edu/talk/files/kansas10.pdf
# or https://r.iq.harvard.edu/docs/matchit/2.4-15/Examples2.html
zeFormula <- formula(re78 ~ age + educ + educ^2 + race + nodegree + married + re74 + re75 + treat)

get_estimated_treatment <- function(matching_output, zeFormula){
  z.out <- zelig(zeFormula , data = match.data(matching_output), model = "ls")
  # x.out <- setx(z.out, data = match.data(matching_output, "treat"), cond = TRUE)
  # s.out <- sim(z.out, x = x.out)
  return(z.out)
}

cem_ATE <- get_estimated_treatment(cemMatching, zeFormula)
mdm_ATE<- get_estimated_treatment(MDMatching, zeFormula)
psm_ATE <- get_estimated_treatment(PSMMatching, zeFormula)
rf_ATE <- get_estimated_treatment(RFMatching, zeFormula)
xgb_ATE <- get_estimated_treatment(XGBMatching, zeFormula)
gen_ATE <- get_estimated_treatment(GenMatching, zeFormula)

# compute mean absolute error
cem <- Metrics::mae(match.data(cemMatching)$re78,cem_ATE$get_predict() %>% unlist())
mdm <- Metrics::mae(match.data(MDMatching)$re78,mdm_ATE$get_predict() %>% unlist())
psm <- Metrics::mae(match.data(PSMMatching)$re78,psm_ATE$get_predict() %>% unlist())
rf <- Metrics::mae(match.data(RFMatching)$re78,rf_ATE$get_predict() %>% unlist())
xgb <- Metrics::mae(match.data(XGBMatching)$re78,xgb_ATE$get_predict() %>% unlist())
gen <- Metrics::mae(match.data(GenMatching)$re78,gen_ATE$get_predict() %>% unlist())

# compute mean squared error
mse_cem <- Metrics::rmse(match.data(cemMatching)$re78,cem_ATE$get_predict() %>% unlist())
mse_mdm <- Metrics::rmse(match.data(MDMatching)$re78,mdm_ATE$get_predict() %>% unlist())
mse_psm <- Metrics::rmse(match.data(PSMMatching)$re78,psm_ATE$get_predict() %>% unlist())
mse_rf <- Metrics::rmse(match.data(RFMatching)$re78,rf_ATE$get_predict() %>% unlist())
mse_xgb <- Metrics::rmse(match.data(XGBMatching)$re78,xgb_ATE$get_predict() %>% unlist())
mse_gen <- Metrics::rmse(match.data(GenMatching)$re78,gen_ATE$get_predict() %>% unlist())


to_plot <- bind_cols(model = stringr::str_to_upper(c("cem","mdm","psm","rf","xgb","gen")), MAE=c(cem,mdm,psm,rf,xgb,gen), MSE=c(mse_cem, mse_mdm, mse_psm, mse_rf, mse_xgb, mse_gen))  

MAEplot <- to_plot %>% ggplot(aes(y=MAE, x = reorder(model, -MAE), fill = model)) + geom_bar(stat="identity") + theme_minimal() + theme(text = element_text(size=20), axis.title = element_text(color = "grey20", size = 25)) + ggthemes::scale_fill_tableau() + ylab("Mean absolute error") + xlab("matching model") + theme(legend.position = "none")

MSEplot <- to_plot %>% ggplot(aes(y=MSE, x = reorder(model, -MSE), fill = model)) + geom_bar(stat="identity") + theme_minimal() + theme(text = element_text(size=20), axis.title = element_text(color = "grey20", size = 25)) + ggthemes::scale_fill_tableau() + ylab("Root mean squared error") + xlab("matching model") + theme(legend.position = "none")

# Addon: unmatched estimates from RCT
all <- lm(re78 ~ age + educ + race + married + nodegree + re74 + re75 + treat, data=lalonde)
summary(all)
lmtest::coeftest(all, vcov = sandwich::vcovHAC(all))


# TASK: compute ATT and ATE
taskFormula <- formula(re78 ~ age + educ + educ^2 + race + nodegree + married + re74 + re75)

att_mod <- zelig(taskFormula , data = match.data(cemMatching, "treat"), model = "ls")
att_est <- att_mod$get_predict() %>% unlist()

ateu_mod <- zelig(taskFormula , data = match.data(cemMatching, "control"), model = "ls")
ateu_est <- ateu_mod$get_predict() %>% unlist()

ate_point_est <- c(
  ate = mean(att_est)-mean(ateu_est),
  sd = sqrt((length(att_est -1) * sd(att_est)^2 + length(ateu_est -1) * sd(ateu_est)^2) / (length(att_est)) + length(ateu_est) - 2 ) * sqrt(1/length(att_est) + 1/length(ateu_est))
)
# for calc of sd of differences of means see https://stats.stackexchange.com/questions/302445/standard-error-of-difference 

t.test(att_est, ateu_est)
