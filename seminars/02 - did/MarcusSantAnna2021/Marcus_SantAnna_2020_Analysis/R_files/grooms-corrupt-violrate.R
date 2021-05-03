###########################################################################
# Application:  Corrupt vs Not corrupt states.
# Allow for corrupt specific trends
###########################################################################
###########################################################################
# Application: grooms_st_2
###########################################################################
# Load data
grooms <- haven::read_dta(here("../createddata/grooms_st_2.dta"))
grooms <- subset(grooms, (grooms$authyear>=1977) | (is.na(grooms$authyear)==1))
grooms$treat <- 1 - base::is.na(grooms$authyear)
grooms$authyear <- base::ifelse(is.na(grooms$authyear), 0, grooms$authyear)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  Never treated as control
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Get ATT(g,t)'s
out.never.violrate <- MarcusSantAnna2020::unc_att_gt_panel_het(y_name ="violrate",
                                                       t_name = "year",
                                                       id_name = "state",
                                                       first_treat_name = "authyear",
                                                       het_name = "corrupt",
                                                       weights_name = "totfac",
                                                       comparison_group = "never",
                                                       data = grooms,
                                                       print_details = FALSE,
                                                       nboot = nboot,
                                                       bstrap = FALSE,
                                                       alp = alp)
# Get event-study estimates
ES_aggte <- MarcusSantAnna2020::aggte_het(out.never.violrate, type = "dynamic")

# Select those with event time within the range we want
e_select <- (ES_aggte$dif$egt >= mine) & ((ES_aggte$dif$egt <= maxe))
egt <- ES_aggte$dif$egt[e_select]
ES_att <- ES_aggte$dif$att.egt[e_select]
ES_inf_function <- ES_aggte$dif$inf.function[, e_select]
# Do the bootstrap
par_boot <-  out.never.violrate$DIDparams
bootst_es <- did::mboot(ES_inf_function, par_boot)
#----------------------------------------------------------------------------
# Put all in a data frame
event.study <- cbind.data.frame(year = as.factor(-20:20),
                                att = ES_att,
                                att.se = bootst_es$se,
                                post = as.factor(1),
                                cv = bootst_es$crit.val,
                                cv.pointwise = cv.pointwise,
                                row.names = NULL)
dabreaks <- event.study$year[seq(1, length(event.study$year), 4)]
#----------------------------------------------------------------------------
plot.event.never.violrate <- ggplot2::ggplot(event.study, aes(x=year, y=att, ymin = (att-cv*att.se),
                                                              ymax=att+cv*att.se, post=post, group = 1)) +
  geom_line(aes(x=year, y=att), size = 1.5, colour = "red4") +
  geom_hline(yintercept = 0,colour="black", size = 0.5, linetype = "dashed")+
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin=  (att-cv*att.se), ymax =  (att+cv*att.se)), alpha = 0.35)+
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.17,0.12), breaks = seq(-0.16,0.12,0.04)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  ggtitle("(b) Event study using the never-treated units as comparison group, allowing for corruption-specific trends")

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  Not-Yet-treated as control
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
out.ny.violrate <- MarcusSantAnna2020::unc_att_gt_panel_het(y_name ="violrate",
                                                    t_name = "year",
                                                    id_name = "state",
                                                    first_treat_name = "authyear",
                                                    het_name = "corrupt",
                                                    weights_name = "totfac",
                                                    comparison_group = "not_yet",
                                                    data = grooms,
                                                    nboot = nboot,
                                                    print_details = FALSE,
                                                    bstrap = FALSE,
                                                    alp = alp)
# Get event-study estimates
ES_aggte <- MarcusSantAnna2020::aggte_het(out.ny.violrate, type = "dynamic")
# Select those with event time within the range we want
e_select <- (ES_aggte$dif$egt >= mine) & ((ES_aggte$dif$egt <= maxe))
egt <- ES_aggte$dif$egt[e_select]
ES_att <- ES_aggte$dif$att.egt[e_select]
ES_inf_function <- ES_aggte$dif$inf.function[, e_select]
# Do the bootstrap
par_boot <-  out.ny.violrate$DIDparams
bootst_es <- did::mboot(ES_inf_function, par_boot)
#----------------------------------------------------------------------------
# Put all in a data frame
event.study <- cbind.data.frame(year = as.factor(-20:20),
                                att = ES_att,
                                att.se = bootst_es$se,
                                post = as.factor(1),
                                cv = bootst_es$crit.val,
                                cv.pointwise = cv.pointwise,
                                row.names = NULL)
dabreaks <- event.study$year[seq(1, length(event.study$year), 4)]
#----------------------------------------------------------------------------
plot.event.ny.violrate <- ggplot2::ggplot(event.study, aes(x=year, y=att, ymin = (att-cv*att.se),
                                                           ymax=att+cv*att.se, post=post, group = 1)) +
  geom_line(aes(x=year, y=att), size = 1.5, colour = "red4") +
  geom_hline(yintercept = 0,colour="black", size = 0.5, linetype = "dashed")+
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin=  (att-cv*att.se), ymax =  (att+cv*att.se)), alpha = 0.35)+
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.17,0.12), breaks = seq(-0.16,0.12,0.04)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  ggtitle("(c) Event study using the not-yet-treated units as comparison group, allowing for corruption-specific trends")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  All not-Yet-treated as control (new estimator from the paper)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
out.nyall.violrate <- MarcusSantAnna2020::unc_att_gt_panel_het(y_name ="violrate",
                                                       t_name = "year",
                                                       id_name = "state",
                                                       first_treat_name = "authyear",
                                                       het_name = "corrupt",
                                                       weights_name = "totfac",
                                                       comparison_group = "not_yet_all",
                                                       data = grooms,
                                                       nboot = nboot,
                                                       print_details = FALSE,
                                                       bstrap = FALSE,
                                                       alp = alp)
# Get event-study estimates
ES_aggte <- MarcusSantAnna2020::aggte_het(out.nyall.violrate, type = "dynamic")
# Select those with event time within the range we want
e_select <- (ES_aggte$dif$egt >= mine) & ((ES_aggte$dif$egt <= maxe))
egt <- ES_aggte$dif$egt[e_select]
ES_att <- ES_aggte$dif$att.egt[e_select]
ES_inf_function <- ES_aggte$dif$inf.function[, e_select]
# Do the bootstrap
par_boot <-  out.nyall.violrate$DIDparams
bootst_es <- did::mboot(ES_inf_function, par_boot)
#----------------------------------------------------------------------------
# Put all in a data frame
event.study <- cbind.data.frame(year = as.factor(-20:20),
                                att = ES_att,
                                att.se = bootst_es$se,
                                post = as.factor(1),
                                cv = bootst_es$crit.val,
                                cv.pointwise = cv.pointwise,
                                row.names = NULL)
dabreaks <- event.study$year[seq(1, length(event.study$year), 4)]
#----------------------------------------------------------------------------
plot.event.nyall.violrate <- ggplot2::ggplot(event.study, aes(x=year, y=att, ymin = (att-cv*att.se),
                                                              ymax=att+cv*att.se, post=post, group = 1)) +
  geom_line(aes(x=year, y=att), size = 1.5, colour = "red4") +
  geom_hline(yintercept = 0,colour="black", size = 0.5, linetype = "dashed")+
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin=  (att-cv*att.se), ymax =  (att+cv*att.se)), alpha = 0.35)+
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.17,0.12), breaks = seq(-0.16,0.12,0.04)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  ggtitle("(d) Event study using all not-yet-treated units as comparison group, allowing for corruption-specific trends")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using dynamic TWFE (event study)
#----------------------------------------------------------------------------
# Create the treatment leads and lag dummies 
taus = fastDummies::dummy_cols(data.frame(tau = grooms$tau))
taus <- taus[paste0("tau_",-32:31)]
# I drop the tau_NA, tau_-1 (baseline) and tau_-32.Dropping two periods avoid problems as pointed out by B&J.
taus = taus[, !(colnames(taus) %in% c("tau", "tau_-1", "tau_NA","tau_-32"))]#,
                                     # "tau_-31", "tau_-30", "tau_-29", 
                                    #  "tau_-28", "tau_-27", "tau_-26"))]

# I put _m instead of _- so the formula below doesn't collapse.
colnames(taus)[1:30] <- paste0(rep("tau_m", 30),31:2)


# Create the treatment leads and lag dummies 
taus_corrupt = grooms$corrupt * taus

# I put _m instead of _- so the formula below doesn't collapse.
colnames(taus_corrupt)[1:30] <- paste0(rep("c_tau_m", 30),31:2)
colnames(taus_corrupt)[31:62] <- paste0(rep("c_tau", 32),0:31)

# Drop these because of lack of obs (like in STATA)
taus <- taus[,-(1:5)]
taus_corrupt <- taus_corrupt[, -length(taus_corrupt)]
grooms_twfe = cbind(grooms, taus, taus_corrupt)

twfe.violrate.dyn.corrupt = lfe::felm(as.formula(paste("violrate ~", 
                                                       paste(colnames(taus), collapse = " + "),
                                                       "+",paste(colnames(taus_corrupt), collapse = " + "),
                                                       "| (state + year)| 0 | state")),
                                      data = grooms_twfe, 
                                      exactDOF = F,
                                      weights = grooms$totfac)
#----------------------------------------------------------------------------
# Outcome: violrate
#----------------------------------------------------------------------------
event.study <- cbind.data.frame(year = as.factor(-20:20),
                                att = c(twfe.violrate.dyn.corrupt$beta[73:92],0,
                                        twfe.violrate.dyn.corrupt$beta[93:112]),
                                att.se = c(twfe.violrate.dyn.corrupt$cse[73:92],0,
                                           twfe.violrate.dyn.corrupt$cse[93:112]),
                                post = as.factor(1),
                                cv.pointwise = cv.pointwise,
                                row.names = NULL)

plot.event.TWFE.violrate <- ggplot2::ggplot(event.study, aes(x=year, y=att, 
                                                             ymin = (att-cv.pointwise*att.se),
                                                             ymax=att+cv.pointwise*att.se, post=post, group = 1)) +
  #geom_point(aes(colour=post), size=1.5) +
  geom_line(aes(x=year, y=att), size = 1.5, colour = "red4") +
  geom_hline(yintercept = 0,colour="black", size = 0.5, linetype = "dashed")+
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin=  (att-cv.pointwise*att.se), ymax =  (att+cv.pointwise*att.se)), alpha = 0.35)+
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.17,0.12), breaks = seq(-0.16,0.12,0.04)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  #ggtitle("Outcome: Violation rate \nComparison group: TWFE")
  ggtitle("(a) Event study based on TWFE specification")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
violrate.plots <- grid.arrange(plot.event.TWFE.violrate,
                               plot.event.never.violrate,
                               plot.event.ny.violrate, 
                               plot.event.nyall.violrate,
                               nrow = 4)

violrate.plots
ggsave(here("plots/event-study-violrate-corrupt.pdf"), width = 12, height = 14, units = "in",
       plot = violrate.plots)
dev.off()
