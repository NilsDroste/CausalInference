#############################
# Application: Baseline specification (unconditional Event-Study Analysis)
###########################################################################
# Load data
grooms <- haven::read_dta(here("createddata","grooms_st_2.dta"))

# Histogram of Authorization year
hist.auth <- ggplot(grooms[grooms$year==1977 & is.na(grooms$authyear)==F ,],
                    aes(x = as.factor(authyear))) +
  geom_bar() +
  labs(title = "", x = "\nYear of State Authorization", y = "Frequency\n") +
  theme_minimal() +
  theme(axis.title = element_text(size=15))+
  theme(axis.text.y = element_text(size = 14, face = "bold"))+
  theme(axis.text.x = element_text(size = 14, face = "bold"))

hist.auth

ggsave(here("plots/histogram.pdf"), width = 12, height = 8, units = "in", plot = hist.auth)
#dev.off()

# Drop all units that are "always treated"
grooms <- subset(grooms, (grooms$authyear>=1977) | (is.na(grooms$authyear)==1))
grooms$treat <- 1 - base::is.na(grooms$authyear)
grooms$authyear <- base::ifelse(is.na(grooms$authyear), 0, grooms$authyear)


# Visualize exact treatment timing
year.adoption <- panelView(violrate ~ after,
                           data = as.data.frame(grooms),
                           index = c("state_code", "year"), pre.post = T,
                           by.timing = TRUE,
                           theme.bw = 1,
                           xlab = "", ylab = "",
                           xlim = c(1970,2008),
                           axis.lab.gap = c(2,0),
                           cex.main = 20, cex.axis= 28, cex.lab = 16, cex.legend = 30,
                           main = "",
                           background = "white",
                           legend.labs = c("Never-treated",
                                           "Treated (before state authorization)",
                                           "Treated (after state authorization)")
)
year.adoption
ggsave(here("plots/year-adoption.pdf"), width = 20, height = 14, units = "in", plot = year.adoption)
dev.off()
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Outcome: Violrate
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  Never treated as control
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Get ATT(g,t)'s
out.never.violrate <- MarcusSantAnna2020::unc_att_gt_panel(y_name ="violrate",
                                                   t_name = "year",
                                                   id_name = "state",
                                                   first_treat_name = "authyear",
                                                   #het_name = "corrupt",
                                                   weights_name = "totfac",
                                                   comparison_group = "never",
                                                   data = grooms,
                                                   print_details = FALSE,
                                                   nboot = nboot,
                                                   bstrap = FALSE,
                                                   alp = alp)
# Get event-study estimates
ES_aggte <- did::aggte(out.never.violrate, type = "dynamic")

# Select those with event time within the range we want
e_select <- (ES_aggte$egt >= mine) & ((ES_aggte$egt <= maxe))
egt <- ES_aggte$egt[e_select]
ES_att <- ES_aggte$att.egt[e_select]
ES_inf_function <- ES_aggte$inf.function$dynamic.inf.func.e[, e_select]
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
  scale_y_continuous(limits =c(-0.07,0.05), breaks = seq(-0.07,0.05,0.02)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  #ggtitle("Outcome: Violation rate \nComparison group: Never treated")
  ggtitle("(b) Event study using never-treated units as comparison group")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  Not-Yet-treated as control
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
out.ny.violrate <- MarcusSantAnna2020::unc_att_gt_panel(y_name ="violrate",
                                                t_name = "year",
                                                id_name = "state",
                                                first_treat_name = "authyear",
                                                #het_name = "corrupt",
                                                weights_name = "totfac",
                                                comparison_group = "not_yet",
                                                data = grooms,
                                                nboot = nboot,
                                                print_details = FALSE,
                                                bstrap = FALSE,
                                                alp = alp)
# Get event-study estimates
ES_aggte <- did::aggte(out.ny.violrate, type = "dynamic")
# Select those with event time within the range we want
e_select <- (ES_aggte$egt >= mine) & ((ES_aggte$egt <= maxe))
egt <- ES_aggte$egt[e_select]
ES_att <- ES_aggte$att.egt[e_select]
ES_inf_function <- ES_aggte$inf.function$dynamic.inf.func.e[, e_select]
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
  #geom_errorbar(aes(colour=post), width=0.1) +
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin=  (att-cv*att.se), ymax =  (att+cv*att.se)), alpha = 0.35)+
  #geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.35) +
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.07,0.05), breaks = seq(-0.07,0.05,0.02)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  #ggtitle("Outcome: Violation rate \nComparison group: Not-yet treated")
  ggtitle("(c) Event study using not-yet-treated units as comparison group")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  All not-Yet-treated as control (new estimator from the paper)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
out.nyall.violrate <- MarcusSantAnna2020::unc_att_gt_panel(y_name ="violrate",
                                                t_name = "year",
                                                id_name = "state",
                                                first_treat_name = "authyear",
                                                #het_name = "corrupt",
                                                weights_name = "totfac",
                                                comparison_group = "not_yet_all",
                                                data = grooms,
                                                nboot = nboot,
                                                print_details = FALSE,
                                                bstrap = FALSE,
                                                alp = alp)
# Get event-study estimates
ES_aggte <- did::aggte(out.nyall.violrate, type = "dynamic")
# Select those with event time within the range we want
e_select <- (ES_aggte$egt >= mine) & ((ES_aggte$egt <= maxe))
egt <- ES_aggte$egt[e_select]
ES_att <- ES_aggte$att.egt[e_select]
ES_inf_function <- ES_aggte$inf.function$dynamic.inf.func.e[, e_select]
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
  #geom_errorbar(aes(colour=post), width=0.1) +
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin=  (att-cv*att.se), ymax =  (att+cv*att.se)), alpha = 0.35)+
  #geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.35) +
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.07,0.05), breaks = seq(-0.07,0.05,0.02)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  #ggtitle("Outcome: Violation rate \nComparison group: Not-yet treated")
  ggtitle("(d) Event study using all not-yet-treated units as comparison group")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results based on TWFE specification
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using dynamic TWFE (event study)
#----------------------------------------------------------------------------
# Create the treatment leads and lag dummies
taus = fastDummies::dummy_cols(data.frame(tau = grooms$tau))
taus <- taus[paste0("tau_",-32:31)]
# I drop the tau_NA, tau_-1 (baseline) and tau_-32.Dropping two periods avoid problems as pointed out by B&J.
taus = taus[, !(colnames(taus) %in% c("tau", "tau_-1", "tau_NA","tau_-32"))]
# I put _m instead of _- so the formula below doesn't collapse.
colnames(taus)[1:30] <- paste0(rep("tau_m", 30),31:2)
grooms = cbind(grooms, taus)
#----------------------------------------------------------------------------
twfe.violrate.dyn = lfe::felm(as.formula(paste("violrate ~",
                                               paste(colnames(taus), collapse = " + "),
                                               "| (state + year)| 0 | state")),
                              data = grooms,
                              exactDOF = F,
                              weights = grooms$totfac)
#----------------------------------------------------------------------------
# Put all in a data frame
event.study <- cbind.data.frame(year = as.factor(-20:20),
                                att = c(twfe.violrate.dyn$beta[11:30,],0,twfe.violrate.dyn$beta[31:50,]),
                                att.se = c(twfe.violrate.dyn$cse[11:30],0,twfe.violrate.dyn$cse[31:50]),
                                post = as.factor(1),
                                cv.pointwise = cv.pointwise,
                                row.names = NULL)

dabreaks <- event.study$year[seq(1, length(event.study$year), 4)]

plot.event.TWFE.violrate <- ggplot2::ggplot(event.study, aes(x=year, y=att,
                                                             ymin = (att-cv.pointwise*att.se),
                                                             ymax=att+cv.pointwise*att.se, post=post, group = 1)) +
  #geom_point(aes(colour=post), size=1.5) +
  geom_line(aes(x=year, y=att), size = 1.5, colour = "red4") +
  geom_hline(yintercept = 0,colour="black", size = 0.5, linetype = "dashed")+
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.4) +
  geom_ribbon(aes(ymin= (att - cv.pointwise*att.se), ymax=  (att+cv.pointwise*att.se)), alpha = 0.35) +
  theme_minimal()+
  xlab("# Periods treated") +
  ylab("Event Study ATT") +
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_y_continuous(limits =c(-0.07,0.05), breaks = seq(-0.07,0.05,0.02)) +
  scale_x_discrete(breaks=dabreaks, labels=as.character(dabreaks))+
  theme(plot.title = element_text(color="black", face="bold", size = 12))+
  #ggtitle("Outcome: Violation rate \nComparison group:  TWFE specification")
  ggtitle("(a) Event study based on TWFE specification")
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
violrate.plots <- grid.arrange(plot.event.TWFE.violrate,
                               plot.event.never.violrate,
                               plot.event.ny.violrate,
                               plot.event.nyall.violrate,
                               nrow = 4)

violrate.plots
ggsave(here("plots/event-study-violrate.pdf"), width = 12, height = 14, units = "in",
       plot = violrate.plots)
dev.off()

