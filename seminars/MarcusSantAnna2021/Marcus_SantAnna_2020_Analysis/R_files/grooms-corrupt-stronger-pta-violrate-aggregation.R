###########################################################################
# Application: Aggregated results; Corrupt vs Not corrupt states.
# Ruling out corruption-specific trends
###########################################################################
###########################################################################
# Load data
grooms <- haven::read_dta(here("createddata","grooms_st_2.dta"))
# Drop all units that are "always treated"
grooms <- subset(grooms, (grooms$authyear>=1977) | (is.na(grooms$authyear)==1))
grooms$treat <- 1 - base::is.na(grooms$authyear)
grooms$authyear <- base::ifelse(is.na(grooms$authyear), 0, grooms$authyear)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Outcome: Violation rate
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  Never treated as control
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Get ATT(g,t)'s
out.never.violrate <- MarcusSantAnna2020::unc_att_gt_panel_het2(y_name ="violrate",
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
e_select <- (ES_aggte$dif$egt >= 0) & ((ES_aggte$dif$egt <= maxe))
egt <- ES_aggte$egt[e_select]
# Get influence function for average of ES estimates
ES_inf_function <- ES_aggte$dif$inf.function[, e_select]
inf_func <- rowMeans(ES_inf_function)
# Do the bootstrap
par_boot <-  out.never.violrate$DIDparams
bootst_es <- did::mboot(inf_func, par_boot)
# Save point estimates and standard errors
att_never_time <- mean( ES_aggte$dif$att.egt[e_select])
att_never_time_se <- bootst_es$se

# Now do this for the "simple estimator"
aggte_simple <- MarcusSantAnna2020::aggte_het(out.never.violrate, type = "simple")
inf_func_simple <- aggte_simple$aggte_het1$inf.function$simple.att -  
  aggte_simple$aggte_het0$inf.function$simple.att
bootst_simple <- did::mboot(inf_func_simple, par_boot)
# Save point estimates and standard errors
att_never_simple <- aggte_simple$aggte_het1$overall.att - aggte_simple$aggte_het0$overall.att
att_never_simple_se <- bootst_simple$se

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  Not-Yet-treated as control
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
out.ny.violrate <- MarcusSantAnna2020::unc_att_gt_panel_het2(y_name ="violrate",
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
e_select <-  (ES_aggte$dif$egt >= 0) & ((ES_aggte$dif$egt <= maxe))
egt <- ES_aggte$egt[e_select]
# Get influence function for average of ES estimates
ES_inf_function <- ES_aggte$dif$inf.function[, e_select]
inf_func <- rowMeans(ES_inf_function)
# Do the bootstrap
par_boot <-  out.ny.violrate$DIDparams
bootst_es <- did::mboot(inf_func, par_boot)
# Save point estimates and standard errors
att_ny_time <- mean( ES_aggte$dif$att.egt[e_select])
att_ny_time_se <- bootst_es$se

# Now do this for the "simple estimator"
aggte_simple <- MarcusSantAnna2020::aggte_het(out.ny.violrate, type = "simple")
inf_func_simple <- aggte_simple$aggte_het1$inf.function$simple.att - 
  aggte_simple$aggte_het0$inf.function$simple.att
bootst_simple <- did::mboot(inf_func_simple, par_boot)
# Save point estimates and standard errors
att_ny_simple <- aggte_simple$aggte_het1$overall.att - aggte_simple$aggte_het0$overall.att
att_ny_simple_se <- bootst_simple$se

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using  All not-Yet-treated as control (new estimator from the paper)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
out.nyall.violrate <- MarcusSantAnna2020::unc_att_gt_panel_het2(y_name ="violrate",
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
e_select <-  (ES_aggte$dif$egt >= 0) & ((ES_aggte$dif$egt <= maxe))
egt <- ES_aggte$egt[e_select]
# Get influence function for average of ES estimates
ES_inf_function <- ES_aggte$dif$inf.function[, e_select]
inf_func <- rowMeans(ES_inf_function)
# Do the bootstrap
par_boot <-  out.nyall.violrate$DIDparams
bootst_es <- did::mboot(inf_func, par_boot)
# Save point estimates and standard errors
att_nyall_time <- mean( ES_aggte$dif$att.egt[e_select])
att_nyall_time_se <- bootst_es$se

# Now do this for the "simple estimator"
aggte_simple <- MarcusSantAnna2020::aggte_het(out.nyall.violrate, type = "simple")
inf_func_simple <- aggte_simple$aggte_het1$inf.function$simple.att -
  aggte_simple$aggte_het0$inf.function$simple.att
bootst_simple <- did::mboot(inf_func_simple, par_boot)
# Save point estimates and standard errors
att_nyall_simple <- aggte_simple$aggte_het1$overall.att - 
  aggte_simple$aggte_het0$overall.att
att_nyall_simple_se <- bootst_simple$se

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Results using static TWFE
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
grooms$corrupt_after <- grooms$corrupt*grooms$after
#----------------------------------------------------------------------------
# All results now
twfe.violrate.corrupt = lfe::felm(violrate ~ after + corrupt_after| (state + year)| 0 | state,
                                  data = grooms, 
                                  exactDOF = T,
                                  weights = grooms$totfac)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Put all results in a table
#
summary.never.grooms <- rbind(
  att.simple.never.violrate = att_never_simple,
  att.simple.se.never.violrate = att_never_simple_se,
  att.dyn.never.violrate = att_never_time,
  att.dyn.se.never.violrate = att_never_time_se,
  twfe.violrate = twfe.violrate.corrupt$beta[2],
  twfe.se.violrate = twfe.violrate.corrupt$cse[2]
)

summary.ny.grooms <- rbind(
  att.simple.ny.violrate = att_ny_simple,
  att.simple.se.ny.violrate = att_ny_simple_se,
  att.dyn.ny.violrate = att_ny_time,
  att.dyn.se.ny.violrate = att_ny_time_se,
  twfe.violrate = twfe.violrate.corrupt$beta[2],
  twfe.se.violrate = twfe.violrate.corrupt$cse[2]
)

summary.nyall.grooms <- rbind(
  att.simple.ny.violrate = att_nyall_simple,
  att.simple.se.ny.violrate = att_nyall_simple_se,
  att.dyn.ny.violrate = att_nyall_time,
  att.dyn.se.ny.violrate = att_nyall_time_se,
  twfe.violrate = twfe.violrate.corrupt$beta[2],
  twfe.se.violrate = twfe.violrate.corrupt$cse[2]
)


summary.grooms <- cbind(summary.never.grooms,summary.ny.grooms, summary.nyall.grooms)


rownames(summary.grooms) <- c("ATT Simple - Violrate", "ATT Simple SE - Violrate",
                              "Averaged Dynamic ATT - Violrate", "Averaged Dynamic ATT SE - Violrate",
                              "TWFE - Violrate", "TWFE SE - Violrate")

colnames(summary.grooms) <- c("Never Treated", "Not-yet Treated","All Not-yet Treated")

xlsx::write.xlsx(summary.grooms, here("tables/grooms-corrupt-stronger-PTA.xlsx"))
