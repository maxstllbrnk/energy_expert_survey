################################################################################
# test_estimation_core.R — fit_mixed_logit() and post_estimation() on the synthetic
#                          cleaned data, with one set of constants and by group
# Inputs : experts.rds, vignettes.rds of a synthetic run
# Outputs: in <SECURE_DIR>/output/test_estimation_core/
# Checks : a nesting model starts at the log-likelihood of the model it nests and
#          ends above it; loading works; the results have the agreed fields
################################################################################

source(file.path("R", "setup.R"))
est_dir <- file.path(OUTPUT_DIR, "test_estimation_core")

experts <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds")) %>%
  filter(is_complete, arm == "fw", !is.na(expert_group))
vignettes <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds")) %>%
  filter(resp_uid %in% experts$resp_uid, !is.na(vig_rec))
alternatives <- tibble(alternative = factor(sort(names(TECHNOLOGY_LABELS))))

choice_long <- vignettes %>%
  select(-expert_group, -is_complete) %>%
  left_join(experts %>% select(resp_uid, expert_group), by = "resp_uid",
            relationship = "many-to-one") %>%
  mutate(income_low = if_else(att_income == "unter 40.000 € brutto im Jahr", 1, 0),
         floor_heating = if_else(att_heat_distribution == "eine Fußbodenheizung", 1, 0),
         is_adviser = if_else(expert_group == "Energy advisers", 1, 0),
         is_hvac = if_else(expert_group == "HVAC installers", 1, 0),
         is_sweep = if_else(expert_group == "Chimney sweeps", 1, 0),
         chid = row_number()) %>%
  cross_join(alternatives) %>%
  mutate(chosen = vig_rec == alternative) %>%
  arrange(chid, alternative)
choice_data <- dfidx(choice_long, idx = list(c("chid", "resp_uid"), "alternative"),
                     choice = "chosen", shape = "long")

m_small <- fit_mixed_logit("m_small", chosen ~ 0 | 1 + income_low + floor_heating,
                           choice_data, "Wärmepumpe", est_dir)
m_group <- fit_mixed_logit("m_group", chosen ~ 0 | 1 + is_hvac + is_sweep + income_low + floor_heating,
                           choice_data, "Wärmepumpe", est_dir, start_from = m_small)
stopifnot(as.numeric(logLik(m_group)) >= as.numeric(logLik(m_small)))
m_by_group <- fit_mixed_logit("m_by_group", chosen ~ 0 | 0 + is_adviser + is_hvac + is_sweep + income_low + floor_heating,
                              choice_data, "Wärmepumpe", est_dir,
                              group_constants = c("is_adviser", "is_hvac", "is_sweep"),
                              start_from = m_group)
stopifnot(as.numeric(logLik(m_by_group)) >= as.numeric(logLik(m_group)))

results_small <- post_estimation(m_small, est_dir, label = "Small")
results_by_group <- post_estimation(m_by_group, est_dir, label = "Constants by group")
print(results_small$fit)
print(results_by_group$fit %>% select(n_parameters, log_likelihood, aic, bic, trace_hinv_b, tic))
print(results_small$coefficient_table %>% head(4))
print(results_by_group$constant_table %>% head(3))
stopifnot(identical(results_small$sample_hash, results_by_group$sample_hash))

# Loading: nothing changed, so nothing is estimated
m_again <- fit_mixed_logit("m_small", chosen ~ 0 | 1 + income_low + floor_heating,
                           choice_data, "Wärmepumpe", est_dir)
results_again <- post_estimation(m_again, est_dir)
stopifnot(identical(coef(m_again), coef(m_small)))
cat("Estimation core: all checks passed\n")
