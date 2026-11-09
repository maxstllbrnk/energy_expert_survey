################################################################################
# district_heating.R — the mixed logit models of the recommendations, for the
#                      experts with the district-heating option (arm fw, seven
#                      outcomes)
# Inputs : experts.rds, vignettes.rds, reference_values.rds (CLEAN_DATA_DIR)
# Outputs: OUTPUT_DIR/district_heating/: summary_statistics/; per model, a folder
#          with results.rds, results.xlsx and model.rds; model_comparison/.
#          model.rds holds the data, so it stays on the secure drive.
# Checks : the sample at every step; one row per expert x vignette; known levels
#          and recommendations
################################################################################
# To add a variable: one line in make_regressors() and one term in a formula.
# make_regressors() then needs its source variable in every state, so `at` needs
# its reference value too; compute_ames() stops and names it. Its effect: a change.

source(file.path("R", "setup.R"))

# 1. Settings ------------------------------------------------------------------
est_dir <- file.path(OUTPUT_DIR, "district_heating")
# What the sample is, in the notes of its tables and on its deck: change it with the
# filters below
sample_label <- "Experts who completed, with the district-heating option and a group"
reference <- "Wärmepumpe"
# Alphabetical: this order sets the order of the random constants, so their
# Halton primes and the elements of L. Written out, as sort() depends on the
# language of the computer.
alternatives <- c("Fernwärme", "Gasheizung", "Hybridheizung",
                  "Keine Empfehlung", "Ölheizung", "Pelletheizung", "Wärmepumpe")

# 2. Data and sample -----------------------------------------------------------
experts_all <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes_all <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))
reference_values <- readRDS(file.path(CLEAN_DATA_DIR, "reference_values.rds"))

# One filter per step; to change the sample, add or delete such a pair of lines.
# Models with the group need one for every expert, and all models share one
# sample, so that their log-likelihoods can be compared.
sample_log <- count_sample(experts_all, "All persons", "resp_uid")
experts <- experts_all %>%
  filter(is_complete)
sample_log <- bind_rows(sample_log,
                        count_sample(experts, "Completed", "resp_uid"))
experts <- experts %>%
  filter(arm == "fw")
sample_log <- bind_rows(sample_log,
                        count_sample(experts, "Arm fw", "resp_uid"))
experts <- experts %>%
  filter(!is.na(expert_group))
sample_log <- bind_rows(sample_log,
                        count_sample(experts, "With a group", "resp_uid"))

vignettes <- vignettes_all %>%
  filter(resp_uid %in% experts$resp_uid)

# 3. Summary statistics of this sample -----------------------------------------
run_summary_statistics(experts, vignettes,
                       file.path(est_dir, "summary_statistics"), sample_label)

# 4. The regressors ------------------------------------------------------------
# Two placeholders. COP: the electricity price divided by it is the heat pump's
# price per kWh of heat, the same for both heat distributions for now (Max,
# 2026-10-19). The price is centred before it is divided: the part this leaves
# out, the mean price divided by the COP, differs only with the heat
# distribution, so the coefficients of floor_heating take it up if the two COPs
# differ. HYBRID_GAS_SHARE: the share of the heat from the gas boiler of a hybrid
# heating (Max, 2026-10-20).
COP <- c(radiators = 3, floor_heating = 3)
HYBRID_GAS_SHARE <- 0.5

# Where the answers are centred, the same in both estimation scripts, so that
# they share one reference expert. setNames(): the values, with these names.
prices <- reference_values$prices
expectations <- reference_values$expectations
price_mean <- setNames(prices$mean, prices$fuel)
price_q25 <- setNames(prices$q25, prices$fuel)
price_q75 <- setNames(prices$q75, prices$fuel)
rise_share <- setNames(expectations$share_rise, expectations$fuel)
heat_pump_share <- reference_values$own_heating[["Wärmepumpe"]]
hdd_mean <- reference_values$hdd$mean

#' From one row per state, with the source variables, to the regressors in one
#' row per state x alternative. A state is a decision here and a reference state
#' or a change in compute_ames(), so that every variable is coded once.
make_regressors <- function(states) {
  # A level outside the design or an unknown group would count as the reference
  for (attribute in unique(DESIGN_LEVELS$attribute)) {
    known_levels <- DESIGN_LEVELS$level[DESIGN_LEVELS$attribute == attribute]
    unknown_levels <- setdiff(states[[attribute]], known_levels)
    if (length(unknown_levels) > 0) {
      stop(attribute, " has unknown levels: ",
           paste(unknown_levels, collapse = ", "))
    }
  }
  stopifnot(all(states$expert_group %in% EXPERT_GROUPS))

  # (a) With one coefficient per technology: the attributes against their
  # reference levels; the group; the degree days in 1,000, 0 at the mean and for
  # an expert without nuts3, without a dummy (Max, 2026-10-27); a heat pump at
  # home, 0 at the share (Max, 2026-11-01).
  states <- states %>%
    mutate(
      age_60 = if_else(att_couple_age == "Mitte 60", 1, 0),
      income_low = if_else(att_income == "unter 40.000 € brutto im Jahr", 1, 0),
      heating_oil = if_else(att_current_heating == "Ölheizung", 1, 0),
      replacement_urgent = if_else(
        att_replacement_timing == "ist defekt und muss kurzfristig ersetzt werden",
        1, 0
      ),
      built_2000 = if_else(att_build_year == "2000", 1, 0),
      demand_70 = if_else(att_heat_demand == "70", 1, 0),
      demand_200 = if_else(att_heat_demand == "200", 1, 0),
      floor_heating = if_else(
        att_heat_distribution == "eine Fußbodenheizung", 1, 0
      ),
      is_adviser = if_else(expert_group == "Energy advisers", 1, 0),
      is_hvac = if_else(expert_group == "HVAC installers", 1, 0),
      is_sweep = if_else(expert_group == "Chimney sweeps", 1, 0),
      hdd_c = if_else(is.na(hdd), 0, (hdd - hdd_mean) / 1000),
      heat_pump_at_home_c = heat_pump_at_home - heat_pump_share
    )

  # (b) Each fuel's price and expected rise, centred at the mean and the share.
  # Not reported or blank: 0 with a dummy, which is the difference to an expert
  # at the mean or share.
  for (fuel in c("gas", "hpelec", "dh", "oil", "pellet")) {
    price <- states[[paste0("price_", fuel, "_kwh")]]
    rise <- states[[paste0("pexp_", fuel, "_rise")]]
    states[[paste0("price_", fuel, "_na")]] <- if_else(is.na(price), 1, 0)
    states[[paste0("price_", fuel, "_c")]] <-
      if_else(is.na(price), 0, price - price_mean[[fuel]])
    states[[paste0("pexp_", fuel, "_na")]] <- if_else(is.na(rise), 1, 0)
    states[[paste0("pexp_", fuel, "_c")]] <-
      if_else(is.na(rise), 0, rise - rise_share[[fuel]])
  }

  # (c) The heat pump per kWh of heat. The hybrid: the weighted average of gas
  # and the heat pump, not reported or blank if one of them is.
  states <- states %>%
    mutate(
      cop = if_else(floor_heating == 1, COP[["floor_heating"]],
                    COP[["radiators"]]),
      price_hpelec_heat_c = price_hpelec_c / cop,
      price_hybrid_na = if_else(price_gas_na == 1 | price_hpelec_na == 1, 1, 0),
      price_hybrid_c = if_else(
        price_hybrid_na == 1, 0,
        HYBRID_GAS_SHARE * price_gas_c +
          (1 - HYBRID_GAS_SHARE) * price_hpelec_heat_c
      ),
      pexp_hybrid_na = if_else(pexp_gas_na == 1 | pexp_hpelec_na == 1, 1, 0),
      pexp_hybrid_c = if_else(
        pexp_hybrid_na == 1, 0,
        HYBRID_GAS_SHARE * pexp_gas_c + (1 - HYBRID_GAS_SHARE) * pexp_hpelec_c
      )
    )

  # (d) One row per alternative. cross_join(): every state with every
  # alternative. With one coefficient for all technologies: what differs between
  # the alternatives, 0 in the rows of the others.
  states %>%
    cross_join(tibble(
      alternative = factor(alternatives, levels = alternatives)
    )) %>%
    mutate(
      price = case_when(
        alternative == "Gasheizung" ~ price_gas_c,
        alternative == "Ölheizung" ~ price_oil_c,
        alternative == "Pelletheizung" ~ price_pellet_c,
        alternative == "Wärmepumpe" ~ price_hpelec_heat_c,
        alternative == "Hybridheizung" ~ price_hybrid_c,
        alternative == "Fernwärme" ~ price_dh_c,
        TRUE ~ 0
      ),
      gas_price_na = if_else(alternative == "Gasheizung", price_gas_na, 0),
      oil_price_na = if_else(alternative == "Ölheizung", price_oil_na, 0),
      pellet_price_na = if_else(alternative == "Pelletheizung", price_pellet_na, 0),
      hpelec_price_na = if_else(alternative == "Wärmepumpe", price_hpelec_na, 0),
      hybrid_price_na = if_else(alternative == "Hybridheizung", price_hybrid_na, 0),
      dh_price_na = if_else(alternative == "Fernwärme", price_dh_na, 0),
      expects_rise = case_when(
        alternative == "Gasheizung" ~ pexp_gas_c,
        alternative == "Ölheizung" ~ pexp_oil_c,
        alternative == "Pelletheizung" ~ pexp_pellet_c,
        alternative == "Wärmepumpe" ~ pexp_hpelec_c,
        alternative == "Hybridheizung" ~ pexp_hybrid_c,
        alternative == "Fernwärme" ~ pexp_dh_c,
        TRUE ~ 0
      ),
      gas_rise_na = if_else(alternative == "Gasheizung", pexp_gas_na, 0),
      oil_rise_na = if_else(alternative == "Ölheizung", pexp_oil_na, 0),
      pellet_rise_na = if_else(alternative == "Pelletheizung", pexp_pellet_na, 0),
      hpelec_rise_na = if_else(alternative == "Wärmepumpe", pexp_hpelec_na, 0),
      hybrid_rise_na = if_else(alternative == "Hybridheizung", pexp_hybrid_na, 0),
      dh_rise_na = if_else(alternative == "Fernwärme", pexp_dh_na, 0)
    )
}

# 5. The data in mlogit's format -----------------------------------------------
# A vignette without a recommendation carries no choice; its expert stays with
# the other vignettes. Rows sorted by expert, then vignette, and alternatives
# alphabetical: this order sets the Halton draws of each expert. starts_with():
# the columns whose names begin so.
decisions <- vignettes %>%
  filter(!is.na(vig_rec)) %>%
  select(resp_uid, vig_num, vig_rec, starts_with("att_")) %>%
  left_join(experts, by = "resp_uid", relationship = "many-to-one") %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number())
check_unique(decisions, c("resp_uid", "vig_num"))
stopifnot(all(decisions$vig_rec %in% alternatives))
sample_log <- bind_rows(sample_log,
                        count_sample(decisions, "Decisions", "resp_uid"))
print(sample_log)

choice_long <- make_regressors(decisions) %>%
  mutate(chosen = as.character(vig_rec) == as.character(alternative)) %>%
  arrange(chid, alternative)
# dfidx(): the data indexed by decision (chid, nested in the expert, which
# panel = TRUE uses) and alternative
choice_data <- dfidx(choice_long,
                     idx = list(c("chid", "resp_uid"), "alternative"),
                     choice = "chosen", shape = "long")

# 6. The effects: the reference state and the changes --------------------------
# In the source variables of make_regressors(). The reference vignette and an
# expert with the mean prices and degree days, who expects no price to rise and
# has no heat pump at home (Max, 2026-10-30, 2026-11-01): each change switches on
# one answer.
at_neutral <- list(
  att_couple_age = "Mitte 40",
  att_income = "65.000 € brutto im Jahr",
  att_current_heating = "Gasheizung",
  att_replacement_timing = "muss in den nächsten Jahren ersetzt werden",
  att_build_year = "1970",
  att_heat_demand = "120",
  att_heat_distribution = "Heizkörper",
  price_gas_kwh = price_mean[["gas"]],
  price_hpelec_kwh = price_mean[["hpelec"]],
  price_dh_kwh = price_mean[["dh"]],
  price_oil_kwh = price_mean[["oil"]],
  price_pellet_kwh = price_mean[["pellet"]],
  pexp_gas_rise = 0,
  pexp_hpelec_rise = 0,
  pexp_dh_rise = 0,
  pexp_oil_rise = 0,
  pexp_pellet_rise = 0,
  hdd = hdd_mean,
  heat_pump_at_home = 0
)

# Each change sets `to` and, if it does not start from `at`, `from`
attribute_changes <- list(
  age_60 = list(to = list(att_couple_age = "Mitte 60")),
  income_low = list(to = list(att_income = "unter 40.000 € brutto im Jahr")),
  heating_oil = list(to = list(att_current_heating = "Ölheizung")),
  replacement_urgent = list(to = list(
    att_replacement_timing = "ist defekt und muss kurzfristig ersetzt werden"
  )),
  built_2000 = list(to = list(att_build_year = "2000")),
  demand_70 = list(to = list(att_heat_demand = "70")),
  demand_200 = list(to = list(att_heat_demand = "200")),
  floor_heating = list(to = list(
    att_heat_distribution = "eine Fußbodenheizung"
  ))
)
# Prices from the 25th to the 75th percentile (Max, 2026-10-29), so that the
# terms compare on one scale. A fuel moves every technology that uses it:
# electricity the heat pump and the hybrid, gas the gas heating and the hybrid.
price_changes <- list(
  price_gas = list(from = list(price_gas_kwh = price_q25[["gas"]]),
                   to = list(price_gas_kwh = price_q75[["gas"]])),
  price_hpelec = list(from = list(price_hpelec_kwh = price_q25[["hpelec"]]),
                      to = list(price_hpelec_kwh = price_q75[["hpelec"]])),
  price_dh = list(from = list(price_dh_kwh = price_q25[["dh"]]),
                  to = list(price_dh_kwh = price_q75[["dh"]])),
  price_oil = list(from = list(price_oil_kwh = price_q25[["oil"]]),
                   to = list(price_oil_kwh = price_q75[["oil"]])),
  price_pellet = list(from = list(price_pellet_kwh = price_q25[["pellet"]]),
                      to = list(price_pellet_kwh = price_q75[["pellet"]])),
  price_na_gas = list(to = list(price_gas_kwh = NA_real_)),
  price_na_hpelec = list(to = list(price_hpelec_kwh = NA_real_)),
  price_na_dh = list(to = list(price_dh_kwh = NA_real_)),
  price_na_oil = list(to = list(price_oil_kwh = NA_real_)),
  price_na_pellet = list(to = list(price_pellet_kwh = NA_real_))
)
expectation_changes <- list(
  expects_rise_gas = list(from = list(pexp_gas_rise = 0),
                          to = list(pexp_gas_rise = 1)),
  expects_rise_hpelec = list(from = list(pexp_hpelec_rise = 0),
                             to = list(pexp_hpelec_rise = 1)),
  expects_rise_dh = list(from = list(pexp_dh_rise = 0),
                         to = list(pexp_dh_rise = 1)),
  expects_rise_oil = list(from = list(pexp_oil_rise = 0),
                          to = list(pexp_oil_rise = 1)),
  expects_rise_pellet = list(from = list(pexp_pellet_rise = 0),
                             to = list(pexp_pellet_rise = 1)),
  rise_na_gas = list(to = list(pexp_gas_rise = NA_real_)),
  rise_na_hpelec = list(to = list(pexp_hpelec_rise = NA_real_)),
  rise_na_dh = list(to = list(pexp_dh_rise = NA_real_)),
  rise_na_oil = list(to = list(pexp_oil_rise = NA_real_)),
  rise_na_pellet = list(to = list(pexp_pellet_rise = NA_real_))
)
expert_changes <- list(
  heat_pump_at_home = list(from = list(heat_pump_at_home = 0),
                           to = list(heat_pump_at_home = 1)),
  hdd = list(from = list(hdd = reference_values$hdd$q25),
             to = list(hdd = reference_values$hdd$q75))
)

# 7. The models ----------------------------------------------------------------
# Each: the formula (the terms with one coefficient for all technologies before
# the |, those with one per technology after it), the estimation, the standard
# errors clustered by expert, the effects. If nothing changed, each step loads
# what it saved.

# (4) Random constants for all experts, correlated, whose means the groups shift
# (the energy advisers are the reference group); the attributes of the vignette.
# The benchmark without the expert's prices and characteristics.
formula_m4 <- chosen ~ 0 |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating
m4 <- fit_mixed_logit("m4", formula_m4, choice_data, reference, est_dir)
results_m4 <- post_estimation(
  m4, est_dir, label = "Random constants, correlated, with occupational group"
)
results_m4 <- compute_ames(
  results_m4, make_regressors, name = "neutral", at = at_neutral,
  changes = attribute_changes, profiles = EXPERT_GROUPS
)

# (4p) adds each technology's price per kWh and its dummy for a price not
# reported
formula_m4p <- chosen ~ price + gas_price_na + oil_price_na + pellet_price_na +
  hpelec_price_na + hybrid_price_na + dh_price_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating
m4p <- fit_mixed_logit("m4p", formula_m4p, choice_data, reference, est_dir)
results_m4p <- post_estimation(
  m4p, est_dir,
  label = "Random constants, correlated, with occupational group and prices"
)
results_m4p <- compute_ames(
  results_m4p, make_regressors, name = "neutral", at = at_neutral,
  changes = c(attribute_changes, price_changes), profiles = EXPERT_GROUPS
)

# (4pe), the main specification (Max, 2026-10-29), adds the expert's
# characteristics: the degree days and a heat pump at home per technology; each
# technology's expected rise and its dummy for a blank in Q17 with one
# coefficient. It starts from (4p), so that it cannot end below it.
# A dummy that is the same for every expert of the sample (all answered for that
# fuel, or none) has no coefficient, which make_regressors() cannot know:
# fit_mixed_logit() then stops and names it. Delete its term from this formula
# and from that of (6pe), and its change (rise_na_<fuel>) from the lists above:
# the change would still move the expected rise.
formula_m4pe <- chosen ~ price + gas_price_na + oil_price_na + pellet_price_na +
  hpelec_price_na + hybrid_price_na + dh_price_na +
  expects_rise + gas_rise_na + oil_rise_na + pellet_rise_na + hpelec_rise_na +
  hybrid_rise_na + dh_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating +
  hdd_c + heat_pump_at_home_c
m4pe <- fit_mixed_logit("m4pe", formula_m4pe, choice_data, reference, est_dir,
                        start_from = m4p)
results_m4pe <- post_estimation(
  m4pe, est_dir,
  label = paste("Random constants, correlated, with occupational group,",
                "prices and expert characteristics")
)
results_m4pe <- compute_ames(
  results_m4pe, make_regressors, name = "neutral", at = at_neutral,
  changes = c(attribute_changes, price_changes, expectation_changes,
              expert_changes),
  profiles = EXPERT_GROUPS
)

# (6pe) is (4pe) with one set of random constants per group, each with its own
# means and covariance matrix, as the robustness check of the main specification
# (Max, 2026-10-29). It starts where it predicts the same as (4pe).
formula_m6pe <- chosen ~ price + gas_price_na + oil_price_na + pellet_price_na +
  hpelec_price_na + hybrid_price_na + dh_price_na +
  expects_rise + gas_rise_na + oil_rise_na + pellet_rise_na + hpelec_rise_na +
  hybrid_rise_na + dh_rise_na |
  0 + is_hvac + is_sweep + is_adviser +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating +
  hdd_c + heat_pump_at_home_c
m6pe <- fit_mixed_logit("m6pe", formula_m6pe, choice_data, reference, est_dir,
                        group_constants = c("is_hvac", "is_sweep", "is_adviser"),
                        start_from = m4pe)
results_m6pe <- post_estimation(
  m6pe, est_dir,
  label = paste("Random constants by occupational group, correlated,",
                "with prices and expert characteristics")
)
results_m6pe <- compute_ames(
  results_m6pe, make_regressors, name = "neutral", at = at_neutral,
  changes = c(attribute_changes, price_changes, expectation_changes,
              expert_changes),
  profiles = EXPERT_GROUPS
)

# Another reference state, for the same model: an expert with average
# characteristics, at the share of each answer. Anything else works the same way,
# e.g. an unrenovated house: at_average$att_heat_demand <- "200". Other profiles
# than the groups: profiles = list("Advisers with a heat pump" =
# list(expert_group = "Energy advisers", heat_pump_at_home = 1)). A name no longer
# used stays in the results until post_estimation(force = TRUE) starts them anew.
at_average <- at_neutral
at_average$pexp_gas_rise <- rise_share[["gas"]]
at_average$pexp_hpelec_rise <- rise_share[["hpelec"]]
at_average$pexp_dh_rise <- rise_share[["dh"]]
at_average$pexp_oil_rise <- rise_share[["oil"]]
at_average$pexp_pellet_rise <- rise_share[["pellet"]]
at_average$heat_pump_at_home <- heat_pump_share
results_m4pe <- compute_ames(
  results_m4pe, make_regressors, name = "average", at = at_average,
  changes = attribute_changes, profiles = EXPERT_GROUPS
)

# 8. How the models compare ----------------------------------------------------
# The fixed protocol: compare_models(), the rule at the top of its file. The sheet
# `decision` of model_comparison.xlsx has one row per question. A model on the
# shortlist of the cross-validation is estimated five more times, on four sessions:
# a fold of (6pe) needs an estimated 3 to 4 GB per session.
comparison <- compare_models(
  est_dir,
  tests = tribble(
    ~question,                                 ~smaller, ~larger,
    "Do the prices matter?",                   "m4",     "m4p",
    "Do the expert characteristics matter?",   "m4p",    "m4pe",
    "Do the covariances differ by group?",     "m4pe",   "m6pe"
  ),
  blocks = list(
    prices = c("price", "gas_price_na", "oil_price_na", "pellet_price_na",
               "hpelec_price_na", "hybrid_price_na", "dh_price_na"),
    expected_rises = c("expects_rise", "gas_rise_na", "oil_rise_na",
                       "pellet_rise_na", "hpelec_rise_na", "hybrid_rise_na",
                       "dh_rise_na")
  ),
  final_model = "m4pe",
  ame_name = "neutral",
  cv_models = c("m4pe", "m6pe"),
  n_workers = min(4, N_WORKERS)
)
