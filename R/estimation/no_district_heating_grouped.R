################################################################################
# no_district_heating_grouped.R — no_district_heating.R with the gas and the oil
#                         heating as one outcome, priced by gas (arm nofw, five
#                         outcomes)
# Inputs : experts.rds, vignettes.rds, reference_values.rds (CLEAN_DATA_DIR)
# Outputs: OUTPUT_DIR/no_district_heating_grouped/: run_info.rds,
#          summary_statistics/; per model, a folder with results.rds,
#          results.xlsx and model.rds; model_comparison/. model.rds holds the
#          data, so it stays on the secure drive.
# Checks : one row per expert x vignette; known levels; every recommendation
#          has its outcome
################################################################################
# The same sample, models, effects and comparison as no_district_heating.R. Since
# 2026-11-16 this script is the main run of arm nofw, and no_district_heating.R,
# with the gas and the oil heating apart, is kept (Max). Its models have other
# outcomes, so compare_models() cannot compare them with these: the two scripts
# compare only descriptively, through the effects on the outcomes they share.
# To add a variable: one line in make_regressors() and one term in a formula.
# make_regressors() then needs its source variable in every state: at_neutral needs
# its reference value, and a variable of the expert goes into expert_variables
# (section 6), which both populations of the effects carry. Its effect: a change in
# one of the lists of section 6, and that list in changes_of_model (section 8) for
# every model that has the term.

source(file.path("R", "setup.R"))

# 1. Settings ------------------------------------------------------------------
est_dir <- file.path(OUTPUT_DIR, "no_district_heating_grouped")
# What the sample is, in the notes of its tables and on its deck: change it with the
# filters below. The same as in no_district_heating.R, as the sample is the same.
sample_label <- paste("Experts who completed, without the district-heating option, with a",
                      "group, age and year of training")
reference <- "Wärmepumpe"
# The outcomes of the models (Max, 2026-11-14): the gas and the oil heating as one,
# as oil is recommended too rarely for a coefficient of its own on every term (a
# term without variation among the oil recommendations stops fit_mixed_logit(), and
# oil's random constant was large and imprecise). The pellet and the hybrid heating
# stay apart: in (4pe) they react to the vignette's current oil heating in
# opposite directions, and they are priced differently. Grouped here from vig_rec,
# which stays as it is in vignettes.rds, as the grouping is a choice of the model,
# not of the cleaning. Written out, so that an answer missing here stops the
# script (section 5). The name of the grouped outcome: Claude's proposal, agreed by
# Max, 2026-11-16.
outcome_of_recommendation <- c(
  "Gasheizung" = "Gas- oder Ölheizung",
  "Ölheizung" = "Gas- oder Ölheizung",
  "Hybridheizung" = "Hybridheizung",
  "Keine Empfehlung" = "Keine Empfehlung",
  "Pelletheizung" = "Pelletheizung",
  "Wärmepumpe" = "Wärmepumpe"
)
# Alphabetical: this order sets the order of the random constants, so their
# Halton primes and the elements of L. Written out, as sort() depends on the
# language of the computer. The grouped outcome takes the place of the gas
# heating, so the outcomes of no_district_heating.R keep their primes, except the
# pellet heating.
alternatives <- c("Gas- oder Ölheizung", "Hybridheizung", "Keine Empfehlung",
                  "Pelletheizung", "Wärmepumpe")

# 2. Data and sample -----------------------------------------------------------
experts_all <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes_all <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))
reference_values <- readRDS(file.path(CLEAN_DATA_DIR, "reference_values.rds"))

# The sample, one condition per line; to change it, add or delete a line. Models
# with the group need one for every expert, and all models share one sample, so
# that their log-likelihoods can be compared; so the expert's covariates need an
# answer from every expert (Max, 2026-11-14): age and the year training was
# completed. The heat pump's share of their group is no longer required (Claude's
# proposal, agreed by Max, 2026-11-15): Q10b was not asked of offices that ticked
# only engineering or only "other", nor of advisers and sweeps with no household in
# Q10a (docs/data_contract.md), so a missing share stays, at the mean with a dummy
# (section 4). This widens the sample of every model by those experts. What it can
# now hit: the stop of fit_mixed_logit() on the dummy of one technology
# (hp_share_na:<technology>), if none of them recommends that technology; the deck
# of section 3 covers them too. The size of the sample is in the results of every
# model.
experts <- experts_all %>%
  filter(is_complete,
         arm == "nofw",
         !is.na(expert_group),
         !is.na(age),
         !is.na(years_since_training))
# The answers of Q17 as text, so that a change can set any of them. Without oil, as
# no outcome is priced by oil.
for (fuel in c("gas", "hpelec", "pellet")) {
  experts[[paste0("pexp_", fuel)]] <- as.character(experts[[paste0("pexp_", fuel)]])
}

vignettes <- vignettes_all %>%
  filter(resp_uid %in% experts$resp_uid)

# What this run is, for the output script (R/run_output.R): the label of the
# sample, its arm and whether the gas and the oil heating are one outcome
# (2026-11-16). A folder without it is left out of the exhibits.
dir.create(est_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(sample_label = sample_label, arm = "nofw", variant = "grouped"),
        file.path(est_dir, "run_info.rds"))

# 3. Summary statistics of this sample -----------------------------------------
run_summary_statistics(experts, vignettes,
                       file.path(est_dir, "summary_statistics"), sample_label)

# 4. The regressors ------------------------------------------------------------
# The prices (Claude's proposal, agreed by Max, 2026-11-15): each technology's price
# relative to the gas price, in logs, with one coefficient for all technologies;
# the gas heating and no recommendation have 0. A ratio counts a gap of 10% the
# same for every fuel, and a factor that scales all of an expert's prices (with or
# without VAT or the base fee, an old tariff) cancels. Until then each row held its
# own price in cent per kWh. The heat pump's price per kWh of heat is the
# electricity price divided by its COP: in logs the COP is a constant, which the
# constants of the technologies take up (and those of floor_heating, if it
# differed by heat distribution), so the models need none. The hybrid heating has
# no price of its own: it runs on the heat pump and uses gas only as a backup, so
# the electricity price and its expected rise enter its row as they enter the heat
# pump's (Max, 2026-11-12). The gas or oil heating is priced by gas alone, as oil
# makes up a small part of it (Max, 2026-11-14): its price relative to gas is 0,
# like the gas heating's in no_district_heating.R. Its expected change and its
# blank dummies are those of gas too, as the hybrid takes the heat pump's (Claude's
# proposal, agreed by Max, 2026-11-16). The oil price and Q17 for oil enter no
# model here.

# Where the answers are centred, the same in both estimation scripts, so that
# they share one reference expert. setNames(): the values, with these names. The
# expert's continuous covariates: 0 at their means; the heating at home and a
# course on heat pumps are not centred (section 4).
prices <- reference_values$prices
price_mean <- setNames(prices$mean, prices$fuel)
price_q25 <- setNames(prices$q25, prices$fuel)
price_q75 <- setNames(prices$q75, prices$fuel)
hdd_mean <- reference_values$hdd$mean
heating_group_share <- reference_values$own_heating_group
covariates <- reference_values$covariates
# Q17 in four categories, the two falls together, against "ungefähr gleich", not
# centred (Max, 2026-11-14). The answers exactly as in the data (note "10%" against
# "10 %").
expectation_category <- c(
  "mehr als 10% niedriger" = "fall", "bis zu 10 % niedriger" = "fall",
  "ungefähr gleich" = "no change",
  "bis zu 10 % höher" = "small rise", "mehr als 10% höher" = "large rise"
)

#' From one row per state, with the source variables, to the regressors in one
#' row per state x alternative. A state is a decision here and a reference state
#' or a change in compute_ames(), so that every variable is coded once.
make_regressors <- function(states) {
  # A level outside the design, an unknown group or an unknown answer would count
  # as the reference
  for (attribute in unique(DESIGN_LEVELS$attribute)) {
    known_levels <- DESIGN_LEVELS$level[DESIGN_LEVELS$attribute == attribute]
    unknown_levels <- setdiff(states[[attribute]], known_levels)
    if (length(unknown_levels) > 0) {
      stop(attribute, " has unknown levels: ",
           paste(unknown_levels, collapse = ", "))
    }
  }
  stopifnot(all(states$expert_group %in% EXPERT_GROUPS))
  stopifnot(all(states$own_heating_group %in% names(heating_group_share)))
  for (fuel in c("gas", "hpelec", "pellet")) {
    answers <- as.character(states[[paste0("pexp_", fuel)]])
    unknown_answers <- setdiff(answers[!is.na(answers)], names(expectation_category))
    if (length(unknown_answers) > 0) {
      stop("pexp_", fuel, " has unknown answers: ",
           paste(unknown_answers, collapse = ", "))
    }
  }

  # (a) With one coefficient per technology: the attributes against their
  # reference levels, and a house built in 2000 at 70 kWh, the one combination of
  # year and demand the design has (Max, 2026-11-14); the group; the degree days
  # in 1,000, 0 at the mean and for an expert without nuts3, without a dummy (Max,
  # 2026-10-27); a heat pump at home (Max, 2026-11-01) and, proposed by Claude in
  # the plan and agreed by Max (2026-11-14), the other heating at home against
  # conventional heating; the years since training and age in tens of years, 0 at
  # their means; the heat pump's share of the past year in tens of percentage
  # points, 0 at the mean of the group that answered its question and 0 for the
  # other group, and the chimney sweeps' own slope; a course on heat pumps
  # (hp_course as it is). A share of the expert's group that is missing, as its
  # question was not asked or not answered, is 0 too, with the dummy hp_share_na
  # (Claude's proposal, agreed by Max, 2026-11-15). The heating at home and the
  # course are 0 or 1, not centred, so that the constants are those of an expert
  # with conventional heating at home and no course, like the reference expert of
  # section 6 (Claude's proposal, agreed by Max, 2026-11-16; until then each was 0
  # at its share). Where a term is centred changes only the constants, not the
  # other coefficients, the probabilities or any effect.
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
      built_2000_x_demand_70 = built_2000 * demand_70,
      is_adviser = if_else(expert_group == "Energy advisers", 1, 0),
      is_hvac = if_else(expert_group == "HVAC installers", 1, 0),
      is_sweep = if_else(expert_group == "Chimney sweeps", 1, 0),
      hdd_c = if_else(is.na(hdd), 0, (hdd - hdd_mean) / 1000),
      own_heat_pump = if_else(own_heating_group == "Heat pump", 1, 0),
      own_renewable = if_else(own_heating_group == "Other renewable", 1, 0),
      own_other_heating = if_else(own_heating_group == "Other or not stated", 1, 0),
      years_since_training_c =
        (years_since_training - covariates$years_since_training$mean) / 10,
      expert_age_c = (age - covariates$age$mean) / 10,
      own_hp_share = if_else(is_hvac == 1, hp_share_installed, hp_share_recommended),
      hp_share_na = if_else(is.na(own_hp_share), 1, 0),
      hp_share_installed_c = if_else(
        is_hvac == 1 & hp_share_na == 0,
        (hp_share_installed - covariates$hp_share_installed$mean) / 10, 0
      ),
      hp_share_recommended_c = if_else(
        is_hvac == 0 & hp_share_na == 0,
        (hp_share_recommended - covariates$hp_share_recommended$mean) / 10, 0
      ),
      hp_share_recommended_sweep_c = hp_share_recommended_c * is_sweep
    )

  # (b) Each fuel's price, and its expected change in four categories against no
  # change, not centred (Max, 2026-11-14). A blank price counts as the fuel's mean,
  # also in the ratios of (c), and a blank in Q17 as no change, each with a dummy,
  # which is the difference to an expert at the mean price, or to one who expects
  # no change. unname(): the categories without the answers as names.
  for (fuel in c("gas", "hpelec", "pellet")) {
    price <- states[[paste0("price_", fuel, "_kwh")]]
    category <- unname(
      expectation_category[as.character(states[[paste0("pexp_", fuel)]])]
    )
    states[[paste0("price_", fuel, "_na")]] <- if_else(is.na(price), 1, 0)
    states[[paste0("price_", fuel, "_filled")]] <-
      if_else(is.na(price), price_mean[[fuel]], price)
    states[[paste0("pexp_", fuel, "_na")]] <- if_else(is.na(category), 1, 0)
    states[[paste0("rise_", fuel)]] <-
      if_else(category %in% c("small rise", "large rise"), 1, 0)
    states[[paste0("large_rise_", fuel)]] <- if_else(category %in% "large rise", 1, 0)
    states[[paste0("fall_", fuel)]] <- if_else(category %in% "fall", 1, 0)
  }

  # (c) Each fuel's price relative to the gas price, in logs, 0 at the mean prices
  for (fuel in c("hpelec", "pellet")) {
    states[[paste0("log_price_vs_gas_", fuel)]] <-
      log(states[[paste0("price_", fuel, "_filled")]] / states$price_gas_filled) -
      log(price_mean[[fuel]] / price_mean[["gas"]])
  }

  # (d) One row per alternative. cross_join(): every state with every
  # alternative. With one coefficient for all technologies: what differs between
  # the alternatives, 0 in the rows of the others. The price relative to gas is 0
  # in the rows of the gas or oil heating, so the gas price enters those of every
  # other technology. One blank dummy per fuel, also in the hybrid's rows: with
  # separate hybrid dummies, (4pe)'s Hessian was not positive definite in the arm
  # fw (Max, 2026-11-12).
  uses_electricity <- c("Wärmepumpe", "Hybridheizung")
  uses_gas <- c("Gas- oder Ölheizung")
  states %>%
    cross_join(tibble(
      alternative = factor(alternatives, levels = alternatives)
    )) %>%
    mutate(
      log_price_vs_gas = case_when(
        alternative == "Pelletheizung" ~ log_price_vs_gas_pellet,
        alternative %in% uses_electricity ~ log_price_vs_gas_hpelec,
        TRUE ~ 0
      ),
      gas_price_na = if_else(alternative %in% uses_gas, price_gas_na, 0),
      pellet_price_na = if_else(alternative == "Pelletheizung", price_pellet_na, 0),
      hpelec_price_na = if_else(alternative %in% uses_electricity,
                                price_hpelec_na, 0),
      expects_rise = case_when(
        alternative %in% uses_gas ~ rise_gas,
        alternative == "Pelletheizung" ~ rise_pellet,
        alternative %in% uses_electricity ~ rise_hpelec,
        TRUE ~ 0
      ),
      expects_rise_large = case_when(
        alternative %in% uses_gas ~ large_rise_gas,
        alternative == "Pelletheizung" ~ large_rise_pellet,
        alternative %in% uses_electricity ~ large_rise_hpelec,
        TRUE ~ 0
      ),
      expects_fall = case_when(
        alternative %in% uses_gas ~ fall_gas,
        alternative == "Pelletheizung" ~ fall_pellet,
        alternative %in% uses_electricity ~ fall_hpelec,
        TRUE ~ 0
      ),
      gas_rise_na = if_else(alternative %in% uses_gas, pexp_gas_na, 0),
      pellet_rise_na = if_else(alternative == "Pelletheizung", pexp_pellet_na, 0),
      hpelec_rise_na = if_else(alternative %in% uses_electricity,
                               pexp_hpelec_na, 0)
    )
}

# 5. The data in mlogit's format -----------------------------------------------
# A vignette without a recommendation carries no choice; its expert stays with
# the other vignettes. Rows sorted by expert, then vignette, and alternatives
# alphabetical: this order sets the Halton draws of each expert. starts_with():
# the columns whose names begin so. `outcome`: the recommendation, grouped as in
# section 1; NA for an answer the grouping does not name, which the check stops.
decisions <- vignettes %>%
  filter(!is.na(vig_rec)) %>%
  select(resp_uid, vig_num, vig_rec, starts_with("att_")) %>%
  left_join(experts, by = "resp_uid", relationship = "many-to-one") %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number(),
         outcome = unname(outcome_of_recommendation[as.character(vig_rec)]))
check_unique(decisions, c("resp_uid", "vig_num"))
stopifnot(all(decisions$outcome %in% alternatives))

choice_long <- make_regressors(decisions) %>%
  mutate(chosen = outcome == as.character(alternative)) %>%
  arrange(chid, alternative)
# dfidx(): the data indexed by decision (chid, nested in the expert, which
# panel = TRUE uses) and alternative
choice_data <- dfidx(choice_long,
                     idx = list(c("chid", "resp_uid"), "alternative"),
                     choice = "chosen", shape = "long")

# 6. The effects: the reference state and the changes --------------------------
# In the source variables of make_regressors(). The reference vignette and an
# expert with the mean prices and degree days, who expects no price to change and
# heats with gas or oil at home, the reference categories (Max, 2026-10-30,
# 2026-11-01 and 2026-11-14), with the mean years since training and age, the mean
# heat pump share of their group and no course on heat pumps (Claude's proposal,
# 2026-11-14, open): each change switches on one answer.
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
  price_pellet_kwh = price_mean[["pellet"]],
  pexp_gas = "ungefähr gleich",
  pexp_hpelec = "ungefähr gleich",
  pexp_pellet = "ungefähr gleich",
  hdd = hdd_mean,
  own_heating_group = "Conventional",
  years_since_training = covariates$years_since_training$mean,
  age = covariates$age$mean,
  hp_course = 0,
  hp_share_installed = covariates$hp_share_installed$mean,
  hp_share_recommended = covariates$hp_share_recommended$mean
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
  )),
  # Year and demand together, against the sum of built_2000 and demand_70: the
  # interaction of (4pe)_1 shows here (the plan's step 1, agreed by Max,
  # 2026-11-14). Every model has the change, as it moves terms they all have
  # (Claude's proposal, 2026-11-14, open).
  built_2000_demand_70 = list(
    from = list(att_build_year = "1970", att_heat_demand = "120"),
    to = list(att_build_year = "2000", att_heat_demand = "70")
  )
)
# Prices from the 25th to the 75th percentile (Max, 2026-10-29), so that the
# terms compare on one scale. A fuel moves every technology priced by it relative
# to gas: electricity the heat pump and the hybrid, and the gas price every
# technology other than the gas or oil heating (since 2026-11-15). No oil price.
price_changes <- list(
  price_gas = list(from = list(price_gas_kwh = price_q25[["gas"]]),
                   to = list(price_gas_kwh = price_q75[["gas"]])),
  price_hpelec = list(from = list(price_hpelec_kwh = price_q25[["hpelec"]]),
                      to = list(price_hpelec_kwh = price_q75[["hpelec"]])),
  price_pellet = list(from = list(price_pellet_kwh = price_q25[["pellet"]]),
                      to = list(price_pellet_kwh = price_q75[["pellet"]])),
  # Not reporting a price against reporting the mean, which moves only the dummy,
  # as a blank counts as the mean. Read as "price not known", not as an effect of
  # the price: it goes with how familiar an expert is with the technology (Claude's
  # proposal, agreed by Max, 2026-11-15). `from` is given, so that over observed
  # experts it is the same for every expert, whatever they answered
  # (docs/results_contract.md); at the reference expert it is `at` anyway.
  price_na_gas = list(from = list(price_gas_kwh = price_mean[["gas"]]),
                      to = list(price_gas_kwh = NA_real_)),
  price_na_hpelec = list(from = list(price_hpelec_kwh = price_mean[["hpelec"]]),
                         to = list(price_hpelec_kwh = NA_real_)),
  price_na_pellet = list(from = list(price_pellet_kwh = price_mean[["pellet"]]),
                         to = list(price_pellet_kwh = NA_real_))
)
# Each fuel's expected change in Q17, from "ungefähr gleich" to a rise of up to 10%
# (expects_rise_<fuel>), of more than 10%, to a fall or to a blank (the plan's step
# 2, agreed by Max, 2026-11-14), with `from` for the same reason as above. A model
# whose expected rise has two levels has the large rise too, as it moves its term,
# and there equals the rise of up to 10% (Claude's proposal, 2026-11-14, open). A
# fall moves none of its terms, so only the models with the four categories have
# it (fall_changes), as each model's sets hold the changes of its own terms (Max,
# 2026-11-14). setNames(): a list with the variable's name.
expectation_changes <- list()
fall_changes <- list()
for (fuel in c("gas", "hpelec", "pellet")) {
  variable <- paste0("pexp_", fuel)
  no_change <- setNames(list("ungefähr gleich"), variable)
  expectation_changes[[paste0("expects_rise_", fuel)]] <- list(
    from = no_change, to = setNames(list("bis zu 10 % höher"), variable)
  )
  expectation_changes[[paste0("expects_large_rise_", fuel)]] <- list(
    from = no_change, to = setNames(list("mehr als 10% höher"), variable)
  )
  expectation_changes[[paste0("rise_na_", fuel)]] <- list(
    from = no_change, to = setNames(list(NA_character_), variable)
  )
  fall_changes[[paste0("expects_fall_", fuel)]] <- list(
    from = no_change, to = setNames(list("bis zu 10 % niedriger"), variable)
  )
}
expert_changes <- list(
  heat_pump_at_home = list(from = list(own_heating_group = "Conventional"),
                           to = list(own_heating_group = "Heat pump")),
  hdd = list(from = list(hdd = reference_values$hdd$q25),
             to = list(hdd = reference_values$hdd$q75))
)
# The covariates of the steps of section 7, each change for the models that have
# its term (Max, 2026-11-14): the heating at home against conventional heating;
# the years since training, age and the heat pump shares from their 25th to their
# 75th percentile, a share only over the group that answered its question
# (proposed by Claude in the plan, agreed by Max, 2026-11-14)
heating_changes <- list(
  renewable_at_home = list(from = list(own_heating_group = "Conventional"),
                           to = list(own_heating_group = "Other renewable")),
  other_at_home = list(from = list(own_heating_group = "Conventional"),
                       to = list(own_heating_group = "Other or not stated"))
)
training_changes <- list(
  years_since_training = list(
    from = list(years_since_training = covariates$years_since_training$q25),
    to = list(years_since_training = covariates$years_since_training$q75)
  )
)
age_changes <- list(
  expert_age = list(from = list(age = covariates$age$q25),
                    to = list(age = covariates$age$q75))
)
course_changes <- list(
  hp_course = list(from = list(hp_course = 0), to = list(hp_course = 1))
)
share_changes <- list(
  hp_share_installed = list(
    from = list(hp_share_installed = covariates$hp_share_installed$q25),
    to = list(hp_share_installed = covariates$hp_share_installed$q75)
  ),
  hp_share_recommended = list(
    from = list(hp_share_recommended = covariates$hp_share_recommended$q25),
    to = list(hp_share_recommended = covariates$hp_share_recommended$q75)
  ),
  # No share against the mean share of the group, which moves only the dummy. Read
  # as "not asked Q10b (or Q11b), or no answer", not as an effect of the share
  # (Claude's proposal, agreed by Max, 2026-11-15). `from` is given, as for a
  # blank price.
  hp_share_na = list(
    from = list(hp_share_installed = covariates$hp_share_installed$mean,
                hp_share_recommended = covariates$hp_share_recommended$mean),
    to = list(hp_share_installed = NA_real_, hp_share_recommended = NA_real_)
  )
)

# The households of the effects over the observed experts: the reference vignette
# and three others (Max, 2026-11-12). The three others carry only the changes of
# the expert's terms, not those of the attributes (Claude's proposal, agreed by
# Max, 2026-11-16): an effect at a household differs from the main set only
# through the probabilities it starts from. For the expert's terms, that is what
# the households show; for the attributes, the main set and the answers show it
# already. A change to the level a household already has, or one that leaves
# the design (no house built in 2000 is unrenovated or has oil heating), is left
# out and blank (Max, 2026-11-14); at the reference household there is none. In
# every set, a heat pump share is blank for the groups
# that did not answer its question: over the experts because their share is
# missing, at the reference expert (neutral) because it moves none of their
# regressors (Claude's proposal, 2026-11-14, open). Over the experts, it also
# leaves out each expert whose own share is missing (2026-11-15). Every change of
# section 6 sets the same variables in `from` and `to`, so none stops on that.
at_reference <- at_neutral[grepl("^att_", names(at_neutral))]
at_inefficient <- at_reference
at_inefficient$att_current_heating <- "Ölheizung"
at_inefficient$att_heat_demand <- "200"
at_efficient <- at_reference
at_efficient$att_build_year <- "2000"
at_efficient$att_heat_demand <- "70"
at_efficient$att_heat_distribution <- "eine Fußbodenheizung"
at_old_poor <- at_reference
at_old_poor$att_couple_age <- "Mitte 60"
at_old_poor$att_income <- "unter 40.000 € brutto im Jahr"
households <- list(reference = at_reference, inefficient = at_inefficient,
                   efficient = at_efficient, old_poor = at_old_poor)

# Over every answer, each attribute change starts at the reference level, so that
# each answer adds the change from that level to the one named, whatever level it
# showed: the usual AME over the sample. With `to` alone, the answers already at
# the named level would be left out. An answer that a change takes outside the
# design is left out too: for built_2000 the 1970 houses with oil heating or at
# 200 kWh, for heating_oil and demand_200 the houses built in 2000.
attribute_changes_answers <- list()
for (effect in names(attribute_changes)) {
  variable <- names(attribute_changes[[effect]]$to)
  attribute_changes_answers[[effect]] <- list(
    from = at_neutral[variable],
    to = attribute_changes[[effect]]$to
  )
}

# The populations: the experts of the estimation sample, each with their own
# source variables of make_regressors(), and the answers they gave. A new variable
# of the expert goes into expert_variables. all_of(): the columns with these names.
expert_variables <- c("expert_group", "hdd", "own_heating_group",
                      "years_since_training", "age", "hp_course",
                      "hp_share_installed", "hp_share_recommended",
                      "price_gas_kwh", "price_hpelec_kwh", "price_pellet_kwh",
                      "pexp_gas", "pexp_hpelec", "pexp_pellet")
population_experts <- experts %>%
  filter(resp_uid %in% decisions$resp_uid) %>%
  select(resp_uid, all_of(expert_variables))
population_answers <- decisions %>%
  select(resp_uid, vig_num, all_of(expert_variables), starts_with("att_"))
# All experts pooled, and by group
profiles_observed <- list("All experts" = list())
for (group in EXPERT_GROUPS) {
  profiles_observed[[group]] <- list(expert_group = group)
}

# 7. The models ----------------------------------------------------------------
# Each: the formula (the terms with one coefficient for all technologies before
# the |, those with one per technology after it), the estimation and the standard
# errors clustered by expert; the effects follow in section 8. If nothing changed,
# each step loads what it saved.

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

# (4p) adds each technology's price relative to the gas price, in logs (section
# 4), and the dummy of each fuel for a price not reported
formula_m4p <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating
m4p <- fit_mixed_logit("m4p", formula_m4p, choice_data, reference, est_dir)
results_m4p <- post_estimation(
  m4p, est_dir,
  label = "Random constants, correlated, with occupational group and prices"
)

# (4pe), the main specification (Max, 2026-10-29), adds the expert's
# characteristics: the degree days and a heat pump at home per technology; each
# technology's expected rise and its dummy for a blank in Q17 with one
# coefficient. It starts from (4p), so that it cannot end below it.
# A dummy that is the same for every expert of the sample (all answered for that
# fuel, or none) has no coefficient, which make_regressors() cannot know:
# fit_mixed_logit() then stops and names it. Delete its term from this formula and
# those of the steps below, and its change (rise_na_<fuel>) from the lists above:
# the change would still move the expected rise.
formula_m4pe <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating +
  hdd_c + own_heat_pump
m4pe <- fit_mixed_logit("m4pe", formula_m4pe, choice_data, reference, est_dir,
                        start_from = m4p)
results_m4pe <- post_estimation(
  m4pe, est_dir,
  label = paste("Random constants, correlated, with occupational group,",
                "prices and expert characteristics")
)

# (6pe), the robustness check with random constants by group, is left out in this
# arm for now (2026-11-12), as in no_district_heating.R; for this script too
# (Claude's proposal, agreed by Max, 2026-11-16). To add it, copy its block
# from district_heating_grouped.R without the terms of Fernwärme, and add it to
# results_of_model, changes_of_model, cv_models and the tests below.

# The steps of the covariates (Max, 2026-11-14). (4pe)_k is (4pe) after step k:
# each adds one block to the model before and starts from it, so that it cannot
# end below it, and compare_models() tests each step (section 9). If
# fit_mixed_logit() stops on the term of one technology, e.g.
# built_2000_x_demand_70:Pelletheizung when no expert recommends pellets for a
# house built in 2000 at 70 kWh, the step needs a decision before it can run.
# Step 1: a house built in 2000 at 70 kWh, per technology, as year and demand may
# work as substitutes
formula_m4pe_1 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump
m4pe_1 <- fit_mixed_logit("m4pe_1", formula_m4pe_1, choice_data, reference, est_dir,
                          start_from = m4pe)
results_m4pe_1 <- post_estimation(
  m4pe_1, est_dir, label = "(4pe), step 1: a house built in 2000 at 70 kWh"
)

# Step 2: the expected change in four categories, with one coefficient for all
# technologies each: a rise of more than 10% on top of a rise, and a fall
formula_m4pe_2 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump
m4pe_2 <- fit_mixed_logit("m4pe_2", formula_m4pe_2, choice_data, reference, est_dir,
                          start_from = m4pe_1)
results_m4pe_2 <- post_estimation(
  m4pe_2, est_dir, label = "(4pe), step 2: the expected change in four categories"
)

# Step 3: the heating at home, other renewables and other or not stated, against
# conventional heating, next to a heat pump
formula_m4pe_3 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump + own_renewable + own_other_heating
m4pe_3 <- fit_mixed_logit("m4pe_3", formula_m4pe_3, choice_data, reference, est_dir,
                          start_from = m4pe_2)
results_m4pe_3 <- post_estimation(
  m4pe_3, est_dir, label = "(4pe), step 3: the heating at home in four groups"
)

# Step 4: the years since training was completed
formula_m4pe_4 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump + own_renewable + own_other_heating +
  years_since_training_c
m4pe_4 <- fit_mixed_logit("m4pe_4", formula_m4pe_4, choice_data, reference, est_dir,
                          start_from = m4pe_3)
results_m4pe_4 <- post_estimation(
  m4pe_4, est_dir, label = "(4pe), step 4: the years since training"
)

# Step 5: age, given the years since training
formula_m4pe_5 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump + own_renewable + own_other_heating +
  years_since_training_c + expert_age_c
m4pe_5 <- fit_mixed_logit("m4pe_5", formula_m4pe_5, choice_data, reference, est_dir,
                          start_from = m4pe_4)
results_m4pe_5 <- post_estimation(
  m4pe_5, est_dir, label = "(4pe), step 5: age, given the years since training"
)

# Step 6: a course on heat pumps in the last three years
formula_m4pe_6 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump + own_renewable + own_other_heating +
  years_since_training_c + expert_age_c + hp_course
m4pe_6 <- fit_mixed_logit("m4pe_6", formula_m4pe_6, choice_data, reference, est_dir,
                          start_from = m4pe_5)
results_m4pe_6 <- post_estimation(
  m4pe_6, est_dir, label = "(4pe), step 6: a course on heat pumps"
)

# Step 7: the heat pump's share of the past year, installed for HVAC installers and
# recommended for advisers and sweeps, each with its own slope, and the dummy for a
# missing share (2026-11-15), which the step's test adds with the slopes
formula_m4pe_7 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump + own_renewable + own_other_heating +
  years_since_training_c + expert_age_c + hp_course +
  hp_share_installed_c + hp_share_recommended_c + hp_share_na
m4pe_7 <- fit_mixed_logit("m4pe_7", formula_m4pe_7, choice_data, reference, est_dir,
                          start_from = m4pe_6)
results_m4pe_7 <- post_estimation(
  m4pe_7, est_dir, label = "(4pe), step 7: the heat pump share of the past year"
)

# Step 8: the chimney sweeps' own slope of the recommended share, on top of the
# advisers'
formula_m4pe_8 <- chosen ~ log_price_vs_gas +
  gas_price_na + pellet_price_na + hpelec_price_na +
  expects_rise + expects_rise_large + expects_fall +
  gas_rise_na + pellet_rise_na + hpelec_rise_na |
  1 + is_hvac + is_sweep +
  age_60 + income_low + heating_oil + replacement_urgent + built_2000 +
  demand_70 + demand_200 + floor_heating + built_2000_x_demand_70 +
  hdd_c + own_heat_pump + own_renewable + own_other_heating +
  years_since_training_c + expert_age_c + hp_course +
  hp_share_installed_c + hp_share_recommended_c + hp_share_na +
  hp_share_recommended_sweep_c
m4pe_8 <- fit_mixed_logit("m4pe_8", formula_m4pe_8, choice_data, reference, est_dir,
                          start_from = m4pe_7)
results_m4pe_8 <- post_estimation(
  m4pe_8, est_dir, label = "(4pe), step 8: the chimney sweeps' own share slope"
)

# 8. The effects of every model ------------------------------------------------
# For every model (Max, 2026-11-14), over the experts of the sample, by group and
# pooled:
#   observed_reference    the main set (MAIN_AMES in config.R): every expert at the
#                         reference vignette, with the draws of config.R, and its
#                         Krinsky-Robb draws saved
#   observed_<household>  the three other households, with the changes of the
#                         expert's terms only
#   answers               every answer the experts gave, the attribute changes only
#   neutral               one reference expert per group, to compare with earlier
#                         results
# The households and the answers with N_DRAWS_PREDICTION_OTHER prediction draws;
# neutral keeps the draws of config.R, as it takes about a minute. Only the main
# set keeps its Krinsky-Robb draws. Each model has the changes of its terms only,
# so that compare_models() counts an effect that only the larger model has as an
# added one. A set over the experts takes some hours in a full run. What a set
# means, and how to vary it: docs/results_contract.md.
results_of_model <- list(
  m4 = results_m4, m4p = results_m4p, m4pe = results_m4pe,
  m4pe_1 = results_m4pe_1, m4pe_2 = results_m4pe_2, m4pe_3 = results_m4pe_3,
  m4pe_4 = results_m4pe_4, m4pe_5 = results_m4pe_5, m4pe_6 = results_m4pe_6,
  m4pe_7 = results_m4pe_7, m4pe_8 = results_m4pe_8
)
changes_m4pe <- c(attribute_changes, price_changes, expectation_changes,
                  expert_changes)
changes_of_model <- list(
  m4 = attribute_changes,
  m4p = c(attribute_changes, price_changes),
  m4pe = changes_m4pe,
  m4pe_1 = changes_m4pe,
  m4pe_2 = c(changes_m4pe, fall_changes),
  m4pe_3 = c(changes_m4pe, fall_changes, heating_changes),
  m4pe_4 = c(changes_m4pe, fall_changes, heating_changes, training_changes),
  m4pe_5 = c(changes_m4pe, fall_changes, heating_changes, training_changes,
             age_changes),
  m4pe_6 = c(changes_m4pe, fall_changes, heating_changes, training_changes,
             age_changes, course_changes),
  m4pe_7 = c(changes_m4pe, fall_changes, heating_changes, training_changes,
             age_changes, course_changes, share_changes),
  m4pe_8 = c(changes_m4pe, fall_changes, heating_changes, training_changes,
             age_changes, course_changes, share_changes)
)
for (model_name in names(results_of_model)) {
  results <- results_of_model[[model_name]]
  changes <- changes_of_model[[model_name]]
  # The main set, under its name in config.R (observed_reference)
  results <- compute_ames(
    results, make_regressors, name = MAIN_AMES,
    at = households$reference, changes = changes,
    profiles = profiles_observed, population = population_experts
  )
  changes_expert <- changes[!names(changes) %in% names(attribute_changes)]
  for (household in c("inefficient", "efficient", "old_poor")) {
    results <- compute_ames(
      results, make_regressors, name = paste0("observed_", household),
      at = households[[household]], changes = changes_expert,
      profiles = profiles_observed, population = population_experts,
      n_draws = N_DRAWS_PREDICTION_OTHER, keep_draws = FALSE
    )
  }
  results <- compute_ames(
    results, make_regressors, name = "answers", at = list(),
    changes = attribute_changes_answers,
    profiles = profiles_observed, population = population_answers,
    n_draws = N_DRAWS_PREDICTION_OTHER, keep_draws = FALSE
  )
  results <- compute_ames(
    results, make_regressors, name = "neutral", at = at_neutral,
    changes = changes, profiles = EXPERT_GROUPS, keep_draws = FALSE
  )
}

# The main model by federal state, for the maps of the output script: every
# expert of the sample at the reference household, one profile per state that
# has experts (a profile that selects nobody stops compute_ames()), the
# probabilities only (Claude's proposal, agreed by Max, 2026-11-16). An expert
# without a state is in no profile. The main model is that of R/run_output.R;
# make_regressors() keeps the state as it is, as it is no source variable.
population_states <- population_experts %>%
  left_join(experts %>% select(resp_uid, bundesland), by = "resp_uid",
            relationship = "one-to-one")
profiles_states <- list()
for (state in names(STATE_LABELS)) {
  if (state %in% population_states$bundesland) {
    profiles_states[[state]] <- list(bundesland = state)
  }
}
results_m4pe <- compute_ames(
  results_m4pe, make_regressors, name = "observed_states",
  at = households$reference, changes = list(), profiles = profiles_states,
  population = population_states, n_draws = N_DRAWS_PREDICTION_OTHER,
  keep_draws = FALSE
)

# 9. How the models compare ----------------------------------------------------
# The fixed protocol: compare_models(), the rule at the top of its file. The sheet
# `decision` of model_comparison.xlsx has one row per question. Every model is
# cross-validated (Max, 2026-11-09): each is estimated five more times, on four
# sessions. A quick run (config.R) leaves the cross-validation out, and BIC
# decides instead. The AME step reads the main set for the three groups, without
# the pooled row, whose narrower intervals would let an added effect count on its
# own (Max, 2026-11-14).
cv_models <- c("m4", "m4p", "m4pe", paste0("m4pe_", 1:8))
if (QUICK_RUN) {
  cv_models <- NULL
}
comparison <- compare_models(
  est_dir,
  tests = tribble(
    ~question,                                                ~smaller, ~larger,
    "Do the prices matter?",                                  "m4",     "m4p",
    "Do the expert characteristics matter?",                  "m4p",    "m4pe",
    "Step 1: does the year change the effect of the demand?", "m4pe",   "m4pe_1",
    "Step 2: do the categories of the expected change matter?", "m4pe_1", "m4pe_2",
    "Step 3: does the heating at home matter beyond a heat pump?", "m4pe_2", "m4pe_3",
    "Step 4: do the years since training matter?",            "m4pe_3", "m4pe_4",
    "Step 5: does age matter, given the years since training?", "m4pe_4", "m4pe_5",
    "Step 6: does a course on heat pumps matter?",            "m4pe_5", "m4pe_6",
    "Step 7: does the heat pump share of the past year matter?", "m4pe_6", "m4pe_7",
    "Step 8: do chimney sweeps have their own share slope?",  "m4pe_7", "m4pe_8"
  ),
  blocks = list(
    prices = c("log_price_vs_gas", "gas_price_na", "pellet_price_na",
               "hpelec_price_na"),
    expected_rises = c("expects_rise", "gas_rise_na", "pellet_rise_na",
                       "hpelec_rise_na")
  ),
  final_model = "m4pe",
  ame_name = MAIN_AMES,
  ame_profiles = EXPERT_GROUPS,
  cv_models = cv_models,
  n_workers = min(4, N_WORKERS)
)
