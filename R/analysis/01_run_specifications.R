################################################################################
# 01_run_specifications.R — which heating technology do experts recommend?
#                           Mixed logit models of the recommendations
# Inputs : survey_main (with the energy prices of Q15, the expected price
#          developments of Q17, the heating at home of Q20 and nuts3) and
#          vignettes_long (CLEAN_DATA_DIR), through load_analysis_data();
#          heating degree days by NUTS3 region (PUBLIC_DATA_DIR)
# Outputs: RC_LOGIT_MODEL_DIR/rc_logit_<arm>_ref_<outcome>/<model>.rds
#            the fitted model; it contains the estimation data, so it stays on
#            the secure drive
#          RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/<model>.rds
#            aggregate results only (Dropbox): coefficients with standard
#            errors clustered by expert, standard deviations and correlations
#            of the constants, AMEs with Krinsky-Robb standard errors, fit,
#            convergence
#          RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/data_reference.rds
#            the reference values of the data of this run (means, shares and
#            quartiles of the expert characteristics), which
#            01_run_term_effects.R uses
# Checks : treatment of the reported prices; answers of Q17 and Q20; the join
#          of the degree days; experts and decisions dropped by each
#          restriction; attribute values; no missing values; every outcome
#          chosen, also with and without a heat pump at home; price terms
#          identified; design matrix and price terms of full rank; per model:
#          convergence, estimation sample, the Cholesky factor against mlogit's
################################################################################
# Every model in MODELS (section 3) is estimated on the same sample, in four
# steps: (1) the mixed logit, (2) its covariance matrix clustered by expert,
# (3) Krinsky-Robb draws of the parameters, (4) the AMEs at the reference
# vignette with Krinsky-Robb standard errors. The functions are in
# 00_functions_estimation.R.
#
#   change a model         its entry in MODELS (section 3)
#   run only some models   MODELS_TO_RUN (section 3)
#   add a variable         create it in add_regressors()
#                          (00_functions_estimation.R) or join it to the
#                          decisions (section 2.1), then add it to the
#                          regressors of a model

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

# load_analysis_data(), BERUFSGRUPPEN
source(file.path("R", "prepare_analysis_data.R"))
source(file.path("R", "analysis", "00_functions_estimation.R"))

# 1. Settings ------------------------------------------------------------------

# Halton draws per expert in the estimation
n_draws_model <- 1000

# Draws of the random constants used to average the predicted probabilities
# over experts (AMEs). The simulation error of one probability is about 1.1
# percentage points with 1,000 draws and about 0.1 with 100,000.
n_draws_ame <- 100000

# Draws of the parameter vector for the Krinsky-Robb standard errors and
# percentile intervals: with 5,000, each 2.5% tail rests on 125 draws (Max,
# 2026-10-24). One draw takes about 0.4 seconds per profile with 100,000 AME
# draws, so about 1.5 hours for a model with three Berufsgruppen.
n_draws_kr <- 5000

# Estimate for the experts who have "Fernwärme" in their choice set ("fw") or
# not ("nofw")
SELECTED_FW_ARM <- "fw"

# The outcome whose utility is normalised to 0
REFERENCE_OUTCOME <- "Wärmepumpe"

# The possible recommendations, seven in the fw arm and six in the nofw arm
# (no Fernwärme); the reference outcome comes first
if (SELECTED_FW_ARM == "fw") {
  RECOMMENDATIONS <- unique(
    c(REFERENCE_OUTCOME,
      "Wärmepumpe", "Pelletheizung", "Hybridheizung",
      "Gasheizung", "Ölheizung", "Fernwärme", "Keine Empfehlung"
    ))
} else {
  RECOMMENDATIONS <- unique(
    c(REFERENCE_OUTCOME,
      "Wärmepumpe", "Pelletheizung", "Hybridheizung",
      "Gasheizung", "Ölheizung", "Keine Empfehlung"
    ))
}
cat("Arm:", SELECTED_FW_ARM, "| reference outcome:", REFERENCE_OUTCOME, "\n")

# Vignette attributes. The reference level of each, exactly as in the data;
# together they make the reference vignette of the AMEs.
REFERENCE_LEVELS <- list(
  att_couple_age = "Mitte 40",
  att_income = "65.000 € brutto im Jahr",
  att_current_heating = "Gasheizung",
  att_replacement_timing = "muss in den nächsten Jahren ersetzt werden",
  att_build_year = "1970",
  att_heat_demand = "120",
  att_heat_distribution = "Heizkörper"
)

# One 0/1 dummy per other level, named in the column `dummy`
ATTRIBUTE_DUMMIES <- tribble(
  ~dummy,               ~attribute,               ~level,
  "age_60",             "att_couple_age",         "Mitte 60",
  "income_low",         "att_income",             "unter 40.000 € brutto im Jahr",
  "heating_oil",        "att_current_heating",    "Ölheizung",
  "replacement_urgent", "att_replacement_timing", "ist defekt und muss kurzfristig ersetzt werden",
  "built_2000",         "att_build_year",         "2000",
  "demand_70",          "att_heat_demand",        "70",
  "demand_200",         "att_heat_demand",        "200",
  "floor_heating",      "att_heat_distribution",  "eine Fußbodenheizung"
)

# Reported energy prices (Q15): what households among the expert's clients
# paid per kWh of each fuel in January and February 2026, in cent. The
# question asks experts to leave a field blank if none of their clients uses
# the fuel; a blank also covers "do not know". Answers in another unit are
# converted, in this order:
#   (1) below PRICE_EURO_BELOW: euro instead of cent, times 100
#   (2) oil within OIL_PER_LITRE (after (1)): cent per litre, divided by 10
#       (1 l of heating oil is about 10 kWh, as the question says)
#   (3) pellets within PELLETS_PER_KG (after (1)): cent per kg, divided by 5
#       (1 kg of pellets is about 5 kWh, as the question says)
#   (4) an answer more than PRICE_VALID_FACTOR times above or below the median
#       answer for its fuel cannot be read as a price per kWh: it counts as not
#       reported, like a blank
# The bounds are wide on purpose: the survey measures beliefs, so high and low
# answers are kept. Band of (2): Max, 2026-10-19. Band of (3): pellets cost
# about 400 euro per tonne, 40 cent per kg; it starts at 20, above plausible
# answers per kWh (median about 8 cent), so that these stay per kWh (Max,
# 2026-10-20). PRICE_VALID_FACTOR: first choice, to be confirmed.
PRICE_FUELS <- c("gas", "hpelec", "dh", "oil", "pellet")
PRICE_EURO_BELOW <- 1
OIL_PER_LITRE <- c(40, 140)
PELLETS_PER_KG <- c(20, 70)
PRICE_VALID_FACTOR <- 3

# Coefficient of performance of the heat pump: the electricity price divided by
# it is the price per kWh of heat. The same COP for both heat distributions for
# now (Max, 2026-10-19). The price is centred before it is divided: the part
# this leaves out, the mean price divided by the COP, differs only with the
# heat distribution, so the coefficients of floor_heating take it up if the
# two COPs differ.
COP <- c(radiators = 3, floor_heating = 3)

# Hybrid heating: its price per kWh is the weighted average of the gas price
# (weight HYBRID_GAS_SHARE, the share of the heat the gas boiler supplies) and
# the heat pump's price per kWh of heat. 0.5 is a placeholder (Max,
# 2026-10-20).
HYBRID_GAS_SHARE <- 0.5
stopifnot(HYBRID_GAS_SHARE >= 0, HYBRID_GAS_SHARE <= 1, all(COP > 0))

# Expected price developments (Q17): the price of each fuel of Q15 in five
# years, compared with the start of 2026. The answers exactly as in the data
# (note "10%" against "10 %"); the question asks experts to leave a fuel blank
# if it is not available in their area. An expert expects a rise if the answer
# is one of PEXP_RISE, both answers "höher" (Max, 2026-10-27). The question
# asks about the price, not the price after inflation: with inflation, "bis zu
# 10 % höher" is about flat in real terms.
PEXP_ANSWERS <- c("mehr als 10% niedriger", "bis zu 10 % niedriger",
                  "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher")
PEXP_RISE <- c("bis zu 10 % höher", "mehr als 10% höher")

# Heating of the building the expert lives in (Q20): the technologies of the
# recommendations, named the same, and "Sonstiges"
OWN_HEATING_ANSWERS <- c("Wärmepumpe", "Pelletheizung", "Fernwärme",
                         "Gasheizung", "Ölheizung", "Hybridheizung", "Sonstiges")

# Heating degree days of the expert's NUTS3 region (Eurostat, nrg_chddr2_a), of
# one year as in 05_rc_logit_explore_heterogeneity.R, in 1,000 degree days
# (Max, 2026-10-27). In 2025 the German regions range from 2,255 to 3,425.
HDD_YEAR <- 2025
HDD_UNIT <- 1000

# The minimum number of answers behind a statistic in any output (profile: 10
# answers, MIN_N of R/summary_statistics/00_settings.R): here the medians,
# means and quartiles of the reported prices and the quartiles of the degree
# days, which the results and data_reference.rds keep
MIN_ANSWERS <- 10

# 2. Data ----------------------------------------------------------------------
analysis_data <- load_analysis_data(completers_only = TRUE)
experts_all <- analysis_data$main
vignettes_all <- analysis_data$vignettes

# 2.1 Expert characteristics: the reported prices ------------------------------
# With the rules of section 1. The median and the mean of each fuel are taken
# over the experts of the three Berufsgruppen in both arms, so that every arm
# and reference outcome uses the same reference prices. price_<fuel>_na is 1
# if the price is blank or not usable; price_<fuel>_c is the price minus the
# mean, 0 if not reported (the dummy then captures the difference to an expert
# who reported the mean). price_<fuel>_lc is the log of the price minus the
# log of the mean, 0 if not reported: centred at the log of the mean, not at
# the mean of the logs, so that an expert who reported the mean price has 0 in
# both, and a price not reported counts as the mean in both.
in_groups <- experts_all$berufsgruppe %in% BERUFSGRUPPEN
price_reference <- tibble()
price_quartiles <- tibble()
price_treatment <- tibble()
for (fuel in PRICE_FUELS) {
  reported <- experts_all[[paste0("price_", fuel)]]
  treatment <- if_else(is.na(reported), "blank", "as reported")

  # (1) euro instead of cent
  is_euro <- !is.na(reported) & reported < PRICE_EURO_BELOW
  price <- if_else(is_euro, reported * 100, reported)
  treatment[is_euro] <- "euro"

  # (2) oil per litre, (3) pellets per kg
  is_per_unit <- rep(FALSE, length(price))
  if (fuel == "oil") {
    is_per_unit <- !is.na(price) &
      price >= OIL_PER_LITRE[1] & price <= OIL_PER_LITRE[2]
    price <- if_else(is_per_unit, price / 10, price)
  }
  if (fuel == "pellet") {
    is_per_unit <- !is.na(price) &
      price >= PELLETS_PER_KG[1] & price <= PELLETS_PER_KG[2]
    price <- if_else(is_per_unit, price / 5, price)
  }
  treatment[is_per_unit & !is_euro] <- "per litre or kg"
  treatment[is_per_unit & is_euro] <- "euro and per litre or kg"

  # (4) far from the median answer: not a price per kWh
  price_median <- median(price[in_groups], na.rm = TRUE)
  lower <- price_median / PRICE_VALID_FACTOR
  upper <- price_median * PRICE_VALID_FACTOR
  is_unusable <- !is.na(price) & (price < lower | price > upper)
  price[is_unusable] <- NA
  treatment[is_unusable] <- "not usable"
  n_usable <- sum(!is.na(price[in_groups]))
  if (n_usable < MIN_ANSWERS) {
    stop("Only ", n_usable, " usable prices of ", fuel, ": fewer than ",
         MIN_ANSWERS, " answers behind its mean and quartiles")
  }

  price_mean <- mean(price[in_groups], na.rm = TRUE)
  experts_all[[paste0("price_", fuel, "_na")]] <- if_else(is.na(price), 1, 0)
  experts_all[[paste0("price_", fuel, "_c")]] <-
    if_else(is.na(price), 0, price - price_mean)
  # The log of a price of 0 is -Inf, which the check for missing values in
  # section 2.4 would not catch
  if (any(price <= 0, na.rm = TRUE)) {
    stop("Usable prices of ", fuel, " at or below 0")
  }
  experts_all[[paste0("price_", fuel, "_lc")]] <-
    if_else(is.na(price), 0, log(price) - log(price_mean))

  price_reference <- bind_rows(price_reference, tibble(
    fuel = fuel, median = price_median, lower = lower, upper = upper,
    mean = price_mean
  ))
  # The 25th and 75th percentile of the usable prices, over the same experts
  # as the mean: the price changes of 01_run_term_effects.R (Max, 2026-10-29)
  quartiles <- quantile(price[in_groups], c(0.25, 0.75), na.rm = TRUE)
  price_quartiles <- bind_rows(price_quartiles, tibble(
    fuel = fuel, q25 = quartiles[[1]], q75 = quartiles[[2]]
  ))
  price_treatment <- bind_rows(price_treatment, tibble(
    fuel = fuel, treatment = treatment[in_groups]
  ))
}

# Reference prices in cent per kWh, and the number of experts of the three
# Berufsgruppen by treatment of their answer
print(price_reference)
print(price_quartiles)
price_treatment %>%
  count(fuel, treatment) %>%
  pivot_wider(names_from = treatment, values_from = n, values_fill = 0) %>%
  print()

# 2.1 Expert characteristics: the expected price rises (Q17) -------------------
# With the answers of section 1. pexp_<fuel>_na is 1 if the expert left the
# fuel blank. pexp_<fuel>_c is 1 if the expert expects its price to rise, 0 if
# not, minus the share of experts who expect a rise among those who answered;
# 0 if blank (the dummy then captures the difference to an expert with the
# average expectation). The mean of a 0/1 variable is its share of ones, so
# this centres it like the prices: 0 is an expert with average
# characteristics, for whom the AMEs are computed (not the average over the
# experts). As for the prices, the shares are taken over the experts of the
# three Berufsgruppen in both arms.
pexp_reference <- tibble()
for (fuel in PRICE_FUELS) {
  # as.character(): the answers as text, also if they are stored as a factor
  answer <- as.character(experts_all[[paste0("pexp_", fuel)]])
  unknown_answers <- setdiff(unique(answer[!is.na(answer)]), PEXP_ANSWERS)
  if (length(unknown_answers) > 0) {
    stop("pexp_", fuel, " has answers that are not in PEXP_ANSWERS: ",
         paste(unknown_answers, collapse = ", "))
  }
  expects_rise <- if_else(answer %in% PEXP_RISE, 1, 0)
  is_answered <- in_groups & !is.na(answer)
  rise_share <- mean(expects_rise[is_answered])
  experts_all[[paste0("pexp_", fuel, "_na")]] <- if_else(is.na(answer), 1, 0)
  experts_all[[paste0("pexp_", fuel, "_c")]] <-
    if_else(is.na(answer), 0, expects_rise - rise_share)
  pexp_reference <- bind_rows(pexp_reference, tibble(
    fuel = fuel, answered = sum(is_answered), share_rise = rise_share
  ))
}
# Experts of the three Berufsgruppen who answered for each fuel, and the share
# of them who expect a rise
print(pexp_reference)

# 2.1 Expert characteristics: the heating at home (Q20) ------------------------
# own_heating is the answer as text, "Keine Angabe" if missing; like
# "Sonstiges", it is none of the technologies. own_heating_share is the share
# of the experts of the three Berufsgruppen in both arms with each heating at
# home, those without an answer included. It centres the dummy "a heat pump
# heats the expert's home" (heat_pump_at_home, below) and the dummies "this
# technology heats the expert's home" (section 2.5), as the shares above
# centre the expected rises.
own_heating <- as.character(experts_all$dem_own_heating)
unknown_answers <- setdiff(unique(own_heating[!is.na(own_heating)]),
                           OWN_HEATING_ANSWERS)
if (length(unknown_answers) > 0) {
  stop("dem_own_heating has answers that are not in OWN_HEATING_ANSWERS: ",
       paste(unknown_answers, collapse = ", "))
}
experts_all$own_heating <- if_else(is.na(own_heating), "Keine Angabe",
                                   own_heating)
own_heating_share <- numeric(0)
for (answer_level in c(OWN_HEATING_ANSWERS, "Keine Angabe")) {
  own_heating_share[answer_level] <-
    mean(experts_all$own_heating[in_groups] == answer_level)
}
print(round(own_heating_share, 3))
# heat_pump_at_home: 1 if a heat pump heats the expert's home, else 0, minus
# the share of experts with a heat pump at home, so that 0 is an expert with
# average characteristics. A regressor like the degree days, with one
# coefficient per technology (Max, 2026-11-01). "Sonstiges" and no answer
# count as no heat pump.
experts_all$heat_pump_at_home <-
  if_else(experts_all$own_heating == "Wärmepumpe", 1, 0) -
  own_heating_share[["Wärmepumpe"]]

# 2.1 Expert characteristics: heating degree days ------------------------------
# Of the NUTS3 region of the expert's postcode (nuts3, R/cleaning/03_recode.R)
# in HDD_YEAR. hdd_c is the degree days minus their mean over the experts of
# the three Berufsgruppen in both arms, in HDD_UNIT degree days. An expert
# without nuts3 (no valid postcode, or a P.O.-box or large-customer postcode)
# has no degree days and gets the mean, 0, without a dummy (Max, 2026-10-27):
# they are few, and a dummy with one coefficient per technology would rest on
# too few decisions to be estimated. That decision covers experts without
# nuts3 only: a nuts3 without degree days in HDD_YEAR stops the script.
if (!("nuts3" %in% names(experts_all)) || all(is.na(experts_all$nuts3))) {
  stop("survey_main has no nuts3: rerun the cleaning (R/run_cleaning.R) ",
       "with PLZ_NUTS3_FILE in place")
}
hdd <- read.csv(file.path(PUBLIC_DATA_DIR, "hdd_cdd_nuts3.csv")) %>%
  filter(indic_nrg == "HDD", TIME_PERIOD == HDD_YEAR) %>%
  mutate(nuts3 = geo, hdd = OBS_VALUE) %>%
  select(nuts3, hdd)
# One row per region in hdd: relationship = "many-to-one" stops the join if a
# region appeared twice. A left join keeps the rows of experts_all in their
# order, so in_groups still refers to them.
n_experts_all <- nrow(experts_all)
experts_all <- experts_all %>%
  left_join(hdd, by = "nuts3", relationship = "many-to-one")
stopifnot(nrow(experts_all) == n_experts_all)
hdd_mean <- mean(experts_all$hdd[in_groups], na.rm = TRUE)
experts_all <- experts_all %>%
  mutate(hdd_c = if_else(is.na(hdd), 0, (hdd - hdd_mean) / HDD_UNIT))
# Experts of the three Berufsgruppen without nuts3, who get the mean
is_without_nuts3 <- is.na(experts_all$nuts3[in_groups])
is_without_hdd <- is.na(experts_all$hdd[in_groups])
if (any(!is_without_nuts3 & is_without_hdd)) {
  stop(sum(!is_without_nuts3 & is_without_hdd), " experts have a nuts3 ",
       "without degree days in ", HDD_YEAR, ": only experts without nuts3 get ",
       "the mean")
}
cat("Degree days, experts of the three Berufsgruppen: mean", round(hdd_mean),
    "| without nuts3", sum(is_without_nuts3), "| of", sum(in_groups), "\n")
# The 25th and 75th percentile over the experts with degree days: the change
# of the degree days in 01_run_term_effects.R (Max, 2026-10-29)
n_with_hdd <- sum(!is.na(experts_all$hdd[in_groups]))
if (n_with_hdd < MIN_ANSWERS) {
  stop("Only ", n_with_hdd, " experts with degree days: fewer than ",
       MIN_ANSWERS, " answers behind their quartiles")
}
hdd_quartiles <- quantile(experts_all$hdd[in_groups], c(0.25, 0.75),
                          na.rm = TRUE)
cat("Degree days: 25th percentile", round(hdd_quartiles[[1]]),
    "| 75th percentile", round(hdd_quartiles[[2]]), "\n")

# The reference values of the expert characteristics, saved with the results
expert_reference <- list(pexp_rise = PEXP_RISE, pexp = pexp_reference,
                         own_heating_share = own_heating_share,
                         hdd_year = HDD_YEAR, hdd_unit = HDD_UNIT,
                         hdd_mean = hdd_mean)

# all_of(): selects the columns named in a character vector
price_columns <- c(paste0("price_", PRICE_FUELS, "_na"),
                   paste0("price_", PRICE_FUELS, "_c"),
                   paste0("price_", PRICE_FUELS, "_lc"))
pexp_columns <- c(paste0("pexp_", PRICE_FUELS, "_na"),
                  paste0("pexp_", PRICE_FUELS, "_c"))
experts <- experts_all %>%
  select(resp_uid, all_of(price_columns), all_of(pexp_columns), own_heating,
         heat_pump_at_home, hdd_c)

n_vignettes_all <- nrow(vignettes_all)
decisions <- vignettes_all %>%
  left_join(experts, by = "resp_uid", relationship = "many-to-one")
stopifnot(nrow(decisions) == n_vignettes_all)

# 2.2 Sample -------------------------------------------------------------------
# (a) District heating is an option only in the fw arm (between-subject), so
#     each arm is estimated on its own: the one set in SELECTED_FW_ARM
decisions <- decisions %>%
  filter(vig_arm == SELECTED_FW_ARM)
if (nrow(decisions) == 0) stop("No vignettes for the arm ", SELECTED_FW_ARM)
cat("Arm", SELECTED_FW_ARM, ": experts", n_distinct(decisions$resp_uid),
    "| decisions", nrow(decisions), "\n")

# (b) The models with Berufsgruppe need one for every expert, so the filter
#     keeps the three Berufsgruppen: it drops "Keine Zuordnung" and the
#     experts who did not answer Q2 (berufsgruppe NA; %in% gives FALSE for NA).
#     It applies to every model, so that all are estimated on the same sample
#     and their log-likelihoods can be compared. Dropped, by reason:
dropped_berufsgruppe <- decisions %>%
  filter(!(berufsgruppe %in% BERUFSGRUPPEN)) %>%
  group_by(berufsgruppe) %>%
  summarise(experts = n_distinct(resp_uid), decisions = n())
print(dropped_berufsgruppe)

decisions <- decisions %>%
  filter(berufsgruppe %in% BERUFSGRUPPEN)
cat("With a Berufsgruppe: experts", n_distinct(decisions$resp_uid),
    "| decisions", nrow(decisions), "\n")

# (c) A vignette without a recommendation (not answered, vig_rec NA) carries
#     no choice. Its expert stays in the sample with their other vignettes.
cat("Vignettes without a recommendation:", sum(is.na(decisions$vig_rec)),
    "of", nrow(decisions), "\n")
decisions <- decisions %>%
  filter(!is.na(vig_rec))

# 2.3 Regressors ---------------------------------------------------------------
# Every attribute value must be its reference level or the level of one of
# its dummies; any other value (e.g. a different spelling) would silently
# count as the reference level
for (attribute in names(REFERENCE_LEVELS)) {
  known_levels <- c(REFERENCE_LEVELS[[attribute]],
                    ATTRIBUTE_DUMMIES$level[ATTRIBUTE_DUMMIES$attribute == attribute])
  unknown_levels <- setdiff(unique(decisions[[attribute]]), known_levels)
  if (length(unknown_levels) > 0) {
    stop(attribute, " has values that are not in REFERENCE_LEVELS or ",
         "ATTRIBUTE_DUMMIES: ", paste(unknown_levels, collapse = ", "))
  }
}

# The regressors of the models, from the vignette attributes and the
# Berufsgruppe (add_regressors(), 00_functions_estimation.R): the same
# function codes the vignettes of the AMEs in section 4 and of
# 01_run_term_effects.R
decisions <- add_regressors(decisions, ATTRIBUTE_DUMMIES)

# Prices per kWh of the heat pump and the hybrid heating, and the hybrid's
# expected rise, with the settings of section 1 (add_alternative_prices(),
# 00_functions_estimation.R, which says how)
decisions <- add_alternative_prices(decisions, COP, HYBRID_GAS_SHARE)

# Experts of the sample whose price counts as not reported (blank or not
# usable), by Berufsgruppe. The dummies are the same in all of an expert's
# vignettes, so distinct() keeps one row per expert.
decisions %>%
  distinct(resp_uid, berufsgruppe, price_gas_na, price_hpelec_na,
           price_hybrid_na, price_dh_na, price_oil_na, price_pellet_na) %>%
  group_by(berufsgruppe) %>%
  summarise(
    experts = n(),
    gas = sum(price_gas_na),
    hpelec = sum(price_hpelec_na),
    hybrid = sum(price_hybrid_na),
    dh = sum(price_dh_na),
    oil = sum(price_oil_na),
    pellet = sum(price_pellet_na)
  ) %>%
  print()

# Experts of the sample who left a fuel blank in Q17, by Berufsgruppe
decisions %>%
  distinct(resp_uid, berufsgruppe, pexp_gas_na, pexp_hpelec_na,
           pexp_hybrid_na, pexp_dh_na, pexp_oil_na, pexp_pellet_na) %>%
  group_by(berufsgruppe) %>%
  summarise(
    experts = n(),
    gas = sum(pexp_gas_na),
    hpelec = sum(pexp_hpelec_na),
    hybrid = sum(pexp_hybrid_na),
    dh = sum(pexp_dh_na),
    oil = sum(pexp_oil_na),
    pellet = sum(pexp_pellet_na)
  ) %>%
  print()

# 2.4 Checks of the estimation sample ------------------------------------------
# (a) No missing values: mlogit() would drop such choice situations without
#     notice, and the models would no longer share one sample
model_variables <- c("vig_rec", ATTRIBUTE_DUMMIES$dummy, "is_shk",
                     "is_schornsteinfeger", "is_energieberater",
                     "price_gas_c", "price_oil_c", "price_pellet_c",
                     "price_hpelec_heat_c", "price_hybrid_c", "price_dh_c",
                     "price_gas_na", "price_oil_na", "price_pellet_na",
                     "price_hpelec_na", "price_hybrid_na", "price_dh_na",
                     "price_gas_lc", "price_oil_lc", "price_pellet_lc",
                     "price_hpelec_heat_lc", "price_hybrid_lc", "price_dh_lc",
                     "pexp_gas_c", "pexp_oil_c", "pexp_pellet_c",
                     "pexp_hpelec_c", "pexp_hybrid_c", "pexp_dh_c",
                     "pexp_gas_na", "pexp_oil_na", "pexp_pellet_na",
                     "pexp_hpelec_na", "pexp_hybrid_na", "pexp_dh_na",
                     "own_heating", "heat_pump_at_home", "hdd_c")
n_missing <- numeric(0)
for (variable in model_variables) {
  n_missing[variable] <- sum(is.na(decisions[[variable]]))
}
if (any(n_missing > 0)) {
  print(n_missing[n_missing > 0])
  stop("Missing values in the estimation variables (counts above). ",
       "Restrict the sample visibly in section 2.2")
}

# (b) Every recommendation is one of RECOMMENDATIONS and chosen at least once;
#     otherwise its constant is not identified
unknown_outcomes <- setdiff(unique(decisions$vig_rec), RECOMMENDATIONS)
if (length(unknown_outcomes) > 0) {
  stop("Recommendations not in RECOMMENDATIONS: ",
       paste(unknown_outcomes, collapse = ", "))
}
choice_counts <- table(factor(decisions$vig_rec, levels = RECOMMENDATIONS))
if (any(choice_counts == 0)) {
  stop("Never chosen in this sample: ",
       paste(names(choice_counts)[choice_counts == 0], collapse = ", "))
}

# (c) One choice situation per expert x vignette. anyDuplicated(): the
#     position of the first row that repeats an earlier one, 0 if none.
if (anyDuplicated(decisions[c("resp_uid", "vig_num")]) > 0) {
  stop("More than one row per expert and vignette")
}
decisions <- decisions %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number())

n_choices <- nrow(decisions)
n_experts <- n_distinct(decisions$resp_uid)
cat("Estimation sample: experts", n_experts, "| choice situations", n_choices, "\n")
print(choice_counts)

# Experts per Berufsgruppe: each group's constants, and its interactions, are
# estimated from its experts alone
experts_by_group <- decisions %>%
  distinct(resp_uid, berufsgruppe) %>%
  count(berufsgruppe, name = "experts")
print(experts_by_group)

# 2.5 Data in mlogit's format --------------------------------------------------
# One row per decision x outcome; chosen is TRUE in the row of the
# recommended outcome. cross_join(): every row of the decisions with every
# outcome. The outcomes are in alphabetical order (sort()), as dfidx() orders
# them in wide data, the format of the policy brief and the earlier version
# of this script: the order of the outcomes sets the order of the random
# constants, so their Halton primes and the elements of L. mlogit() moves the
# reference outcome to the front.
alternatives <- tibble(alternative = factor(sort(RECOMMENDATIONS),
                                            levels = sort(RECOMMENDATIONS)))
choice_long <- decisions %>%
  cross_join(alternatives) %>%
  mutate(chosen = vig_rec == alternative) %>%
  arrange(chid, alternative)

# The terms that differ between the outcomes of a decision, with one
# coefficient for all technologies (long_terms(), 00_functions_estimation.R):
# the price terms, the price times the heat demand, the expected price rises
# (Q17) and the match of every heating at home (Q20; in no model at the
# moment). If the price mattered through the
# running cost, price times heat demand, the coefficients of the price times
# 70 and 200 kWh/m² would be 70/120 - 1 = -0.42 and 200/120 - 1 = 0.67 times
# the price coefficient.
# Which columns of the decisions feed each technology: its price per kWh, its
# log price and its dummy for a price not reported. Per kWh of fuel for gas,
# oil and pellets, per kWh of heat for district heating and the heat pump,
# the weighted average for the hybrid. Fernwärme is an outcome in the fw arm
# only.
PRICE_VARIABLES <- tribble(
  ~technology,     ~name,    ~price,                ~log_price,             ~not_reported,
  "Gasheizung",    "gas",    "price_gas_c",         "price_gas_lc",         "price_gas_na",
  "Ölheizung",     "oil",    "price_oil_c",         "price_oil_lc",         "price_oil_na",
  "Pelletheizung", "pellet", "price_pellet_c",      "price_pellet_lc",      "price_pellet_na",
  "Wärmepumpe",    "hpelec", "price_hpelec_heat_c", "price_hpelec_heat_lc", "price_hpelec_na",
  "Hybridheizung", "hybrid", "price_hybrid_c",      "price_hybrid_lc",      "price_hybrid_na",
  "Fernwärme",     "dh",     "price_dh_c",          "price_dh_lc",          "price_dh_na"
) %>%
  filter(technology %in% RECOMMENDATIONS)
# Its expected rise (centred, section 2.1) and its dummy for a blank in Q17
EXPECTATION_VARIABLES <- tribble(
  ~technology,     ~name,    ~rise,           ~not_answered,
  "Gasheizung",    "gas",    "pexp_gas_c",    "pexp_gas_na",
  "Ölheizung",     "oil",    "pexp_oil_c",    "pexp_oil_na",
  "Pelletheizung", "pellet", "pexp_pellet_c", "pexp_pellet_na",
  "Wärmepumpe",    "hpelec", "pexp_hpelec_c", "pexp_hpelec_na",
  "Hybridheizung", "hybrid", "pexp_hybrid_c", "pexp_hybrid_na",
  "Fernwärme",     "dh",     "pexp_dh_c",     "pexp_dh_na"
) %>%
  filter(technology %in% RECOMMENDATIONS)

choice_long <- long_terms(choice_long, PRICE_VARIABLES, EXPECTATION_VARIABLES,
                          own_heating_share, OWN_HEATING_ANSWERS,
                          RECOMMENDATIONS)

# A dummy for a price not reported that is the same for every expert has no
# identified coefficient, nor has the price if it is the same in every row
for (k in seq_len(nrow(PRICE_VARIABLES))) {
  if (n_distinct(choice_long[[PRICE_VARIABLES$not_reported[k]]]) < 2) {
    stop("Every expert has the same ", PRICE_VARIABLES$name[k], "_price_na: ",
         "its coefficient is not identified")
  }
}
is_priced <- choice_long$alternative %in% PRICE_VARIABLES$technology
if (n_distinct(choice_long$price[is_priced]) < 2) {
  stop("The price is the same in every row: its coefficient is not identified")
}

# A dummy for a blank in Q17 that is the same for every expert of the sample
# (every one answered, or none) has no coefficient: it is left out of
# rise_na_terms, the dummies the models use
rise_na_terms <- character(0)
for (k in seq_len(nrow(EXPECTATION_VARIABLES))) {
  dummy_name <- paste0(EXPECTATION_VARIABLES$name[k], "_rise_na")
  if (n_distinct(choice_long[[EXPECTATION_VARIABLES$not_answered[k]]]) == 2) {
    rise_na_terms <- c(rise_na_terms, dummy_name)
  } else {
    message("Every expert of the sample has the same ", dummy_name,
            ": left out of the models")
  }
}

# dfidx(): the data indexed by choice situation (chid, nested in the expert,
# resp_uid, which panel = TRUE uses) and outcome
choice_data <- dfidx(
  choice_long,
  idx = list(c("chid", "resp_uid"), "alternative"),
  choice = "chosen",
  shape = "long"
)

# 3. Models --------------------------------------------------------------------
# One entry per model; its name is the name of its files. In every model the
# constants are random, normal and correlated: with one set for all experts,
# with a full covariance matrix; with one set per Berufsgruppe, within the
# Berufsgruppe. All other coefficients are fixed.
#   label        name in tables and figures
#   constants    "(Intercept)": one constant per technology for all experts;
#                GROUP_CONSTANTS: one per technology and Berufsgruppe
#   regressors   variables of the decisions, each with one coefficient per
#                technology
#   prices       terms that differ between the technologies (section 2.5): the
#                price terms, and in (4pe) and (6pe) the expert
#                characteristics of EXPERT_TERMS; each with one coefficient
#                for all technologies; character(0) for none
#   start_from   optional: the model whose estimates are the starting values,
#                so that the estimation starts at its log-likelihood and
#                cannot end below it. For constants by Berufsgruppe, the same
#                model with one set of constants and the Berufsgruppe among
#                the regressors (start_from_common()); otherwise a model with
#                the same constants that this one nests, the added parameters
#                starting at 0 (start_from_nested()). Estimate it first, on
#                the same data and draws (checked).

# The building blocks. Vignette attributes: the dummies of section 1.
ATTRIBUTES <- c(
  "age_60",
  "income_low",
  "heating_oil",
  "replacement_urgent",
  "built_2000",
  "demand_70",
  "demand_200",
  "floor_heating"
)

# The Berufsgruppe shifting the means of the constants; the Energieberater
# are the reference group (Max, 2026-10-24). Another reference group: list
# the other two here and in GROUP_X_ATTRIBUTES.
GROUP_SHIFTS <- c("is_shk", "is_schornsteinfeger")

# One set of constants per Berufsgruppe
GROUP_CONSTANTS <- c("is_shk", "is_schornsteinfeger", "is_energieberater")

# The effects of the attributes for the SHK-Handwerk and the
# Schornsteinfeger, as differences to the Energieberater
GROUP_X_ATTRIBUTES <- c(paste0("is_shk_x_", ATTRIBUTES),
                        paste0("is_schornsteinfeger_x_", ATTRIBUTES))

# Each technology's price per kWh, with one coefficient, and its dummy for a
# price not reported
PRICES <- c("price", paste0(PRICE_VARIABLES$name, "_price_na"))

# For the variants of (4p). Interactions of the attributes (add_regressors(),
# section 2.3), each with one coefficient per technology: heat demand x build
# year (70 kWh/m² only, see there), heat demand x underfloor heating,
# underfloor heating x build year
ATTRIBUTE_INTERACTIONS <- c(
  "demand_70_x_built_2000",
  "demand_70_x_floor_heating",
  "demand_200_x_floor_heating",
  "floor_heating_x_built_2000"
)

# The price times the heat demand (section 2.5): the price coefficient is then
# the effect at 120 kWh/m²
PRICES_X_DEMAND <- c("price_x_demand_70", "price_x_demand_200")

# Each technology's log price instead of its price, with the same dummies for
# a price not reported
PRICES_LOG <- c("log_price", paste0(PRICE_VARIABLES$name, "_price_na"))

# Expert characteristics with one coefficient per technology, like the
# Berufsgruppe shifts: the heating degree days of the expert's region and a
# heat pump at home (section 2.1). The heat pump at home replaces the match
# of every heating at home with one coefficient for all technologies
# (own_heating_match), which mixed different situations (Max, 2026-11-01);
# how to treat the other heatings at home is open.
EXPERT_CHARACTERISTICS <- c("hdd_c", "heat_pump_at_home")

# Expert characteristics that differ between the technologies, with one
# coefficient, like the price (section 2.5): the expected price rise and the
# dummies for no answer on it
EXPERT_TERMS <- c("expects_rise", rise_na_terms)

MODELS <- list(
  # (2) random constants for all experts
  m2 = list(
    label = "Random constants, correlated",
    constants = "(Intercept)",
    regressors = ATTRIBUTES,
    prices = character(0)
  ),
  # (4) as (2), with the Berufsgruppe shifting the means of the constants: the
  #     benchmark without the expert's prices and characteristics
  m4 = list(
    label = "Random constants, correlated, with occupational group",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES),
    prices = character(0)
  ),
  # (4i), (6i): (4) with the effects of the attributes differing by
  #     Berufsgruppe; (6i) also with one set of random constants per
  #     Berufsgruppe, each with its own means and covariance matrix. In (6i)
  #     the Berufsgruppen share no parameter: a separate model per
  #     Berufsgruppe, estimated together. (6), the same without the
  #     interactions, was dropped (Max, 2026-10-29).
  m4i = list(
    label = "Random constants, correlated, with occupational group and interactions",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES, GROUP_X_ATTRIBUTES),
    prices = character(0)
  ),
  m6i = list(
    label = "Random constants by occupational group, correlated, with interactions",
    constants = GROUP_CONSTANTS,
    regressors = c(ATTRIBUTES, GROUP_X_ATTRIBUTES),
    prices = character(0),
    start_from = "m4i"
  ),
  # (4p), (6p), (4ip): (4), (4) with constants by Berufsgruppe, and (4i), with
  #     the energy prices
  m4p = list(
    label = "Random constants, correlated, with occupational group and prices",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES),
    prices = PRICES
  ),
  m6p = list(
    label = "Random constants by occupational group, correlated, with prices",
    constants = GROUP_CONSTANTS,
    regressors = ATTRIBUTES,
    prices = PRICES,
    start_from = "m4p"
  ),
  m4ip = list(
    label = "Random constants, correlated, with occupational group, interactions and prices",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES, GROUP_X_ATTRIBUTES),
    prices = PRICES
  ),
  # (4pa), (4pd), (4pl), (4pe): variants of (4p), with interactions of the
  #     attributes, with the price effect by heat demand, with log prices
  #     instead of prices, and with expert characteristics (heating degree
  #     days, heat pump at home, expected price rises). (4pe) is the main
  #     specification (Max, 2026-10-29). Those that nest (4p) start from it:
  #     (4pd) ended below (4p) without (run of 28 October 2026). (4pl) does
  #     not nest (4p).
  m4pa = list(
    label = "Random constants, correlated, with occupational group, prices and attribute interactions",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES, ATTRIBUTE_INTERACTIONS),
    prices = PRICES,
    start_from = "m4p"
  ),
  m4pd = list(
    label = "Random constants, correlated, with occupational group and prices by heat demand",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES),
    prices = c(PRICES, PRICES_X_DEMAND),
    start_from = "m4p"
  ),
  m4pl = list(
    label = "Random constants, correlated, with occupational group and log prices",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES),
    prices = PRICES_LOG
  ),
  m4pe = list(
    label = "Random constants, correlated, with occupational group, prices and expert characteristics",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES, EXPERT_CHARACTERISTICS),
    prices = c(PRICES, EXPERT_TERMS),
    start_from = "m4p"
  ),
  # (6pe): (4pe) with one set of random constants per Berufsgruppe, each with
  #     its own means and covariance matrix, as the robustness check of the
  #     main specification (Max, 2026-10-29)
  m6pe = list(
    label = "Random constants by occupational group, correlated, with prices and expert characteristics",
    constants = GROUP_CONSTANTS,
    regressors = c(ATTRIBUTES, EXPERT_CHARACTERISTICS),
    prices = c(PRICES, EXPERT_TERMS),
    start_from = "m4pe"
  )
)

# The models this run estimates, in this order; the others keep their saved
# results. A model with start_from comes after that model. All of them:
# c("m4", "m4i", "m6i", "m2", "m4p", "m6p", "m4ip",
#   "m4pa", "m4pd", "m4pl", "m4pe", "m6pe")
MODELS_TO_RUN <- c("m4pe", "m6pe")

# Check every model before the first estimation starts: its constants and
# regressors are columns of the decisions and linearly independent
# (qr()$rank: the number of linearly independent columns), its prices are
# price terms, and the model it starts from has one set of constants
if (!all(MODELS_TO_RUN %in% names(MODELS))) {
  stop("Not in MODELS: ",
       paste(setdiff(MODELS_TO_RUN, names(MODELS)), collapse = ", "))
}
for (model_name in names(MODELS)) {
  specification <- MODELS[[model_name]]
  X <- design_matrix(decisions, c(specification$constants,
                                  specification$regressors))
  if (qr(X)$rank < ncol(X)) {
    stop("Model ", model_name, ": the constants and regressors are linearly ",
         "dependent")
  }
  if (!all(specification$prices %in% names(choice_long))) {
    stop("Model ", model_name, ": prices that are not price terms")
  }
  # The price terms in the long data: linearly independent, and none 0 in
  # every row. For a fuel, the dummy for no answer on the expected rise could
  # be the same as the one for a price not reported; mlogit would then fail
  # only at the Hessian, after the estimation.
  if (length(specification$prices) > 0) {
    P <- as.matrix(choice_long[specification$prices])
    if (qr(P)$rank < ncol(P)) {
      stop("Model ", model_name, ": the price terms are linearly dependent, ",
           "or one is 0 in every row")
    }
  }
  if (!is.null(specification$start_from)) {
    start_specification <- MODELS[[specification$start_from]]
    if (!identical(start_specification$constants, "(Intercept)")) {
      stop("Model ", model_name, " starts from ", specification$start_from,
           ", which must have one set of constants for all experts")
    }
    # With one set of constants, the model must nest the one it starts from:
    # its regressors and prices among this model's
    is_nested <-
      all(start_specification$regressors %in% specification$regressors) &&
      all(start_specification$prices %in% specification$prices)
    if (identical(specification$constants, "(Intercept)") && !is_nested) {
      stop("Model ", model_name, " does not nest ", specification$start_from,
           ", which it starts from")
    }
    # A model that this run estimates after the model it starts from; match():
    # the position in MODELS_TO_RUN, NA if not there
    position <- match(model_name, MODELS_TO_RUN)
    position_start <- match(specification$start_from, MODELS_TO_RUN)
    if (!is.na(position) && !is.na(position_start) && position_start > position) {
      stop("MODELS_TO_RUN: put ", specification$start_from, " before ",
           model_name, ", which starts from it")
    }
  }
}

# A model with the heat pump at home has one coefficient of it per outcome. An
# outcome that no expert with a heat pump at home recommends, or only they
# recommend, has no finite coefficient, which would show only after the
# estimation: stop here, as for an outcome never chosen (section 2.4)
uses_heat_pump_at_home <- FALSE
for (model_name in MODELS_TO_RUN) {
  if ("heat_pump_at_home" %in% MODELS[[model_name]]$regressors) {
    uses_heat_pump_at_home <- TRUE
  }
}
if (uses_heat_pump_at_home) {
  has_heat_pump <- if_else(decisions$own_heating == "Wärmepumpe",
                           "heat pump at home", "no heat pump at home")
  heat_pump_counts <- table(factor(decisions$vig_rec, levels = RECOMMENDATIONS),
                            has_heat_pump)
  print(heat_pump_counts)
  if (any(heat_pump_counts == 0)) {
    stop("An outcome is never chosen by the experts with, or never by those ",
         "without, a heat pump at home (counts above): its coefficient of ",
         "heat_pump_at_home is not identified")
  }
}

# Vignettes of the AMEs: the reference vignette (every attribute at its
# reference level) and its variations, each with one attribute changed, one
# per attribute dummy. as_tibble(): the list as a table with one row.
reference_vignette <- as_tibble(REFERENCE_LEVELS)
ame_vignettes <- reference_vignette %>%
  mutate(vignette = "reference")
for (k in seq_len(nrow(ATTRIBUTE_DUMMIES))) {
  variation <- reference_vignette
  variation[[ATTRIBUTE_DUMMIES$attribute[k]]] <- ATTRIBUTE_DUMMIES$level[k]
  variation$vignette <- ATTRIBUTE_DUMMIES$dummy[k]
  ame_vignettes <- bind_rows(ame_vignettes, variation)
}

# 4. Estimate and save, one model at a time ------------------------------------
# The model is saved right after the estimation, so that a later step that
# fails does not discard it. A model that stops with an error is recorded,
# and the loop continues with the next.
run_dir <- run_folder(SELECTED_FW_ARM, REFERENCE_OUTCOME)
results_dir <- file.path(RC_LOGIT_DIR, run_dir, "results")
model_dir <- file.path(RC_LOGIT_MODEL_DIR, run_dir)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# The reference values of this run's data, for 01_run_term_effects.R: what
# the results of each model keep (the means and shares at which the expert
# characteristics are centred), the quartiles of the prices and degree days,
# and how the terms of section 2.5 are built. Aggregates only. Saved in every
# run, also one that estimates no model; 01_run_term_effects.R checks that the
# reference values are those of each model's results.
data_reference <- list(
  price_reference = price_reference,
  price_quartiles = price_quartiles,
  expert_reference = expert_reference,
  hdd_quartiles = c(q25 = hdd_quartiles[[1]], q75 = hdd_quartiles[[2]]),
  price_settings = list(cop = COP, hybrid_gas_share = HYBRID_GAS_SHARE),
  price_variables = PRICE_VARIABLES,
  expectation_variables = EXPECTATION_VARIABLES,
  own_heating_answers = OWN_HEATING_ANSWERS,
  n_experts = n_experts,
  n_choices = n_choices,
  date = Sys.Date()
)
saveRDS(data_reference, file.path(results_dir, "data_reference.rds"))

run_status <- tibble(model = MODELS_TO_RUN,
                     status = NA_character_,
                     minutes = NA_real_)

for (s in seq_along(MODELS_TO_RUN)) {
  model_name <- MODELS_TO_RUN[s]
  specification <- MODELS[[model_name]]
  message("\n==== Model ", s, " of ", length(MODELS_TO_RUN), ": ", model_name,
          " ====")
  start_time <- Sys.time()   # Sys.time(): the current date and time

  # Results of an earlier run of this model are removed first, so that they
  # cannot be read as current if this run stops with an error
  results_file <- file.path(results_dir, paste0(model_name, ".rds"))
  model_file <- file.path(model_dir, paste0(model_name, ".rds"))
  if (file.exists(results_file)) file.remove(results_file)
  if (file.exists(model_file)) file.remove(model_file)

  # tryCatch() runs the block; if a step stops with an error, it returns the
  # error message (conditionMessage()) instead, and the loop continues
  run_status$status[s] <- tryCatch({
    # 4.1 Mixed logit ----------------------------------------------------------
    start_model <- NULL
    if (!is.null(specification$start_from)) {
      start_file <- file.path(model_dir, paste0(specification$start_from, ".rds"))
      if (!file.exists(start_file)) {
        stop("Estimate ", specification$start_from, " first: the starting ",
             "values of ", model_name, " come from it")
      }
      start_model <- readRDS(start_file)
    }
    estimates <- estimate_mixed_logit(specification, choice_data,
                                      RECOMMENDATIONS, n_draws_model,
                                      start_model)
    rm(start_model)
    model <- estimates$model
    saveRDS(model, model_file)
    convergence <- check_convergence(model, n_choices)

    random <- estimates$random_coefficients
    fixed_parameters <- estimates$fixed_parameters
    theta_hat <- coef(model)
    # The formula as mlogit received it (showEnv = FALSE leaves out where R
    # stores it)
    print(estimates$formula, showEnv = FALSE)
    cat("Parameters:", length(theta_hat), "| random constants:", nrow(random),
        "| elements of L fixed at 0:", length(fixed_parameters), "\n")

    # Check: besides the elements of L, the parameters are exactly one per
    # column of the design matrix and technology, and the prices, so the AMEs
    # use every coefficient (setequal(): same elements, in any order)
    columns <- c(specification$constants, specification$regressors)
    technologies <- RECOMMENDATIONS[-1]
    is_cholesky <- startsWith(names(theta_hat), "chol.")
    expected_names <- c(as.vector(outer(columns, technologies, paste, sep = ":")),
                        specification$prices)
    if (!setequal(names(theta_hat)[!is_cholesky], expected_names)) {
      stop("mlogit's parameters are not the columns of the design matrix ",
           "times the technologies and the prices")
    }

    # Check: the Cholesky factor that the AMEs use reproduces mlogit's
    # covariance matrix of the constants, so the order and names of the
    # random constants are right. unclass(): cov.mlogit() as a plain matrix.
    L <- cholesky_factor(theta_hat, random, fixed_parameters)
    omega_mlogit <- unclass(cov.mlogit(model))[random$name, random$name]
    stopifnot(max(abs(L %*% t(L) - omega_mlogit)) < 1e-8)

    # 4.2 Covariance matrix clustered by expert --------------------------------
    robust <- vcov_cluster_robust(model, choice_data, cluster = "resp_uid",
                                  h = 1e-4)
    V <- robust$vcov
    coefficient_table <- tibble(
      parameter = names(theta_hat),
      estimate = as.numeric(theta_hat),
      se = sqrt(diag(V))
    ) %>%
      mutate(z = estimate / se,
             p = 2 * pnorm(-abs(z)))

    # 4.3 Krinsky-Robb draws of the parameters ---------------------------------
    # From N(theta_hat, V), the distribution behind the standard errors. The
    # same draws give the standard errors of the standard deviations and
    # correlations of the constants and of the AMEs. The seed is set right
    # here, so the draws do not depend on what ran before.
    set.seed(20261003)
    theta_draws <- MASS::mvrnorm(n_draws_kr, mu = theta_hat, Sigma = V)
    random_table <- random_constants_table(theta_hat, theta_draws, random,
                                           fixed_parameters)

    # 4.4 AMEs at the reference vignette ---------------------------------------
    # One design matrix per Berufsgruppe: the vignettes of section 3 for an
    # expert of that group, with the mean heating degree days (hdd_c = 0) and
    # the share of a heat pump at home (heat_pump_at_home = 0). The terms of
    # the prices are 0: the mean prices and, in (4pe) and (6pe), the shares of
    # the expected rises. For these two, 01_run_term_effects.R also computes
    # the AMEs for an expert who expects no price to rise and has no heat pump
    # at home, and 02_output.R reports those (Max, 2026-10-30; heat pump
    # 2026-11-01).
    # If the model has no regressor that depends on the Berufsgruppe, all are
    # the same, and one profile, "All experts", is enough
    # (design_by_profile()).
    vignettes_at_mean <- ame_vignettes %>%
      mutate(hdd_c = 0, heat_pump_at_home = 0)
    X_by_profile <- design_by_profile(vignettes_at_mean, BERUFSGRUPPEN, columns,
                                      ATTRIBUTE_DUMMIES)

    # Standard normal draws z_r of the random constants, one row per draw.
    # The same draws for all vignettes, profiles and Krinsky-Robb draws.
    set.seed(20261001)
    standard_draws <- matrix(rnorm(n_draws_ame * nrow(random)),
                             nrow = n_draws_ame, ncol = nrow(random))
    ames <- ame_krinsky_robb(theta_hat, theta_draws, X_by_profile,
                             standard_draws, random, RECOMMENDATIONS,
                             fixed_parameters)
    rm(standard_draws)

    # 4.5 Fit ------------------------------------------------------------------
    fit <- fit_statistics(model, choice_counts, n_experts, n_draws_model)
    print(fit %>%
            select(n_parameters, log_likelihood, mcfadden_r2, aic, bic))

    # 4.6 Save the aggregate results -------------------------------------------
    # Nothing at the level of experts or vignettes: the model stays in
    # model_dir. The formula is saved as text (deparse(): its text, in
    # pieces), because a formula object also saves the environment it was
    # made in, which holds the data.
    start_from_name <- NA_character_
    if (!is.null(specification$start_from)) {
      start_from_name <- specification$start_from
    }
    results <- list(
      name = model_name,
      specification = specification,
      arm = SELECTED_FW_ARM,
      reference_outcome = REFERENCE_OUTCOME,
      outcomes = RECOMMENDATIONS,
      formula = paste(deparse(estimates$formula), collapse = " "),
      reference_levels = REFERENCE_LEVELS,
      attribute_dummies = ATTRIBUTE_DUMMIES,
      random_coefficients = random,
      fixed_parameters = fixed_parameters,
      coefficients = theta_hat,
      vcov = V,
      inverse_hessian = robust$inverse_hessian,
      coefficient_table = coefficient_table,
      random_constants = random_table,
      ame_table = ames$table,
      probability_draws = ames$probability_draws,
      theta_draws = theta_draws,
      fit = fit,
      # code, iterations, largest gradient element; the model it started
      # from and the log-likelihood there (NA without start_from)
      convergence = c(
        convergence,
        list(start_from = start_from_name,
             log_likelihood_start = estimates$log_likelihood_start)
      ),
      choice_counts = choice_counts,
      experts_by_group = experts_by_group,
      price_reference = price_reference,
      price_settings = list(euro_below = PRICE_EURO_BELOW,
                            oil_per_litre = OIL_PER_LITRE,
                            pellets_per_kg = PELLETS_PER_KG,
                            valid_factor = PRICE_VALID_FACTOR,
                            cop = COP,
                            hybrid_gas_share = HYBRID_GAS_SHARE),
      expert_reference = expert_reference,
      draws = list(n_draws_model = n_draws_model, n_draws_ame = n_draws_ame,
                   n_draws_kr = n_draws_kr),
      date = Sys.Date()
    )
    saveRDS(results, results_file)
    "done"
  }, error = function(e) {
    paste("error:", conditionMessage(e))
  })

  # difftime(): the time between two points in time, here in minutes
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  run_status$minutes[s] <- round(minutes, 1)
  message("Model ", model_name, ": ", run_status$status[s],
          " (", run_status$minutes[s], " minutes)")
}

# 5. Summary of the run --------------------------------------------------------
print(run_status)
cat("Models written to", model_dir, "\n")
cat("Results written to", results_dir, "\n")
