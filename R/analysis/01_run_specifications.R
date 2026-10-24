################################################################################
# 01_run_specifications.R — which heating technology do experts recommend?
#                           Mixed logit models of the recommendations
# Inputs : survey_main (with the energy prices of Q15) and vignettes_long
#          (CLEAN_DATA_DIR), through load_analysis_data()
# Outputs: RC_LOGIT_MODEL_DIR/rc_logit_<arm>_ref_<outcome>/<model>.rds
#            the fitted model; it contains the estimation data, so it stays on
#            the secure drive
#          RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/<model>.rds
#            aggregate results only (Dropbox): coefficients with standard
#            errors clustered by expert, standard deviations and correlations
#            of the constants, AMEs with Krinsky-Robb standard errors, fit
# Checks : treatment of the reported prices; experts and decisions dropped by
#          each restriction; attribute values; no missing values; every
#          outcome chosen; price terms identified; design matrix of full rank;
#          per model: convergence, estimation sample, the Cholesky factor
#          against mlogit's
################################################################################
# Every model in MODELS (section 3) is estimated on the same sample, in four
# steps: (1) the mixed logit, (2) its covariance matrix clustered by expert,
# (3) Krinsky-Robb draws of the parameters, (4) the AMEs at the reference
# vignette with Krinsky-Robb standard errors. The functions are in
# 00_functions_estimation.R.
#
#   change a model         its entry in MODELS (section 3)
#   run only some models   MODELS_TO_RUN (section 3)
#   add a variable         create it in add_regressors() (section 2.3) or
#                          join it to the decisions (section 2.1), then add
#                          it to the regressors of a model

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
# who reported the mean).
in_groups <- experts_all$berufsgruppe %in% BERUFSGRUPPEN
price_reference <- tibble()
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

  price_mean <- mean(price[in_groups], na.rm = TRUE)
  experts_all[[paste0("price_", fuel, "_na")]] <- if_else(is.na(price), 1, 0)
  experts_all[[paste0("price_", fuel, "_c")]] <-
    if_else(is.na(price), 0, price - price_mean)

  price_reference <- bind_rows(price_reference, tibble(
    fuel = fuel, median = price_median, lower = lower, upper = upper,
    mean = price_mean
  ))
  price_treatment <- bind_rows(price_treatment, tibble(
    fuel = fuel, treatment = treatment[in_groups]
  ))
}

# Reference prices in cent per kWh, and the number of experts of the three
# Berufsgruppen by treatment of their answer
print(price_reference)
price_treatment %>%
  count(fuel, treatment) %>%
  pivot_wider(names_from = treatment, values_from = n, values_fill = 0) %>%
  print()

# all_of(): selects the columns named in a character vector
price_columns <- c(paste0("price_", PRICE_FUELS, "_na"),
                   paste0("price_", PRICE_FUELS, "_c"))
experts <- experts_all %>%
  select(resp_uid, all_of(price_columns))

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

#' The regressors of the models, from the vignette attributes (att_*) and the
#' Berufsgruppe. Used for the decisions here and for the vignettes of the AMEs
#' in section 4, so that both are coded the same way.
#'   <dummy>                       one per row of ATTRIBUTE_DUMMIES
#'   is_shk, is_schornsteinfeger,  one per Berufsgruppe
#'   is_energieberater
#'   is_<group>_x_<dummy>          Schornsteinfeger or Energieberater times an
#'                                 attribute dummy
add_regressors <- function(data) {
  for (k in seq_len(nrow(ATTRIBUTE_DUMMIES))) {
    is_level <- data[[ATTRIBUTE_DUMMIES$attribute[k]]] == ATTRIBUTE_DUMMIES$level[k]
    data[[ATTRIBUTE_DUMMIES$dummy[k]]] <- if_else(is_level, 1, 0)
  }
  data <- data %>%
    mutate(
      is_shk = if_else(berufsgruppe == "SHK-Handwerk", 1, 0),
      is_schornsteinfeger = if_else(berufsgruppe == "Schornsteinfeger", 1, 0),
      is_energieberater = if_else(berufsgruppe == "Energieberater", 1, 0)
    )
  for (group in c("is_schornsteinfeger", "is_energieberater")) {
    for (dummy in ATTRIBUTE_DUMMIES$dummy) {
      data[[paste0(group, "_x_", dummy)]] <- data[[group]] * data[[dummy]]
    }
  }
  data
}

decisions <- add_regressors(decisions)

# Prices per kWh of the heat pump and the hybrid heating (section 1). The heat
# pump's is the electricity price divided by the COP of the vignette's heat
# distribution. The hybrid's is the weighted average of the gas price and the
# heat pump's; it counts as not reported if the gas or the electricity price
# is not reported. An expert who reported the mean prices has 0 in every
# vignette.
decisions <- decisions %>%
  mutate(
    cop = if_else(floor_heating == 1, COP[["floor_heating"]], COP[["radiators"]]),
    price_hpelec_heat_c = price_hpelec_c / cop,
    price_hybrid_na = if_else(price_gas_na == 1 | price_hpelec_na == 1, 1, 0),
    price_hybrid_c = if_else(price_hybrid_na == 1, 0,
                             HYBRID_GAS_SHARE * price_gas_c +
                               (1 - HYBRID_GAS_SHARE) * price_hpelec_heat_c)
  )

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

# 2.4 Checks of the estimation sample ------------------------------------------
# (a) No missing values: mlogit() would drop such choice situations without
#     notice, and the models would no longer share one sample
model_variables <- c("vig_rec", ATTRIBUTE_DUMMIES$dummy, "is_shk",
                     "is_schornsteinfeger", "is_energieberater",
                     "price_gas_c", "price_oil_c", "price_pellet_c",
                     "price_hpelec_heat_c", "price_hybrid_c", "price_dh_c",
                     "price_gas_na", "price_oil_na", "price_pellet_na",
                     "price_hpelec_na", "price_hybrid_na", "price_dh_na")
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

# The price terms, which differ between the outcomes of a decision: in the
# rows of each technology, its price per kWh (`price`) and its dummy for a
# price not reported (<name>_price_na); 0 in the rows of the other outcomes.
# Per kWh of fuel for gas, oil and pellets, per kWh of heat for district
# heating and the heat pump, the weighted average for the hybrid. Fernwärme is
# an outcome in the fw arm only.
PRICE_VARIABLES <- tribble(
  ~technology,     ~name,    ~price,                ~not_reported,
  "Gasheizung",    "gas",    "price_gas_c",         "price_gas_na",
  "Ölheizung",     "oil",    "price_oil_c",         "price_oil_na",
  "Pelletheizung", "pellet", "price_pellet_c",      "price_pellet_na",
  "Wärmepumpe",    "hpelec", "price_hpelec_heat_c", "price_hpelec_na",
  "Hybridheizung", "hybrid", "price_hybrid_c",      "price_hybrid_na",
  "Fernwärme",     "dh",     "price_dh_c",          "price_dh_na"
) %>%
  filter(technology %in% RECOMMENDATIONS)

choice_long$price <- 0
is_priced <- rep(FALSE, nrow(choice_long))
for (k in seq_len(nrow(PRICE_VARIABLES))) {
  is_technology <- choice_long$alternative == PRICE_VARIABLES$technology[k]
  price_k <- choice_long[[PRICE_VARIABLES$price[k]]]
  not_reported_k <- choice_long[[PRICE_VARIABLES$not_reported[k]]]
  choice_long$price[is_technology] <- price_k[is_technology]
  dummy_name <- paste0(PRICE_VARIABLES$name[k], "_price_na")
  choice_long[[dummy_name]] <- if_else(is_technology, not_reported_k, 0)
  is_priced <- is_priced | is_technology

  # A dummy that is the same for every expert has no identified coefficient
  if (n_distinct(not_reported_k) < 2) {
    stop("Every expert has the same ", dummy_name, ": its coefficient is ",
         "not identified")
  }
}
if (n_distinct(choice_long$price[is_priced]) < 2) {
  stop("The price is the same in every row: its coefficient is not identified")
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
#   prices       price terms (section 2.5), each with one coefficient for all
#                technologies; character(0) for none
#   start_from   optional, for constants by Berufsgruppe: the same model with
#                one set of constants and the Berufsgruppe among the
#                regressors, whose estimates are the starting values
#                (start_from_common()). Estimate it first.

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

# The Berufsgruppe shifting the means of the constants; the SHK-Handwerk is
# the reference
GROUP_SHIFTS <- c("is_schornsteinfeger", "is_energieberater")

# One set of constants per Berufsgruppe
GROUP_CONSTANTS <- c("is_shk", "is_schornsteinfeger", "is_energieberater")

# The effects of the attributes for the Schornsteinfeger and the
# Energieberater, as differences to the SHK-Handwerk
GROUP_X_ATTRIBUTES <- c(paste0("is_schornsteinfeger_x_", ATTRIBUTES),
                        paste0("is_energieberater_x_", ATTRIBUTES))

# Each technology's price per kWh, with one coefficient, and its dummy for a
# price not reported
PRICES <- c("price", paste0(PRICE_VARIABLES$name, "_price_na"))

MODELS <- list(
  # (2) random constants for all experts
  m2 = list(
    label = "Random constants, correlated",
    constants = "(Intercept)",
    regressors = ATTRIBUTES,
    prices = character(0)
  ),
  # (4) main specification: as (2), with the Berufsgruppe shifting the means
  #     of the constants
  m4 = list(
    label = "Random constants, correlated, with occupational group",
    constants = "(Intercept)",
    regressors = c(GROUP_SHIFTS, ATTRIBUTES),
    prices = character(0)
  ),
  # (6) one set of random constants per Berufsgruppe, each with its own means
  #     and covariance matrix
  m6 = list(
    label = "Random constants by occupational group, correlated",
    constants = GROUP_CONSTANTS,
    regressors = ATTRIBUTES,
    prices = character(0),
    start_from = "m4"
  ),
  # (4i), (6i): (4) and (6), with the effects of the attributes differing by
  #     Berufsgruppe. In (6i) the Berufsgruppen share no parameter: a separate
  #     model per Berufsgruppe, estimated together.
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
  # (4p), (6p), (4ip): (4), (6) and (4i) with the energy prices
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
  )
)

# The models this run estimates, in this order; the others keep their saved
# results. A model with start_from comes after that model.
MODELS_TO_RUN <- c("m4", "m6", "m4i", "m6i", "m2", "m4p", "m6p", "m4ip")

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
  if (!is.null(specification$start_from)) {
    if (!identical(MODELS[[specification$start_from]]$constants, "(Intercept)")) {
      stop("Model ", model_name, " starts from ", specification$start_from,
           ", which must have one set of constants for all experts")
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
    check_convergence(model, n_choices)

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
    # expert of that group. If the model has no regressor that depends on
    # the Berufsgruppe, all are the same, and one profile, "All experts",
    # is enough.
    X_by_profile <- list()
    for (profile_name in BERUFSGRUPPEN) {
      profile_vignettes <- ame_vignettes %>%
        mutate(berufsgruppe = profile_name) %>%
        add_regressors()
      X <- design_matrix(profile_vignettes, columns)
      rownames(X) <- ame_vignettes$vignette
      X_by_profile[[profile_name]] <- X
    }
    is_same_for_all <- TRUE
    for (profile_name in BERUFSGRUPPEN) {
      if (!identical(X_by_profile[[profile_name]], X_by_profile[[1]])) {
        is_same_for_all <- FALSE
      }
    }
    if (is_same_for_all) {
      X_by_profile <- list("All experts" = X_by_profile[[1]])
    }

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
      choice_counts = choice_counts,
      experts_by_group = experts_by_group,
      price_reference = price_reference,
      price_settings = list(euro_below = PRICE_EURO_BELOW,
                            oil_per_litre = OIL_PER_LITRE,
                            pellets_per_kg = PELLETS_PER_KG,
                            valid_factor = PRICE_VALID_FACTOR,
                            cop = COP,
                            hybrid_gas_share = HYBRID_GAS_SHARE),
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
