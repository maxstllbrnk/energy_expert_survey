################################################################################
# 05_rc_logit_explore_heterogeneity.R — which heating technology do experts
#                  recommend? (Mixed) multinomial logit models with one set of
#                  constants (ASCs) per Berufsgruppe
# Inputs : survey_main (with nuts3 and the energy prices of Q15),
#          vignettes_long (CLEAN_DATA_DIR); heating and cooling degree days by
#          NUTS3 (PUBLIC_DATA_DIR)
# Outputs: none yet
# Checks : treatment of the reported prices by fuel; experts dropped by
#          Berufsgruppe; group dummies sum to 1; price variables complete and
#          their dummies identified; coefficient and Cholesky names;
#          convergence and estimation sample of each model
################################################################################

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source(file.path("R", "prepare_analysis_data.R"))   # load_analysis_data()

# 1. Settings ------------------------------------------------------------------

# Draws model estimation
n_draws_model <- 200

# Draws of the coefficient vector used to average the predicted probabilities
# over the distribution of the random coefficients (marginal effects). The
# simulation error of one probability is about 1.1 percentage points with
# 1,000 draws and about 0.1 with 100,000. Check 6.5 (e) shows the largest
# change between two independent sets of draws, which is somewhat larger.
n_draws_ame <- 1000

# Draws for AME Krinsky-Robb Standard Errors 
n_draws_kr <- 10000

# Estimate for the experts who have "Fernwärme" in their choice set ("fw") or
# not ("nofw")
SELECTED_FW_ARM <- "fw"

# The possible recommendations, seven in the fw arm and six in the nofw arm
# (no Fernwärme); the reference outcome comes first
REFERENCE_OUTCOME <- "Wärmepumpe"

# 03_run_all_specifications.R runs this script for several arms and reference
# outcomes: it passes them in RUN_SETTINGS, which then replaces the two
# values above. Run on its own, the script uses the values above.
if (exists("RUN_SETTINGS", inherits = FALSE)) {
  SELECTED_FW_ARM <- RUN_SETTINGS$arm
  REFERENCE_OUTCOME <- RUN_SETTINGS$reference
}
cat("Arm:", SELECTED_FW_ARM, "| reference outcome:", REFERENCE_OUTCOME, "\n")

if (SELECTED_FW_ARM == "fw"){
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

# Levels of the vignette attributes exactly as in the data; the first level is
# the reference category
ATTRIBUTE_LEVELS <- list(
  att_couple_age = c("Mitte 40", "Mitte 60"),
  att_income = c("65.000 € brutto im Jahr", "unter 40.000 € brutto im Jahr"),
  att_current_heating = c("Gasheizung", "Ölheizung"),
  att_replacement_timing = c("muss in den nächsten Jahren ersetzt werden",
                             "ist defekt und muss kurzfristig ersetzt werden"),
  att_build_year = c("1970", "2000"),
  att_heat_demand = c("120", "70", "200"),
  att_heat_distribution = c("Heizkörper", "eine Fußbodenheizung")
)

# Regressors. choice_formula is mlogit's notation: before `|` come attributes
# of the technologies (none: 0); after it come attributes of the vignette,
# which get one coefficient per technology, plus a constant per technology (1).
# design_formula lists the same attributes; it builds the design matrix for
# the marginal effects. A check in section 4 ties the two together.
choice_formula <- vig_rec ~ 0 | 1 + att_couple_age + att_income +
  att_current_heating + att_replacement_timing + att_build_year +
  att_heat_demand + att_heat_distribution

design_formula <- ~ att_couple_age + att_income + att_current_heating +
  att_replacement_timing + att_build_year + att_heat_demand +
  att_heat_distribution

# Reported energy prices (Q15): what households among the expert's clients paid
# per kWh of each fuel in January and February 2026, in cent. The question asks
# experts to leave a field blank if none of their clients uses the fuel; a
# blank also covers "do not know". Answers in another unit are converted, in
# this order:
#   (1) below PRICE_EURO_BELOW: euro instead of cent, times 100
#   (2) oil within OIL_PER_LITRE (after (1)): cent per litre, divided by 10
#       (1 l of heating oil is about 10 kWh, as the question says)
#   (3) pellets within PELLETS_PER_KG (after (1)): cent per kg, divided by 5
#       (1 kg of pellets is about 5 kWh, as the question says)
#   (4) an answer more than PRICE_VALID_FACTOR times above or below the median
#       answer for its fuel cannot be read as a price per kWh: it counts as not
#       reported, like a blank
# The bounds are wide on purpose: the survey measures beliefs, so high and low
# answers are kept. Bands of (2) and (3): Max, 2026-10-19. PRICE_VALID_FACTOR:
# first choice, to be confirmed.
PRICE_FUELS <- c("gas", "hpelec", "dh", "oil", "pellet")
PRICE_EURO_BELOW <- 1
OIL_PER_LITRE <- c(40, 140)
PELLETS_PER_KG <- c(10, 100)
PRICE_VALID_FACTOR <- 3

# Coefficient of performance of the heat pump: the electricity price divided by
# it is the price per kWh of heat. The same COP for both heat distributions for
# now (Max, 2026-10-19). A COP that is the same in every vignette only rescales
# the coefficients of the electricity price and leaves every other estimate
# unchanged; different values make the price of heat differ between vignettes.
COP <- c(radiators = 3, floor_heating = 3)

# The price that enters the utility of each technology, centred at the mean
# over the experts, and the dummy for a price not reported (blank or not
# usable). Each row gets its own two coefficients; the hybrid gets the gas and
# the electricity price. Fernwärme is an outcome in the fw arm only.
PRICE_TERMS <- tribble(
  ~term,           ~technology,     ~price,                ~not_reported,
  "gas",           "Gasheizung",    "price_gas_c",         "price_gas_na",
  "oil",           "Ölheizung",     "price_oil_c",         "price_oil_na",
  "pellet",        "Pelletheizung", "price_pellet_c",      "price_pellet_na",
  "hpelec",        "Wärmepumpe",    "price_hpelec_heat_c", "price_hpelec_na",
  "hybrid_gas",    "Hybridheizung", "price_gas_c",         "price_gas_na",
  "hybrid_hpelec", "Hybridheizung", "price_hpelec_heat_c", "price_hpelec_na",
  "dh",            "Fernwärme",     "price_dh_c",          "price_dh_na"
)
price_terms_arm <- PRICE_TERMS %>%
  filter(technology %in% RECOMMENDATIONS)

# 2. Data ----------------------------------------------------------------------
analysis_data <- load_analysis_data(completers_only = TRUE)
vignettes_all <- analysis_data$vignettes
experts_all <- analysis_data$main

# nuts3 comes with survey_main (R/cleaning/03_recode.R, from Eurostat's
# postcode list). survey_main files built before that change do not have it,
# and a cleaning run without the postcode list leaves it NA for everyone.
if (!("nuts3" %in% names(experts_all)) || all(is.na(experts_all$nuts3))) {
  stop("survey_main has no nuts3: rerun the cleaning (R/run_cleaning.R) ",
       "with PLZ_NUTS3_FILE in place")
}

hdd <- read.csv(file.path(PUBLIC_DATA_DIR, "hdd_cdd_nuts3.csv")) %>%
  filter(indic_nrg == "HDD", TIME_PERIOD == 2025) %>%
  mutate(nuts3 = geo,
         hdd_2025 = OBS_VALUE) %>%
  select(nuts3, hdd_2025)

cdd <- read.csv(file.path(PUBLIC_DATA_DIR, "hdd_cdd_nuts3.csv")) %>%
  filter(indic_nrg == "CDD", TIME_PERIOD == 2025) %>%
  mutate(nuts3 = geo,
         cdd_2025 = OBS_VALUE) %>%
  select(nuts3, cdd_2025)

# One row per NUTS3 region in hdd and cdd: relationship = "many-to-one" stops
# the join if a region appeared twice
n_experts_all <- nrow(experts_all)
experts_all <- experts_all %>%
  left_join(
    hdd, by = c("nuts3" = "nuts3"), relationship = "many-to-one"
  ) %>%
  left_join(
    cdd, by = c("nuts3" = "nuts3"), relationship = "many-to-one"
  )
stopifnot(nrow(experts_all) == n_experts_all)
# Experts without nuts3 (no valid postcode, or a P.O.-box or large-customer
# postcode) get no degree days; they would drop out of any model using them
cat("Experts without nuts3:", sum(is.na(experts_all$nuts3)),
    "| without hdd_2025:", sum(is.na(experts_all$hdd_2025)),
    "| of", n_experts_all, "\n")

# Reported prices, one row per expert, with the rules of section 1. The median
# and the mean of each fuel are taken over the experts of the three
# Berufsgruppen in both arms, so that every arm and reference outcome uses the
# same reference prices. price_<fuel>_na is 1 if the price is blank or not
# usable; price_<fuel>_c is the price minus the mean, 0 if not reported (the
# dummy then captures the difference to an expert who reported the mean).
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

price_columns <- c(paste0("price_", PRICE_FUELS, "_na"),
                   paste0("price_", PRICE_FUELS, "_c"))

# District heating is an option only in the fw arm (between-subject), so each
# arm is estimated on its own: the one set in SELECTED_FW_ARM
decisions <- vignettes_all %>%
  filter(vig_arm == SELECTED_FW_ARM)
n_decisions_arm <- nrow(decisions)
# all_of(): selects the columns named in a character vector
decisions <- decisions %>%
  left_join(
    experts_all %>% select(
      resp_uid, hdd_2025, all_of(price_columns)
    ), by = c("resp_uid" = "resp_uid"), relationship = "many-to-one"
  )
stopifnot(nrow(decisions) == n_decisions_arm)
if (nrow(decisions) == 0) stop("No vignettes for the arm ", SELECTED_FW_ARM)

# The filter below keeps the three Berufsgruppen. It drops "Keine Zuordnung"
# and also experts who did not answer Q2: filter() drops rows where the
# condition is NA. Number of experts dropped, by reason:
experts_dropped <- decisions %>%
  filter(is.na(berufsgruppe) | berufsgruppe == "Keine Zuordnung") %>%
  distinct(resp_uid, berufsgruppe) %>%
  count(berufsgruppe)
print(experts_dropped)

decisions <- decisions %>%
  select(resp_uid, vig_num, vig_rec, 
         berufsgruppe,
         att_couple_age, att_income,
         att_current_heating, att_replacement_timing, att_build_year,
         att_heat_demand, att_heat_distribution,
         all_of(price_columns)) %>%
  mutate(
    vig_rec = factor(vig_rec, levels = RECOMMENDATIONS),
    att_couple_age = factor(att_couple_age,
                            levels = ATTRIBUTE_LEVELS$att_couple_age),
    att_income = factor(att_income,
                        levels = ATTRIBUTE_LEVELS$att_income),
    att_current_heating = factor(att_current_heating,
                                 levels = ATTRIBUTE_LEVELS$att_current_heating),
    att_replacement_timing = factor(att_replacement_timing,
                                    levels = ATTRIBUTE_LEVELS$att_replacement_timing),
    att_build_year = factor(att_build_year,
                            levels = ATTRIBUTE_LEVELS$att_build_year),
    att_heat_demand = factor(att_heat_demand,
                             levels = ATTRIBUTE_LEVELS$att_heat_demand),
    att_heat_distribution = factor(att_heat_distribution,
                                   levels = ATTRIBUTE_LEVELS$att_heat_distribution),
    is_energieberater = if_else(berufsgruppe == "Energieberater", 1, 0),
    is_schornsteinfeger = if_else(berufsgruppe == "Schornsteinfeger", 1, 0),
    is_shk = if_else(berufsgruppe == "SHK-Handwerk", 1, 0)
  ) %>%
  filter(berufsgruppe != "Keine Zuordnung") %>%
  select(-berufsgruppe) %>%
  arrange(resp_uid, vig_num) %>%
  mutate(chid = row_number())   # one choice situation per expert x vignette

# Electricity price per kWh of heat: divided by the COP of the vignette's heat
# distribution. An expert who reported the mean price has 0 in every vignette.
decisions <- decisions %>%
  mutate(
    cop = if_else(att_heat_distribution == "eine Fußbodenheizung",
                  COP[["floor_heating"]], COP[["radiators"]]),
    price_hpelec_heat_c = price_hpelec_c / cop
  )

n_choices <- nrow(decisions)
n_experts <- n_distinct(decisions$resp_uid)
cat("Estimation sample: experts", n_experts, "| choice situations", n_choices, "\n")
choice_counts <- table(decisions$vig_rec)
print(choice_counts)

# The price variables must be complete: mlogit() would drop a choice situation
# with a missing value without notice. anyNA(): TRUE if any value in the
# columns is missing.
price_model_columns <- c(price_terms_arm$price, price_terms_arm$not_reported)
stopifnot(!anyNA(decisions[unique(price_model_columns)]))

# Experts of the estimation sample whose price counts as not reported (blank
# or not usable), by Berufsgruppe
experts_estimation <- experts_all %>%
  filter(resp_uid %in% decisions$resp_uid)
experts_estimation %>%
  group_by(berufsgruppe) %>%
  summarise(
    n_experts = n(),
    gas = sum(price_gas_na),
    hpelec = sum(price_hpelec_na),
    dh = sum(price_dh_na),
    oil = sum(price_oil_na),
    pellet = sum(price_pellet_na)
  ) %>%
  print()

# A dummy that is 0 for every expert, or 1 for every expert, has no
# identified coefficient
for (column in unique(price_terms_arm$not_reported)) {
  n_not_reported <- sum(experts_estimation[[column]])
  if (n_not_reported == 0 || n_not_reported == nrow(experts_estimation)) {
    stop("The dummy ", column, " is the same for every expert of the ",
         "estimation sample: its coefficient is not identified")
  }
}

# dfidx() reshapes the data to one row per choice situation x outcome, the
# format mlogit() needs; idx nests each choice situation (chid) within its
# expert (resp_uid), which panel = TRUE uses
choice_data <- dfidx(
  decisions,
  shape = "wide",
  choice = "vig_rec",
  idx = list(c("chid", "resp_uid")),
  idnames = c(NA, "alternatives")
)

# Each price enters only the utility of its own technology: the expert's price
# in that technology's row, 0 in the rows of the other outcomes (likewise the
# dummy). idx(choice_data, 2): the second index of the data, the outcome of
# each row. A column taken from dfidx data has the class "xseries", which
# if_else() does not combine with 0; as.numeric() makes it a plain number.
alternative <- as.character(idx(choice_data, 2))
price_terms <- character(0)
for (k in seq_len(nrow(price_terms_arm))) {
  is_technology <- alternative == price_terms_arm$technology[k]
  price <- as.numeric(choice_data[[price_terms_arm$price[k]]])
  not_reported <- as.numeric(choice_data[[price_terms_arm$not_reported[k]]])
  price_column <- paste0(price_terms_arm$term[k], "_price")
  dummy_column <- paste0(price_terms_arm$term[k], "_price_na")
  choice_data[[price_column]] <- if_else(is_technology, price, 0)
  choice_data[[dummy_column]] <- if_else(is_technology, not_reported, 0)
  price_terms <- c(price_terms, price_column, dummy_column)
}

# 3. Check vignette distribution -----------------------------------------------
ggplot(vignettes_all) +
  geom_histogram(bins = 128, aes(vignette_id)) 

ggplot(vignettes_all %>% filter(vig_arm == "fw")) +
  geom_histogram(bins = 128, aes(vignette_id))

ggplot(vignettes_all %>% filter(vig_arm != "fw")) +
  geom_histogram(bins = 128, aes(vignette_id))

vignette_weights <- vignettes_all %>%
  group_by(vignette_id) %>%
  summarise(
    count_all = n()
  ) %>%
  mutate(weight_all = count_all / sum(count_all)) %>%
  left_join(
    vignettes_all %>%
      filter(vig_arm == "fw") %>% 
      group_by(vignette_id) %>%
      summarise(
        count_fw = n()
      ) %>%
      mutate(weight_fw = count_fw / sum(count_fw)), by = c("vignette_id")
  ) %>%
  left_join(
    vignettes_all %>%
      filter(vig_arm != "fw") %>% 
      group_by(vignette_id) %>%
      summarise(
        count_no_fw = n()
      ) %>%
      mutate(weight_no_fw = count_no_fw / sum(count_no_fw)), by = c("vignette_id")
  )

summary(vignette_weights)

# The prices come before `|`: variables that differ between the outcomes, with
# one coefficient each. Each is non-zero for one technology only, so its
# coefficient is that technology's own. The formula is built as text because
# the price terms differ between the arms; as.formula() turns the text into a
# model formula.
choice_formula <- as.formula(paste(
  "vig_rec ~", paste(price_terms, collapse = " + "),
  "| 0 + is_shk + is_schornsteinfeger + is_energieberater +",
  "att_couple_age + att_income + att_current_heating +",
  "att_replacement_timing + att_build_year + att_heat_demand +",
  "att_heat_distribution"
))

# The group dummies replace the constant. "~ 0 + ..." would code the first
# factor (att_couple_age) with both levels, unlike mlogit, so the matrix is
# built with a constant and that column is dropped
design_formula <- ~ is_shk + is_schornsteinfeger + is_energieberater +
  att_couple_age + att_income + att_current_heating +
  att_replacement_timing + att_build_year + att_heat_demand +
  att_heat_distribution

group_dummy_sum <- decisions$is_shk + decisions$is_schornsteinfeger +
  decisions$is_energieberater
stopifnot(all(group_dummy_sum == 1))


# 4. Estimate models -----------------------------------------------------------
# (1) Multinomial logit
m_mnl <- mlogit(choice_formula, data = choice_data, reflevel = REFERENCE_OUTCOME)

# The regressors of the design matrix times the outcomes other than the
# reference, plus the price terms, must be exactly the coefficients of the
# model (setequal(): same elements, in any order)

#' Design matrix of design_formula without its constant column
#' @param data one row per vignette, with the is_* dummies and att_* factors
#' @return matrix, one column per regressor, named as in mlogit's coefficients
design_matrix <- function(data) {
  X <- model.matrix(design_formula, data)
  X[, colnames(X) != "(Intercept)", drop = FALSE]
}

terms <- colnames(design_matrix(decisions))
expected_coefs <- c(
  as.vector(outer(terms, RECOMMENDATIONS[RECOMMENDATIONS != REFERENCE_OUTCOME],
                  paste, sep = ":")),
  price_terms
)
stopifnot(setequal(expected_coefs, names(coef(m_mnl))))

# The group constants are random; the price coefficients do not start with
# "is_" and stay fixed
rpar_names <- names(coef(m_mnl))[startsWith(names(coef(m_mnl)), "is_")]

# (2) Mixed logit: the constants (ASCs) are normally distributed across experts,
# independently of each other; the coefficients of the attributes are fixed.
# Each expert keeps one draw for all six vignettes (panel = TRUE). halton = NA
# uses Halton draws with mlogit's default primes, n_draws_model per expert.
random_parameters <- rep("n", length(rpar_names))
names(random_parameters) <- rpar_names

m_mixed <- mlogit(choice_formula, data = choice_data,
                  reflevel = REFERENCE_OUTCOME, rpar = random_parameters,
                  panel = TRUE, halton = NA, R = n_draws_model)

# (3) Mixed logit with correlated random coefficients, correlated within each
# Berufsgruppe. Every expert belongs to one group only, so the covariances
# between the ASCs of two different groups do not enter any expert's
# likelihood: they are not identified and are set to 0 (a normalisation, like
# the reference outcome's ASC). In the Cholesky factor L of the covariance
# matrix, mlogit names element L[i, j] (i >= j) "chol.<ASC j>:<ASC i>", with
# the ASCs in the order of rpar_names (group by group). Setting the elements
# that link two groups to 0 makes L, and so the covariance matrix, block
# diagonal: one full covariance matrix per group.
group_of <- sub(":.*", "", rpar_names)   # "is_shk", "is_schornsteinfeger", ...
cross <- character(0)
for (i in seq_along(rpar_names)) {
  for (j in seq_len(i)) {
    if (group_of[i] != group_of[j]) {
      cross <- c(cross, paste0("chol.", rpar_names[j], ":", rpar_names[i]))
    }
  }
}

# Check that these are mlogit's names: a run with few draws and no iterations
# returns the names of all Cholesky elements
m_names <- mlogit(choice_formula, data = choice_data,
                  reflevel = REFERENCE_OUTCOME, rpar = random_parameters,
                  panel = TRUE, halton = NA, correlation = TRUE, R = 10,
                  iterlim = 0, print.level = 0)
chol_names <- names(coef(m_names))[startsWith(names(coef(m_names)), "chol.")]
n_groups <- length(unique(group_of))
n_asc_per_group <- length(rpar_names) / n_groups
n_within <- n_groups * n_asc_per_group * (n_asc_per_group + 1) / 2
stopifnot(all(cross %in% chol_names),
          length(cross) == length(chol_names) - n_within)
rm(m_names)

# constPar fixes the named parameters at the given values. mlogit() looks it up
# in the global environment, not where mlogit() is called, so when a runner
# script sources this script in its own environment an object defined here is
# not found. assign() puts the vector into the global environment; every run
# overwrites it just before estimation. The model stores the name, not the
# values: update(m_mixed_corr, ...) uses whatever the global object holds at
# that moment, so update a model only in the run that estimated it.
cholesky_fixed_at_zero <- rep(0, length(cross))
names(cholesky_fixed_at_zero) <- cross
assign("cholesky_fixed_at_zero", cholesky_fixed_at_zero, envir = globalenv())

m_mixed_corr <- mlogit(choice_formula, data = choice_data,
                       reflevel = REFERENCE_OUTCOME, rpar = random_parameters,
                       panel = TRUE, halton = NA, correlation = TRUE,
                       R = n_draws_model,
                       constPar = cholesky_fixed_at_zero)

# The fixed elements are not in coef(m_mixed_corr): only the within-group
# elements of L are estimated
stopifnot(!any(cross %in% names(coef(m_mixed_corr))))

models <- list(mnl = m_mnl, mixed = m_mixed, mixed_corr = m_mixed_corr)

# Each model must have converged and used every choice situation. mlogit's
# convergence codes: 1 gradient close to zero, 2 successive function values
# within tolerance, 3 last step could not find a higher value, 4 iteration
# limit exceeded. Checked only after all three fits, so that a stop here
# leaves the estimated models in the session.
for (model_name in names(models)) {
  code <- models[[model_name]]$est.stat$code
  cat(model_name, ": convergence code", code, "\n")
  if (!(code %in% c(1, 2))) {
    stop("Model ", model_name, " has not converged (code ", code, ")")
  }
  if (length(fitted(models[[model_name]])) != n_choices) {
    stop("Model ", model_name, " does not use all ", n_choices,
         " choice situations")
  }
}




