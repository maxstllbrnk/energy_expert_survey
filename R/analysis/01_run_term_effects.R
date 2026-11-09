################################################################################
# 01_run_term_effects.R — the effects of the prices, the expert
#                         characteristics and the interactions of the
#                         attributes on the probabilities of the
#                         recommendations
# Inputs : RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/<model>.rds and
#          data_reference.rds (01_run_specifications.R), aggregates only
# Outputs: RC_LOGIT_DIR/rc_logit_<arm>_ref_<outcome>/results/
#            <model>_term_effects.rds, one per model: the change in the
#            probability of each recommendation for each change of a term,
#            with Krinsky-Robb standard errors and percentile intervals; in
#            a model with a neutral base state, also the AMEs, baseline
#            probabilities and mean constants there. 02_output.R makes the
#            figures and the Excel sheet.
# Checks : the reference values of data_reference.rds are those of each
#          model's results; the probabilities and AMEs at the reference
#          vignette reproduce those of 01_run_specifications.R; the mean
#          constants at the neutral base state reproduce its probabilities
################################################################################
# The coefficients of the models are in utility units relative to the heat
# pump, per unit of their term, and their scale differs between models. The
# AMEs of 01_run_specifications.R change one attribute of the reference
# vignette. This script changes the other terms, one at a time, so that every
# effect is a change in a probability, in percentage points, comparable
# across the terms of a model, and across models with the same base state
# (below):
#   price of a fuel          from its 25th to its 75th percentile among the
#                            experts (Max, 2026-10-29); in a model with the
#                            price times the heat demand, also at each other
#                            heat demand
#   price not reported       from reported (the mean) to not reported
#   expected rise            from no to yes (Q17), for one fuel
#   no expectation           from the base state to blank, for one fuel
#   heat pump at home        from no to yes (Q20), (4pe) and (6pe)
#   heating at home          from not this technology to this technology
#                            (Q20), in a model with the match of every
#                            heating at home (own_heating_match, none at the
#                            moment); the heat pump instead of a gas heating
#                            (OWN_HEATING_BEFORE)
#   heating degree days      from the 25th to the 75th percentile among the
#                            experts with a region
#   attribute interactions   the AME of an attribute at the other level of an
#                            attribute it interacts with, e.g. 70 kWh/m² in a
#                            house built in 2000
# Every change starts from a base state (TERM_EFFECTS_BASE): the reference
# vignette and an expert of the profile's Berufsgruppe with the mean prices
# and degree days, and everything else stays there. With "neutral", in a
# model with the expected rises or the heat pump at home ((4pe), (6pe)), this
# expert expects no price to rise and has no heat pump at home (Max,
# 2026-11-01), so that each of these changes switches on one answer
# (neutral_state()). In a model with the match of every heating at home, the
# expert has none of the technologies at home (Q20 has no answer "no
# heating": the expert who answered "Sonstiges" or left Q20 blank, Max,
# 2026-10-31), and the heat pump at home starts from a gas heating at home
# (OWN_HEATING_BEFORE).
# The AMEs and baseline probabilities of 01_run_specifications.R are those of
# an expert with average characteristics (the shares of the expected rises
# and heating at home), so this script also computes them at the neutral
# base state, with the mean constants there, and 02_output.R reports these
# instead, in every table and figure of the model (Max, 2026-10-30). With
# "average", the base state is the reference state of 01 in every model. In a
# model without these terms, the two are the same. A change of a fuel moves
# every technology that uses it, as in the models: electricity the heat pump
# and the hybrid heating, gas the gas and the hybrid heating.
#
# Runtime: like the AMEs of 01_run_specifications.R, one probability per
# change, profile and Krinsky-Robb draw. (4pe) has 22 changes; models can
# run in separate R sessions (MODELS_TO_COMPUTE).

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

# config.R: RC_LOGIT_DIR, packages; BERUFSGRUPPEN
source(file.path("R", "prepare_analysis_data.R"))
# add_regressors(), add_alternative_prices(), long_terms(),
# design_by_profile(), predict_probabilities(), probabilities_krinsky_robb()
source(file.path("R", "analysis", "00_functions_estimation.R"))

# 1. Settings ------------------------------------------------------------------

# The run of 01_run_specifications.R
SELECTED_FW_ARM <- "fw"
REFERENCE_OUTCOME <- "Wärmepumpe"

# The models, the main specification first; a model without saved results is
# skipped with a message, and so is a model without any of the terms above
MODELS_TO_COMPUTE <- c("m4pe", "m6pe", "m4p", "m6p", "m4ip", "m4pa", "m4pd",
                       "m4pl")
MODELS_TO_COMPUTE = c("m4pe", "m6pe")

# Where the changes start, and in (4pe) and (6pe) where 02_output.R reports
# the AMEs and baseline probabilities (header): "neutral", an expert with the
# mean prices and degree days who expects no price to rise and has no heat
# pump at home (Max, 2026-10-30 and 2026-11-01), or "average", the reference
# state of the AMEs of 01_run_specifications.R; the two differ only in (4pe)
# and (6pe)
TERM_EFFECTS_BASE <- "neutral"
if (!(TERM_EFFECTS_BASE %in% c("average", "neutral"))) {
  stop("TERM_EFFECTS_BASE must be \"average\" or \"neutral\"")
}

# In a model with the match of every heating at home (own_heating_match; no
# model at the moment, (4pe) and (6pe) have the heat pump at home only): the
# heating at home that a technology's heating at home replaces, named by the
# technology: the heat pump instead of a gas heating, the most common heating
# at home (Max, 2026-11-01). Before the change the expert has a gas heating
# at home, after it a heat pump. The technologies not named here switch on
# from the base state.
OWN_HEATING_BEFORE <- c("Wärmepumpe" = "Gasheizung")

# 2. Reference values of the data ----------------------------------------------
run_dir <- file.path(RC_LOGIT_DIR, run_folder(SELECTED_FW_ARM, REFERENCE_OUTCOME))
results_dir <- file.path(run_dir, "results")
reference_file <- file.path(results_dir, "data_reference.rds")
if (!file.exists(reference_file)) {
  stop("No data_reference.rds in ", results_dir, ": run ",
       "01_run_specifications.R first (it saves the file in every run, also ",
       "with MODELS_TO_RUN <- character(0))")
}
data_reference <- readRDS(reference_file)
cat("Reference values of", as.character(data_reference$date), "\n")
print(data_reference$price_quartiles)
cat("Degree days: 25th percentile", round(data_reference$hdd_quartiles[["q25"]]),
    "| 75th percentile", round(data_reference$hdd_quartiles[["q75"]]), "\n")

price_reference <- data_reference$price_reference
expert_reference <- data_reference$expert_reference

# 3. The changes ---------------------------------------------------------------

#' The reference state: the reference vignette and an expert with average
#' characteristics, as at the AMEs of 01_run_specifications.R. One row with
#' the attributes (att_*), the degree days (hdd_c), the heat pump at home
#' (heat_pump_at_home) and the expert's prices, expected rises and heating at
#' home, coded as in 01: centred, 0 at the mean; "not reported" and "blank" 0.
#' own_match_<technology> is the value of own_heating_match in the rows of
#' that technology (0: average).
#'
#' @param reference_levels the attributes of the reference vignette
#' @param fuels            the fuels of the prices and expected rises
#' @param technologies     the technologies of the heating at home
#' @return tibble, one row
reference_state <- function(reference_levels, fuels, technologies) {
  state <- as_tibble(reference_levels)
  state$hdd_c <- 0
  state$heat_pump_at_home <- 0
  for (fuel in fuels) {
    state[[paste0("price_", fuel, "_c")]] <- 0
    state[[paste0("price_", fuel, "_lc")]] <- 0
    state[[paste0("price_", fuel, "_na")]] <- 0
    state[[paste0("pexp_", fuel, "_c")]] <- 0
    state[[paste0("pexp_", fuel, "_na")]] <- 0
  }
  for (technology in technologies) {
    state[[paste0("own_match_", technology)]] <- 0
  }
  state
}

#' The neutral base state (TERM_EFFECTS_BASE "neutral"): the reference state,
#' but in a model with the expected rises or the heating at home, an expert
#' who expects no price to rise and has no heat pump at home (with the match
#' of every heating at home: whose home none of the technologies heats), so
#' that a change of these terms switches on one answer (a technology of
#' OWN_HEATING_BEFORE starts from the heating it replaces). Coded as in 01: 0
#' minus the share, as an answer "no". The prices and degree days stay at
#' their means. In a model without these terms, the reference state itself.
#'
#' @param reference        reference_state()
#' @param prices           the model's terms with one coefficient for all
#'                         technologies (specification$prices)
#' @param regressors       the model's regressors (specification$regressors)
#' @param expert_reference of data_reference.rds: the shares of the answers
#' @param fuels            the fuels of the expected rises
#' @param technologies     the technologies of the heating at home
#' @return tibble, one row
neutral_state <- function(reference, prices, regressors, expert_reference,
                          fuels, technologies) {
  state <- reference
  if ("expects_rise" %in% prices) {
    for (fuel in fuels) {
      is_fuel <- expert_reference$pexp$fuel == fuel
      state[[paste0("pexp_", fuel, "_c")]] <-
        0 - expert_reference$pexp$share_rise[is_fuel]
    }
  }
  if ("own_heating_match" %in% prices) {
    for (technology in technologies) {
      state[[paste0("own_match_", technology)]] <-
        0 - expert_reference$own_heating_share[[technology]]
    }
  }
  if ("heat_pump_at_home" %in% regressors) {
    state$heat_pump_at_home <-
      0 - expert_reference$own_heating_share[["Wärmepumpe"]]
  }
  state
}

#' One change: what differs from the base state before and after it
#' (named lists of columns of the state; an empty list is the base state
#' itself), and what it is, for the labels of 02_output.R. condition: of an
#' interaction, the attribute dummy where the attribute changes; of the
#' heating at home, the heating it replaces (OWN_HEATING_BEFORE), NA if none
new_change <- function(id, type, before, after, fuel = NA_character_,
                       technology = NA_character_, attribute = NA_character_,
                       condition = NA_character_, demand = NA_character_,
                       value_before = NA_real_, value_after = NA_real_) {
  list(id = id, type = type, before = before, after = after, fuel = fuel,
       technology = technology, attribute = attribute, condition = condition,
       demand = demand, value_before = value_before, value_after = value_after)
}

#' The changes that apply to a model: those of the terms it has
#'
#' @param specification     the model's specification (its saved results)
#' @param attribute_dummies ATTRIBUTE_DUMMIES of 01
#' @param outcomes          all outcomes
#' @param data_reference    data_reference.rds of 01: the means, shares and
#'                          quartiles, and which technology uses which fuel
#' @param own_heating_before OWN_HEATING_BEFORE: the heating at home that a
#'                          technology's heating at home replaces
#' @return list of changes (new_change())
term_changes <- function(specification, attribute_dummies, outcomes,
                         data_reference, own_heating_before) {
  prices <- specification$prices
  regressors <- specification$regressors
  price_reference <- data_reference$price_reference
  price_quartiles <- data_reference$price_quartiles
  expert_reference <- data_reference$expert_reference
  # The fuels of a technology of this arm: in the arm without district
  # heating, its fuel moves no utility
  price_fuels <- intersect(price_reference$fuel,
                           data_reference$price_variables$name)
  expectation_fuels <- intersect(expert_reference$pexp$fuel,
                                 data_reference$expectation_variables$name)
  changes <- list()
  # The dummies of the heat demand other than the reference demand, and the
  # price times each, named as in long_terms()
  demand_dummies <- attribute_dummies %>%
    filter(attribute == "att_heat_demand")
  demand_terms <- paste0("price_x_", demand_dummies$dummy)

  # (a) The price of each fuel, from its 25th to its 75th percentile, in
  #     levels and logs (the model uses one of them), relative to the mean
  if (any(c("price", "log_price") %in% prices)) {
    for (fuel in price_fuels) {
      mean_price <- price_reference$mean[price_reference$fuel == fuel]
      q25 <- price_quartiles$q25[price_quartiles$fuel == fuel]
      q75 <- price_quartiles$q75[price_quartiles$fuel == fuel]
      before <- list()
      before[[paste0("price_", fuel, "_c")]] <- q25 - mean_price
      before[[paste0("price_", fuel, "_lc")]] <- log(q25) - log(mean_price)
      after <- list()
      after[[paste0("price_", fuel, "_c")]] <- q75 - mean_price
      after[[paste0("price_", fuel, "_lc")]] <- log(q75) - log(mean_price)
      changes[[length(changes) + 1]] <- new_change(
        paste0("price_", fuel), "price", before, after, fuel = fuel,
        value_before = q25, value_after = q75)
      # at each other heat demand, in a model with the price times the heat
      # demand: the same change in a house with that demand
      if (all(demand_terms %in% prices)) {
        for (k in seq_len(nrow(demand_dummies))) {
          before_k <- before
          after_k <- after
          before_k$att_heat_demand <- demand_dummies$level[k]
          after_k$att_heat_demand <- demand_dummies$level[k]
          changes[[length(changes) + 1]] <- new_change(
            paste0("price_", fuel, "_", demand_dummies$dummy[k]), "price",
            before_k, after_k, fuel = fuel, demand = demand_dummies$dummy[k],
            value_before = q25, value_after = q75)
        }
      }
    }
  }

  # (b) Price not reported: from reported (at the mean) to not reported, for
  #     the fuels whose dummy the model has
  for (fuel in price_fuels) {
    if (paste0(fuel, "_price_na") %in% prices) {
      after <- list()
      after[[paste0("price_", fuel, "_na")]] <- 1
      changes[[length(changes) + 1]] <- new_change(
        paste0("price_na_", fuel), "price_na", list(), after, fuel = fuel)
    }
  }

  # (c) Expected rise: from no (0 minus the share) to yes (1 minus the share),
  #     for one fuel; the other fuels stay as in the base state (their share,
  #     or no in the neutral base state)
  if ("expects_rise" %in% prices) {
    for (fuel in expectation_fuels) {
      is_fuel <- expert_reference$pexp$fuel == fuel
      share <- expert_reference$pexp$share_rise[is_fuel]
      before <- list()
      before[[paste0("pexp_", fuel, "_c")]] <- 0 - share
      after <- list()
      after[[paste0("pexp_", fuel, "_c")]] <- 1 - share
      changes[[length(changes) + 1]] <- new_change(
        paste0("expects_rise_", fuel), "expects_rise", before, after,
        fuel = fuel)
    }
  }

  # (d) No expectation: from the base state to blank, for the fuels whose
  #     dummy the model has. A blank is coded as in 01: the dummy 1 and the
  #     expected rise 0.
  for (fuel in expectation_fuels) {
    if (paste0(fuel, "_rise_na") %in% prices) {
      after <- list()
      after[[paste0("pexp_", fuel, "_c")]] <- 0
      after[[paste0("pexp_", fuel, "_na")]] <- 1
      changes[[length(changes) + 1]] <- new_change(
        paste0("rise_na_", fuel), "rise_na", list(), after, fuel = fuel)
    }
  }

  # (e) Heating at home: from not this technology (0 minus the share) to this
  #     technology (1 minus the share); the other technologies stay as in the
  #     base state (their share, or not at home in the neutral base state).
  #     A technology of own_heating_before replaces another heating: before
  #     the change that one is at home (1 minus its share), after it not (0
  #     minus its share), e.g. a heat pump instead of a gas heating
  if ("own_heating_match" %in% prices) {
    own_technologies <- intersect(data_reference$own_heating_answers, outcomes)
    # a name that is not a heating at home of the model would be skipped
    unknown <- setdiff(names(own_heating_before), own_technologies)
    if (length(unknown) > 0) {
      stop("OWN_HEATING_BEFORE: ", paste(unknown, collapse = ", "),
           " is not a heating at home of the model")
    }
    for (technology in own_technologies) {
      share <- expert_reference$own_heating_share[[technology]]
      before <- list()
      before[[paste0("own_match_", technology)]] <- 0 - share
      after <- list()
      after[[paste0("own_match_", technology)]] <- 1 - share
      id <- paste0("own_heating_", technology)
      replaced <- NA_character_
      if (technology %in% names(own_heating_before)) {
        replaced <- own_heating_before[[technology]]
        if (!(replaced %in% own_technologies) || replaced == technology) {
          stop("OWN_HEATING_BEFORE: ", replaced, " is not another heating at ",
               "home of the model")
        }
        replaced_share <- expert_reference$own_heating_share[[replaced]]
        before[[paste0("own_match_", replaced)]] <- 1 - replaced_share
        after[[paste0("own_match_", replaced)]] <- 0 - replaced_share
        id <- paste0(id, "_instead_of_", replaced)
      }
      changes[[length(changes) + 1]] <- new_change(
        id, "own_heating", before, after, technology = technology,
        condition = replaced)
    }
  }

  # (e) A heat pump at home: from no (0 minus the share) to yes (1 minus the
  #     share), in a model with heat_pump_at_home among the regressors ((4pe),
  #     (6pe)); with one coefficient per technology, it moves every utility.
  #     The other heatings at home are not in the model.
  if ("heat_pump_at_home" %in% regressors) {
    share <- expert_reference$own_heating_share[["Wärmepumpe"]]
    changes[[length(changes) + 1]] <- new_change(
      "heat_pump_at_home", "own_heating",
      list(heat_pump_at_home = 0 - share), list(heat_pump_at_home = 1 - share),
      technology = "Wärmepumpe")
  }

  # (f) Heating degree days: from the 25th to the 75th percentile, centred
  #     and in units as in 01
  if ("hdd_c" %in% regressors) {
    q25 <- data_reference$hdd_quartiles[["q25"]]
    q75 <- data_reference$hdd_quartiles[["q75"]]
    changes[[length(changes) + 1]] <- new_change(
      "hdd", "hdd",
      list(hdd_c = (q25 - expert_reference$hdd_mean) / expert_reference$hdd_unit),
      list(hdd_c = (q75 - expert_reference$hdd_mean) / expert_reference$hdd_unit),
      value_before = q25, value_after = q75)
  }

  # (g) Interactions of the attributes: for each pair of dummies whose product
  #     <dummy>_x_<dummy> is a regressor (the exact names of add_regressors()),
  #     the AME of each at the other's level
  for (i in seq_len(nrow(attribute_dummies))) {
    for (j in seq_len(nrow(attribute_dummies))) {
      interaction <- paste0(attribute_dummies$dummy[i], "_x_",
                            attribute_dummies$dummy[j])
      if (interaction %in% regressors) {
        pairs <- list(c(i, j), c(j, i))
        for (pair in pairs) {
          changed <- attribute_dummies[pair[1], ]
          condition <- attribute_dummies[pair[2], ]
          before <- list()
          before[[condition$attribute]] <- condition$level
          after <- before
          after[[changed$attribute]] <- changed$level
          changes[[length(changes) + 1]] <- new_change(
            paste0(changed$dummy, "_at_", condition$dummy), "interaction",
            before, after, attribute = changed$dummy,
            condition = condition$dummy)
        }
      }
    }
  }
  changes
}

#' Rows of states: the base state with the values of a change set
apply_change <- function(state, values) {
  for (column in names(values)) {
    if (!(column %in% names(state))) stop("Not a column of the state: ", column)
    state[[column]] <- values[[column]]
  }
  state
}

# 4. Compute, one model at a time ----------------------------------------------
for (model_name in MODELS_TO_COMPUTE) {
  results_file <- file.path(results_dir, paste0(model_name, ".rds"))
  if (!file.exists(results_file)) {
    message("No results for ", model_name, ": run 01_run_specifications.R for it")
    next
  }
  results <- readRDS(results_file)
  specification <- results$specification
  outcomes <- results$outcomes
  attribute_dummies <- results$attribute_dummies
  start_time <- Sys.time()   # Sys.time(): the current date and time

  # (a) The reference values must be those of this model's results: the
  #     same data, centred the same way. The degree days are in
  #     expert_reference, the prices in price_reference.
  if (results$fit$n_experts != data_reference$n_experts ||
      results$fit$n_choices != data_reference$n_choices) {
    stop(model_name, " was estimated on another sample than data_reference.rds")
  }
  same_prices <- identical(results$price_reference, price_reference) &&
    identical(results$price_settings$cop, data_reference$price_settings$cop) &&
    identical(results$price_settings$hybrid_gas_share,
              data_reference$price_settings$hybrid_gas_share)
  if (length(specification$prices) > 0 && !same_prices) {
    stop("The prices of ", model_name, " differ from data_reference.rds: rerun ",
         "01_run_specifications.R for ", model_name, " or for the data")
  }
  uses_experts <-
    any(c("hdd_c", "heat_pump_at_home") %in% specification$regressors) ||
    any(c("own_heating_match", "expects_rise") %in% specification$prices)
  if (uses_experts && !identical(results$expert_reference, expert_reference)) {
    stop("The expert characteristics of ", model_name, " differ from ",
         "data_reference.rds: rerun 01_run_specifications.R")
  }

  changes <- term_changes(specification, attribute_dummies, outcomes,
                          data_reference, OWN_HEATING_BEFORE)
  if (length(changes) == 0) {
    message(model_name, ": none of the terms of this script")
    next
  }
  cat("\n====", model_name, ":", length(changes), "changes ====\n")

  # (b) The states: the reference state of the AMEs of 01 first (for the
  #     checks in (f) and (g)), then the base state of the changes if it
  #     differs from it (TERM_EFFECTS_BASE "neutral") with the vignettes of
  #     the AMEs there (one attribute changed, "base|<dummy>"), then before
  #     and after of each change, set from the base state; a change that
  #     starts from the base state uses its row
  technologies_own <- intersect(data_reference$own_heating_answers, outcomes)
  expectation_fuels <- intersect(expert_reference$pexp$fuel,
                                 data_reference$expectation_variables$name)
  reference <- reference_state(results$reference_levels, price_reference$fuel,
                               technologies_own)
  base <- reference
  if (TERM_EFFECTS_BASE == "neutral") {
    base <- neutral_state(reference, specification$prices,
                          specification$regressors, expert_reference,
                          expectation_fuels, technologies_own)
  }
  states <- reference %>%
    mutate(vignette = "reference")
  base_row <- "reference"
  base_ame_rows <- character(0)
  if (!identical(base, reference)) {
    base_row <- "base"
    states <- bind_rows(states, base %>%
                          mutate(vignette = base_row))
    for (k in seq_len(nrow(attribute_dummies))) {
      variation <- base
      variation[[attribute_dummies$attribute[k]]] <- attribute_dummies$level[k]
      base_ame_rows <- c(base_ame_rows,
                         paste0("base|", attribute_dummies$dummy[k]))
      states <- bind_rows(states, variation %>%
                            mutate(vignette = paste0("base|",
                                                     attribute_dummies$dummy[k])))
    }
  }
  before_rows <- character(0)
  after_rows <- character(0)
  for (change in changes) {
    before_row <- base_row
    if (length(change$before) > 0) {
      before_row <- paste0(change$id, "|before")
      states <- bind_rows(states, apply_change(base, change$before) %>%
                            mutate(vignette = before_row))
    }
    after_row <- paste0(change$id, "|after")
    states <- bind_rows(states, apply_change(base, change$after) %>%
                          mutate(vignette = after_row))
    before_rows <- c(before_rows, before_row)
    after_rows <- c(after_rows, after_row)
  }
  # the expert of the neutral base state, by the terms of the model
  neutral_items <- character(0)
  if ("expects_rise" %in% specification$prices) {
    neutral_items <- c(neutral_items, "no expected price rise")
  }
  if ("own_heating_match" %in% specification$prices) {
    neutral_items <- c(neutral_items, "none of the technologies at home")
  }
  if ("heat_pump_at_home" %in% specification$regressors) {
    neutral_items <- c(neutral_items, "no heat pump at home")
  }
  base_text <- "the reference state of 01 (average characteristics)"
  if (base_row == "base") base_text <- paste(neutral_items, collapse = ", ")
  cat("Base state of the changes:", base_text, "\n")

  # (c) The design matrices, one per profile, coded as in 01
  columns <- c(specification$constants, specification$regressors)
  X_by_profile <- design_by_profile(states, BERUFSGRUPPEN, columns,
                                    attribute_dummies)
  if (!identical(names(X_by_profile), names(results$probability_draws))) {
    stop(model_name, ": the profiles differ from those of its results")
  }

  # (d) The terms with one coefficient for all technologies, one matrix per
  #     term (states x outcomes), built as in 01 (add_alternative_prices(),
  #     long_terms()). The terms do not depend on the Berufsgruppe, which
  #     add_regressors() needs: the first stands in. The heating at home is
  #     set from own_match_<technology>, as the experts of the states (with
  #     average characteristics, or none of the technologies at home) have no
  #     single answer of Q20. The heat pump at home is a regressor, in the
  #     design matrices of (c).
  states_wide <- states %>%
    mutate(berufsgruppe = BERUFSGRUPPEN[1],
           own_heating = "Keine Angabe") %>%
    add_regressors(attribute_dummies) %>%
    add_alternative_prices(results$price_settings$cop,
                           results$price_settings$hybrid_gas_share)
  states_long <- states_wide %>%
    cross_join(tibble(alternative = outcomes)) %>%
    long_terms(data_reference$price_variables,
               data_reference$expectation_variables,
               expert_reference$own_heating_share,
               data_reference$own_heating_answers, outcomes)
  for (technology in technologies_own) {
    is_technology <- states_long$alternative == technology
    states_long$own_heating_match[is_technology] <-
      states_long[[paste0("own_match_", technology)]][is_technology]
  }
  generic_values <- list()
  for (term in specification$prices) {
    values <- matrix(NA_real_, nrow = nrow(states), ncol = length(outcomes),
                     dimnames = list(states$vignette, outcomes))
    for (outcome in outcomes) {
      outcome_rows <- states_long %>%
        filter(alternative == outcome)
      # one row per state, in the order of the states
      stopifnot(identical(outcome_rows$vignette, states$vignette))
      values[, outcome] <- outcome_rows[[term]]
    }
    # Check: at the reference state every term is 0, as in the AMEs of 01
    # (an expert at the mean), so that the effects start from 01's reference
    # probabilities
    if (any(values["reference", ] != 0)) {
      stop(model_name, ": the term ", term, " is not 0 at the reference state")
    }
    generic_values[[term]] <- values
  }

  # The mean constants at the neutral base state: the constant of each
  # technology plus what the terms with one coefficient for all technologies
  # add to its utility there, relative to the reference outcome's (at the
  # reference state they add nothing), and what the regressors that differ
  # between the two states add (the heat pump at home, with its coefficient
  # of that technology, "heat_pump_at_home:<technology>"). One row per
  # technology and coefficient that adds something, with its weight;
  # 02_output.R adds these to the constants of the tables. Checked in (g).
  base_constant_shift <- NULL
  if (base_row == "base") {
    base_constant_shift <- tibble()
    for (term in names(generic_values)) {
      for (technology in outcomes[-1]) {
        weight <- generic_values[[term]][base_row, technology] -
          generic_values[[term]][base_row, outcomes[1]]
        if (abs(weight) > 1e-12) {
          base_constant_shift <- bind_rows(base_constant_shift, tibble(
            technology = technology,
            term = term,
            weight = weight
          ))
        }
      }
    }
    # A regressor's difference between the two states must be the same in
    # every profile, as it shifts the constants of all of them
    for (regressor in specification$regressors) {
      differences <- numeric(0)
      for (profile_name in names(X_by_profile)) {
        X <- X_by_profile[[profile_name]]
        differences[profile_name] <- X[base_row, regressor] -
          X["reference", regressor]
      }
      if (any(abs(differences - differences[1]) > 1e-12)) {
        stop(model_name, ": ", regressor, " differs between the base and the ",
             "reference state by profile")
      }
      if (abs(differences[1]) > 1e-12) {
        for (technology in outcomes[-1]) {
          base_constant_shift <- bind_rows(base_constant_shift, tibble(
            technology = technology,
            term = paste0(regressor, ":", technology),
            weight = differences[[1]]
          ))
        }
      }
    }
  }

  # (e) The draws as in 01: the same standard normal draws of the random
  #     constants (seed and number), the saved Krinsky-Robb draws of theta
  random <- results$random_coefficients
  set.seed(20261001)
  standard_draws <- matrix(rnorm(results$draws$n_draws_ame * nrow(random)),
                           nrow = results$draws$n_draws_ame, ncol = nrow(random))

  # (f) Check: at theta_hat, the probabilities at the reference vignette and
  #     the AMEs of the attributes are those of 01 (the same draws), so the
  #     design matrices, the profiles and the draws are coded as in 01
  ame_vignettes <- reference %>%
    mutate(vignette = "reference")
  for (k in seq_len(nrow(attribute_dummies))) {
    variation <- reference
    variation[[attribute_dummies$attribute[k]]] <- attribute_dummies$level[k]
    ame_vignettes <- bind_rows(ame_vignettes, variation %>%
                                 mutate(vignette = attribute_dummies$dummy[k]))
  }
  X_check <- design_by_profile(ame_vignettes, BERUFSGRUPPEN, columns,
                               attribute_dummies)
  largest_difference <- 0
  for (profile_name in names(X_check)) {
    probabilities <- predict_probabilities(results$coefficients,
                                           X_check[[profile_name]],
                                           standard_draws, random, outcomes,
                                           results$fixed_parameters)
    saved <- results$ame_table %>%
      filter(profile == profile_name)
    for (k in seq_len(nrow(saved))) {
      p_reference <- probabilities["reference", saved$outcome[k]]
      ame <- probabilities[saved$vignette[k], saved$outcome[k]] - p_reference
      largest_difference <- max(largest_difference,
                                abs(p_reference - saved$p_reference[k]),
                                abs(ame - saved$ame[k]))
    }
  }
  cat("Largest difference to the probabilities and AMEs of 01:",
      signif(largest_difference, 3), "\n")
  if (largest_difference > 1e-8) {
    stop(model_name, ": the probabilities differ from those of 01")
  }

  # (g) The probabilities of all states, at theta_hat and at every
  #     Krinsky-Robb draw, and the effects: after minus before. At a neutral
  #     base state also the AMEs there, as 01 saves them (ame_table,
  #     probability_draws), so that 02_output.R can use them in their place.
  table <- tibble()
  base_probabilities <- tibble()
  base_ame_table <- NULL
  base_probability_draws <- NULL
  z_critical <- qnorm(0.975)
  # The coefficients with the mean constants at the base state, for the
  # check of base_constant_shift: every profile has a 1 in exactly one
  # column of the constants ("(Intercept)" or its group's constant)
  theta_base <- results$coefficients
  if (base_row == "base") {
    base_ame_table <- tibble()
    base_probability_draws <- list()
    for (k in seq_len(nrow(base_constant_shift))) {
      for (constant in specification$constants) {
        name <- paste0(constant, ":", base_constant_shift$technology[k])
        theta_base[[name]] <- theta_base[[name]] + base_constant_shift$weight[k] *
          results$coefficients[[base_constant_shift$term[k]]]
      }
    }
  }
  for (profile_name in names(X_by_profile)) {
    probabilities <- probabilities_krinsky_robb(
      results$coefficients, results$theta_draws, X_by_profile[[profile_name]],
      standard_draws, random, outcomes, results$fixed_parameters,
      generic_values, label = paste(model_name, profile_name)
    )
    # Check: with the terms, the probabilities at the reference state are
    # still those of 01 at the reference vignette
    saved <- results$ame_table %>%
      filter(profile == profile_name) %>%
      distinct(outcome, p_reference)
    reference_difference <- probabilities$estimate["reference", saved$outcome] -
      saved$p_reference
    if (max(abs(reference_difference)) > 1e-8) {
      stop(model_name, ", ", profile_name, ": the probabilities at the ",
           "reference state differ from those of 01")
    }
    # The probabilities at the base state, from which the changes start: the
    # baseline that 02_output.R shows with them
    for (outcome in outcomes) {
      base_draws <- probabilities$draws[base_row, outcome, ]
      base_probabilities <- bind_rows(base_probabilities, tibble(
        profile = profile_name,
        outcome = outcome,
        estimate = probabilities$estimate[base_row, outcome],
        se = sd(base_draws),
        ci_low_kr = as.numeric(quantile(base_draws, 0.025)),
        ci_high_kr = as.numeric(quantile(base_draws, 0.975))
      ))
    }
    # At a neutral base state: the AMEs there, each attribute's vignette
    # minus the base state, with the probabilities at every draw named as in
    # 01 ("reference" for the base state); and the check that the mean
    # constants there give its probabilities at the reference vignette
    if (base_row == "base") {
      X <- X_by_profile[[profile_name]]
      shifted <- predict_probabilities(theta_base, X["reference", , drop = FALSE],
                                       standard_draws, random, outcomes,
                                       results$fixed_parameters)
      shift_difference <- max(abs(shifted["reference", ] -
                                    probabilities$estimate[base_row, ]))
      cat(profile_name, ": largest difference of the mean constants at the",
          "base state to its probabilities:", signif(shift_difference, 3), "\n")
      if (shift_difference > 1e-8) {
        stop(model_name, ", ", profile_name, ": the mean constants at the base ",
             "state do not reproduce its probabilities")
      }
      draws <- probabilities$draws[c(base_row, base_ame_rows), , , drop = FALSE]
      dimnames(draws)[[1]] <- c("reference", attribute_dummies$dummy)
      base_probability_draws[[profile_name]] <- draws
      for (k in seq_along(base_ame_rows)) {
        for (outcome in outcomes) {
          p_base <- probabilities$estimate[base_row, outcome]
          ame_draws <- probabilities$draws[base_ame_rows[k], outcome, ] -
            probabilities$draws[base_row, outcome, ]
          base_ame_table <- bind_rows(base_ame_table, tibble(
            profile = profile_name,
            vignette = attribute_dummies$dummy[k],
            outcome = outcome,
            p_reference = p_base,
            se_p_reference = sd(probabilities$draws[base_row, outcome, ]),
            ame = probabilities$estimate[base_ame_rows[k], outcome] - p_base,
            se = sd(ame_draws),
            ci_low_kr = as.numeric(quantile(ame_draws, 0.025)),
            ci_high_kr = as.numeric(quantile(ame_draws, 0.975))
          ))
        }
      }
    }
    for (k in seq_along(changes)) {
      # not named `change`: inside tibble() below, the column change would
      # hide it
      definition <- changes[[k]]
      # The outcomes whose own utility the change moves: a term of theirs
      # differs between before and after; with a change of the attributes,
      # degree days or heat pump at home, all
      is_affected <- setNames(rep(FALSE, length(outcomes)), outcomes)
      for (term in names(generic_values)) {
        difference <- generic_values[[term]][after_rows[k], ] -
          generic_values[[term]][before_rows[k], ]
        is_affected <- is_affected | abs(difference) > 0
      }
      X <- X_by_profile[[profile_name]]
      if (any(X[after_rows[k], ] != X[before_rows[k], ])) {
        is_affected[] <- TRUE
      }
      for (outcome in outcomes) {
        effect_draws <- probabilities$draws[after_rows[k], outcome, ] -
          probabilities$draws[before_rows[k], outcome, ]
        table <- bind_rows(table, tibble(
          profile = profile_name,
          change = definition$id,
          type = definition$type,
          fuel = definition$fuel,
          technology = definition$technology,
          attribute = definition$attribute,
          condition = definition$condition,
          demand = definition$demand,
          value_before = definition$value_before,
          value_after = definition$value_after,
          outcome = outcome,
          is_affected = is_affected[[outcome]],
          p_before = probabilities$estimate[before_rows[k], outcome],
          p_after = probabilities$estimate[after_rows[k], outcome],
          effect = p_after - p_before,
          se = sd(effect_draws),
          ci_low_kr = as.numeric(quantile(effect_draws, 0.025)),
          ci_high_kr = as.numeric(quantile(effect_draws, 0.975))
        ))
      }
    }
  }
  table <- table %>%
    mutate(z = effect / se,
           p = 2 * pnorm(-abs(z)),
           ci_low = effect - z_critical * se,
           ci_high = effect + z_critical * se)
  # the columns of 01's AME table, in its order
  if (base_row == "base") {
    base_ame_table <- base_ame_table %>%
      mutate(z = ame / se,
             p = 2 * pnorm(-abs(z)),
             ci_low = ame - z_critical * se,
             ci_high = ame + z_critical * se) %>%
      select(profile, vignette, outcome, p_reference, se_p_reference, ame, se,
             z, p, ci_low, ci_high, ci_low_kr, ci_high_kr)
    if (!identical(names(base_ame_table), names(results$ame_table)) ||
        nrow(base_ame_table) != nrow(results$ame_table)) {
      stop(model_name, ": the AMEs at the base state are not laid out as 01's")
    }
  }

  # (h) Save: aggregates only
  # The coefficients they rest on: 02_output.R checks that they are still the
  # model's, so that effects of an earlier estimation cannot be reported.
  # base_state: "neutral" (no expected price rise, no heat pump at home, or
  # none of the technologies at home with the match of every heating at
  # home) or "reference" (average characteristics), with the
  # probabilities there; at "neutral" also the AMEs and probabilities at
  # every draw there, laid out as ame_table and probability_draws of 01, and
  # base_constant_shift for the mean constants there (NULL at "reference")
  term_effects <- list(
    model = model_name,
    coefficients = results$coefficients,
    table = table,
    base_state = if_else(base_row == "base", "neutral", "reference"),
    base_probabilities = base_probabilities,
    base_ame_table = base_ame_table,
    base_probability_draws = base_probability_draws,
    base_constant_shift = base_constant_shift,
    quartiles = list(prices = data_reference$price_quartiles,
                     hdd = data_reference$hdd_quartiles),
    draws = results$draws,
    date = Sys.Date()
  )
  saveRDS(term_effects,
          file.path(results_dir, paste0(model_name, "_term_effects.rds")))
  minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(model_name, ": done in", round(minutes, 1), "minutes\n")

  # The effect of every change on the reference outcome, the heat pump, for
  # the first profile, in percentage points
  print(table %>%
          filter(profile == names(X_by_profile)[1],
                 outcome == REFERENCE_OUTCOME) %>%
          transmute(change, effect_pp = round(100 * effect, 2),
                    se_pp = round(100 * se, 2)),
        n = 60)
}
