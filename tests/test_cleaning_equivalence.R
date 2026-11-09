################################################################################
# test_cleaning_equivalence.R — does the new cleaning step reproduce the old
#                               functions?
# Inputs : survey_main.rds, vignettes_long.rds of a synthetic run (tests/simulate_data.R,
#          then R/run_cleaning.R); degree days (HDD_FILE)
# Outputs: a table in the log, one row per comparison; files only in a temporary
#          folder of the synthetic run. Exit status 1 if a column differs.
# Checks : expert_group, the prices, the last question, the preference scores, the
#          expected rises, the heating at home, the degree days, the shares, the
#          outlier rule, the arms, the vignettes and the persons, each against the
#          old code; reference_values against section 2.1 of the old models
################################################################################
# Run from the project root, in a session with CLAUDECODE=1:
#   Rscript tests/simulate_data.R
#   Rscript R/run_cleaning.R
#   Rscript tests/test_cleaning_equivalence.R
#
# Section 1 holds the old code, from the git tag pre-restructure: the same
# statements, with old_ or OLD_ in front of each name, without most comments and
# without the log lines. What else it leaves out, its parts say. So it is not in
# Max's vocabulary, and it is not meant to be edited.
#
# The comparison runs twice: on the data as simulated, and on a stressed copy with
# what the simulation does not produce: repeat completions, ties between attempts,
# an expert in both arms who stays, euro answers per litre, prices at the edges of
# the bands, odd answers to Q2 and an office of engineers only, skipped and
# abandoned branches of the staircases.
#
# Differences that are intended are printed, and do not fail:
#   1. The outlier rule runs over everyone who answered, instead of the completers
#      of the three groups (Max, 2026-11-09).
#   2. One row per person (Max, 2026-11-09): the old models counted a repeat
#      completion twice, so their reference values can differ.

source(file.path("R", "setup.R"))
source(file.path("R", "cleaning", "00_preferences.R"), encoding = "UTF-8")
if (!USE_SYNTHETIC_DATA) {
  stop("tests/test_cleaning_equivalence.R runs on synthetic data only")
}

# 1. The old code --------------------------------------------------------------

# 1.1 R/prepare_analysis_data.R ------------------------------------------------
OLD_BERUFSGRUPPEN <- c("SHK-Handwerk", "Schornsteinfeger", "Energieberater")

old_add_berufsgruppe <- function(main) {
  main %>%
    mutate(
      berufsgruppe = case_when(
        is.na(firm_cat_hvac)          ~ NA_character_,   # Q2 not answered
        firm_cat_hvac         == "Ja" ~ "SHK-Handwerk",
        firm_cat_chimney      == "Ja" ~ "Schornsteinfeger",
        firm_cat_energyadvice == "Ja" ~ "Energieberater",
        firm_cat_engineering  == "Ja" ~ "Energieberater",
        !is.na(firm_cat_other)        ~ "Energieberater",
        TRUE                          ~ "Keine Zuordnung"
      ),
      berufsgruppe = factor(berufsgruppe, levels = c(OLD_BERUFSGRUPPEN, "Keine Zuordnung"))
    )
}

OLD_QUESTIONNAIRE <- tribble(
  ~item,                    ~label,                                     ~section,
  "time_firm_zip",          "Q0 Postleitzahl",                          "Unternehmen",
  "time_resp_is_owner",     "Q0a Eigentümer/Geschäftsführer",           "Unternehmen",
  "time_resp_role",         "Q0b Rolle im Unternehmen",                 "Unternehmen",
  "time_firm_cat",          "Q2 Unternehmenskategorie",                 "Unternehmen",
  "time_firm_employees",    "Q3 Beschäftigte",                          "Unternehmen",
  "time_firm_revenue",      "Q4 Umsatz",                                "Unternehmen",
  "time_firm_radius",       "Q6 Einzugsgebiet",                         "Unternehmen",
  "time_firm_dh_in_area",   "Q6a Fernwärme im Einzugsgebiet",           "Unternehmen",
  "vignette_1",             "Vignette 1",                               "Vignetten",
  "vignette_2",             "Vignette 2",                               "Vignetten",
  "vignette_3",             "Vignette 3",                               "Vignetten",
  "vignette_4",             "Vignette 4",                               "Vignetten",
  "vignette_5",             "Vignette 5",                               "Vignetten",
  "vignette_6",             "Vignette 6",                               "Vignetten",
  "vignette_cost",          "Kostenfrage zur letzten Vignette",         "Vignetten",
  "time_price",             "Q15 Energiepreise 2026",                   "Energiepreise und CO₂-Preis",
  "time_pexp",              "Q17 Erwartete Preisentwicklung",           "Energiepreise und CO₂-Preis",
  "time_co2_heard",         "Q18 CO₂-Bepreisung bekannt",               "Energiepreise und CO₂-Preis",
  "time_co2_est",           "Q18b CO₂-Preis geschätzt",                 "Energiepreise und CO₂-Preis",
  "time_mix25",             "Q11b Installierte Heizsysteme 2025",       "Heiztechnologien und Hersteller",
  "time_brand_top1",        "Q11c Meistverkaufter Hersteller",          "Heiztechnologien und Hersteller",
  "time_tp_1",              "Zeitpräferenz, Entscheidung 1",            "Zeitpräferenzen",
  "time_tp_2",              "Zeitpräferenz, Entscheidung 2",            "Zeitpräferenzen",
  "time_tp_3",              "Zeitpräferenz, Entscheidung 3",            "Zeitpräferenzen",
  "time_tp_4",              "Zeitpräferenz, Entscheidung 4",            "Zeitpräferenzen",
  "time_tp_5",              "Zeitpräferenz, Entscheidung 5",            "Zeitpräferenzen",
  "time_risk_1",            "Risikopräferenz, Entscheidung 1",          "Risikopräferenzen",
  "time_risk_2",            "Risikopräferenz, Entscheidung 2",          "Risikopräferenzen",
  "time_risk_3",            "Risikopräferenz, Entscheidung 3",          "Risikopräferenzen",
  "time_risk_4",            "Risikopräferenz, Entscheidung 4",          "Risikopräferenzen",
  "time_risk_5",            "Risikopräferenz, Entscheidung 5",          "Risikopräferenzen",
  "time_svc",               "Q8 Angebotene Leistungen",                 "Dienstleistungen, Markt und Kunden",
  "time_adv_households",    "Q10a Beratene Haushalte",                  "Dienstleistungen, Markt und Kunden",
  "time_adv_share",         "Q10b Anteile der Empfehlungen",            "Dienstleistungen, Markt und Kunden",
  "time_barr_ren",          "Q19a Hemmnisse erneuerbare Heizungen",     "Dienstleistungen, Markt und Kunden",
  "time_barr_dh",           "Q19b Hemmnisse Fernwärme",                 "Dienstleistungen, Markt und Kunden",
  "time_barr_conv",         "Q19c Hemmnisse konventionelle Heizungen",  "Dienstleistungen, Markt und Kunden",
  "time_cust_rank",         "Q7 Wichtigste Kundengruppen",              "Dienstleistungen, Markt und Kunden",
  "time_crit_rank",         "Q28 Wichtigste Kriterien der Kunden",      "Dienstleistungen, Markt und Kunden",
  "time_dem_birthyear",     "Q21 Geburtsjahr",                          "Persönliche Angaben",
  "time_dem_education",     "Q22 Bildungsabschluss",                    "Persönliche Angaben",
  "time_dem_training_year", "Q23 Abschlussjahr der Ausbildung",         "Persönliche Angaben",
  "time_cpd",               "Q24 Fortbildungen",                        "Persönliche Angaben",
  "time_cpd_other_text",    "Q24a Sonstige Fortbildungen",              "Persönliche Angaben",
  "time_dem_own_heating",   "Q20 Heizung im eigenen Zuhause",           "Persönliche Angaben",
  "time_fb_comment",        "Feedback",                                 "Feedback"
) %>%
  mutate(position = row_number(),
         section  = fct_inorder(section))

old_question_times <- function(main, vignettes) {
  from_main <- main %>%
    select(resp_uid, starts_with("time_"), -time_interview) %>%
    pivot_longer(-resp_uid, names_to = "item", values_to = "seconds",
                 values_drop_na = TRUE) %>%
    mutate(item = str_replace(item, "^time_(tp|risk)_([1-5]).*$", "time_\\1_\\2"))

  from_vignettes <- bind_rows(
    vignettes %>%
      filter(!is.na(vig_time)) %>%
      transmute(resp_uid, item = str_c("vignette_", vig_num), seconds = vig_time),
    vignettes %>%
      filter(!is.na(vig_cost_time)) %>%
      transmute(resp_uid, item = "vignette_cost", seconds = vig_cost_time)
  )

  bind_rows(from_main, from_vignettes) %>%
    inner_join(OLD_QUESTIONNAIRE, by = "item")
}

old_add_last_question <- function(main, vignettes) {
  last <- old_question_times(main, vignettes) %>%
    slice_max(position, n = 1, with_ties = FALSE, by = resp_uid) %>%
    select(resp_uid, last_question_position = position,
           last_question = label, last_section = section)

  main %>%
    left_join(last, by = "resp_uid") %>%
    mutate(last_question_position = coalesce(last_question_position, 0L),
           last_question          = coalesce(last_question, "Keine Frage beantwortet"))
}

old_load_analysis_data <- function(completers_only = FALSE) {
  main      <- readRDS(file.path(CLEAN_DATA_DIR, "survey_main.rds"))
  vignettes <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes_long.rds"))

  main <- main %>%
    old_add_berufsgruppe() %>%
    old_add_last_question(vignettes)

  if (completers_only) main <- main %>% filter(is_complete)

  main <- old_add_preference_scores(main, quiet = TRUE)

  vignettes <- vignettes %>%
    inner_join(main %>% select(resp_uid, berufsgruppe), by = "resp_uid")

  list(main = main, vignettes = vignettes)
}

OLD_PRICE_FUELS        <- c("gas", "hpelec", "dh", "oil", "pellet")
OLD_PRICE_EURO_BELOW   <- 1
OLD_OIL_PER_LITRE      <- c(40, 140)
OLD_PELLETS_PER_KG     <- c(20, 70)
OLD_PRICE_VALID_FACTOR <- 3

OLD_PEXP_ANSWERS <- c("mehr als 10% niedriger", "bis zu 10 % niedriger",
                      "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher")
OLD_PEXP_RISE    <- c("bis zu 10 % höher", "mehr als 10% höher")

old_clean_reported_prices <- function(main, in_reference) {
  reference <- tibble()
  for (fuel in OLD_PRICE_FUELS) {
    reported <- main[[paste0("price_", fuel)]]
    treatment <- if_else(is.na(reported), "blank", "as reported")

    # (1) euro instead of cent
    is_euro <- !is.na(reported) & reported < OLD_PRICE_EURO_BELOW
    price <- if_else(is_euro, reported * 100, reported)
    treatment[is_euro] <- "euro"

    # (2) oil per litre, (3) pellets per kg
    is_per_unit <- rep(FALSE, length(price))
    if (fuel == "oil") {
      is_per_unit <- !is.na(price) &
        price >= OLD_OIL_PER_LITRE[1] & price <= OLD_OIL_PER_LITRE[2]
      price <- if_else(is_per_unit, price / 10, price)
    }
    if (fuel == "pellet") {
      is_per_unit <- !is.na(price) &
        price >= OLD_PELLETS_PER_KG[1] & price <= OLD_PELLETS_PER_KG[2]
      price <- if_else(is_per_unit, price / 5, price)
    }
    treatment[is_per_unit & !is_euro] <- "per litre or kg"
    treatment[is_per_unit & is_euro] <- "euro and per litre or kg"

    # (4) far from the median answer: not a price per kWh
    price_median <- median(price[in_reference], na.rm = TRUE)
    lower <- price_median / OLD_PRICE_VALID_FACTOR
    upper <- price_median * OLD_PRICE_VALID_FACTOR
    is_unusable <- !is.na(price) & (price < lower | price > upper)
    price[is_unusable] <- NA
    treatment[is_unusable] <- "not usable"

    main[[paste0("price_", fuel, "_kwh")]] <- price
    main[[paste0("price_", fuel, "_treatment")]] <- treatment
    reference <- bind_rows(reference, tibble(
      fuel = fuel, median = price_median, lower = lower, upper = upper,
      mean = mean(price[in_reference], na.rm = TRUE)
    ))
  }
  list(data = main, reference = reference)
}

OLD_HDD_YEAR <- 2025

old_add_heating_degree_days <- function(main) {
  hdd <- read.csv(HDD_FILE) %>%
    filter(indic_nrg == "HDD", TIME_PERIOD == OLD_HDD_YEAR) %>%
    mutate(nuts3 = geo, hdd = OBS_VALUE) %>%
    select(nuts3, hdd)
  # One row per region in hdd: relationship = "many-to-one" stops the join if
  # a region appeared twice
  n_rows <- nrow(main)
  main <- main %>%
    left_join(hdd, by = "nuts3", relationship = "many-to-one")
  stopifnot(nrow(main) == n_rows)
  main
}

# 1.2 R/add_preferences.R ------------------------------------------------------
OLD_STAIRCASES <- list(
  tp   = list(prefix = "tp_",   high = "B", option_a = "heute",
              what = "patience (option B = wait for the larger later payment)"),
  risk = list(prefix = "risk_", high = "A", option_a = "^Verlosung",
              what = "risk tolerance (option A = take the 50/50 lottery)")
)

old_staircase_nodes <- function(data, prefix) {
  cols <- str_subset(names(data), str_c("^", prefix, "[0-9]"))
  tibble(col   = cols,
         level = as.integer(str_sub(str_remove(cols, str_c("^", prefix)), 1, 1)),
         path  = str_to_upper(str_sub(str_remove(cols, str_c("^", prefix)), 2)))
}

old_check_staircase <- function(data, s) {
  nodes <- old_staircase_nodes(data, s$prefix)
  if (!nrow(nodes))
    stop("No ", s$prefix, "* columns in `data`.", call. = FALSE)

  for (v in nodes$col) {
    lv <- levels(data[[v]])
    if (length(lv) != 2)
      stop(v, " has ", length(lv), " levels, expected 2 (the two options).",
           call. = FALSE)
    if (!str_detect(lv[1], s$option_a))
      stop("The first level of ", v, ' is "', lv[1], '", which does not look like ',
           "option A (expected to match /", s$option_a, "/).\n",
           "The score depends on option A coming first - refusing to guess.",
           call. = FALSE)
  }
  nodes
}

old_walk_staircase <- function(data, s) {
  nodes <- old_check_staircase(data, s)
  n <- nrow(data)
  path <- rep("", n); cell <- rep(0L, n); depth <- rep(0L, n)
  active <- rep(TRUE, n)

  for (k in sort(unique(nodes$level))) {
    for (p in unique(path[active])) {
      v <- nodes$col[nodes$level == k & nodes$path == p]
      if (!length(v)) next          # this branch has no question at this level
      rows <- active & path == p
      vals <- data[[v]][rows]
      answered <- !is.na(vals)
      if (!any(answered)) next

      idx    <- which(rows)[answered]
      letter <- if_else(as.integer(vals[answered]) == 1L, "A", "B")
      path[idx]  <- str_c(path[idx], letter)
      cell[idx]  <- cell[idx] * 2L + as.integer(letter == s$high)
      depth[idx] <- k
    }
    active <- depth == k
  }

  stray <- integer(n)
  for (i in seq_len(nrow(nodes))) {
    on <- depth >= nodes$level[i] &
      str_sub(path, 1, str_length(nodes$path[i])) == nodes$path[i]
    stray <- stray + as.integer(!is.na(data[[nodes$col[i]]]) & !on)
  }

  reachable <- vapply(path, function(p) {
    d <- 0L
    for (k in sort(unique(nodes$level)))
      if (any(nodes$level == k & nodes$path == str_sub(p, 1, k - 1))) d <- k
    d
  }, integer(1), USE.NAMES = FALSE)

  tibble(path = path, cell = cell, depth = depth, max_depth = reachable,
         stray = stray,
         score = if_else(depth == 0L, NA_real_, (cell + 0.5) / 2^depth))
}

old_add_preference_scores <- function(data, standardise_on = NULL, min_levels = 1L,
                                      quiet = FALSE) {
  stopifnot(is.data.frame(data))
  if (!is.null(standardise_on)) {
    if (length(standardise_on) != nrow(data))
      stop("`standardise_on` must have one entry per row of `data` (",
           nrow(data), ").", call. = FALSE)
    standardise_on <- !is.na(standardise_on) & standardise_on
  }

  if (!"resp_uid" %in% names(data))
    stop("`data` has no resp_uid column.", call. = FALSE)
  first <- !duplicated(data$resp_uid)

  for (nm in names(OLD_STAIRCASES)) {
    s <- OLD_STAIRCASES[[nm]]
    r <- old_walk_staircase(data[first, , drop = FALSE], s)

    score <- if_else(r$depth >= min_levels, r$score, NA_real_)

    keep <- if (is.null(standardise_on)) rep(TRUE, length(score)) else standardise_on[first]
    m  <- mean(score[keep], na.rm = TRUE)
    sd <- stats::sd(score[keep], na.rm = TRUE)
    if (!is.finite(sd) || sd == 0)
      stop("Cannot standardise ", nm, ": the standardisation sample has ",
           sum(!is.na(score[keep])), " scored respondent(s).", call. = FALSE)

    per_resp <- tibble(resp_uid = data$resp_uid[first])
    per_resp[[str_c(nm, "_score")]]    <- score
    per_resp[[str_c(nm, "_z")]]        <- (score - m) / sd
    per_resp[[str_c(nm, "_cell")]]     <- if_else(r$depth == 0L, NA_integer_, r$cell)
    per_resp[[str_c(nm, "_levels")]]   <- r$depth
    per_resp[[str_c(nm, "_complete")]] <- r$depth > 0L & r$depth == r$max_depth

    data <- left_join(data, per_resp, by = "resp_uid")
  }
  data
}

# 1.3 R/ruecklauf/02_link.R ----------------------------------------------------
# resp_uid is added to the select(), so that the kept row can be compared
old_load_respondents <- function() {
  dat <- old_load_analysis_data(completers_only = FALSE)

  dat$main %>%
    mutate(person = coalesce(meta_token, resp_uid)) %>%
    arrange(desc(is_complete), desc(meta_lastpage)) %>%
    slice_head(n = 1, by = c(wave, person)) %>%
    select(resp_uid, wave, meta_token, is_complete, plz, bundesland,
           bundesland_source, berufsgruppe, target_group)
}

# 1.4 R/summary_statistics/00_settings.R and 01_plot_functions.R ---------------
OLD_TRIM_QUANTILES <- c(0.005, 0.995)
OLD_VALID_RANGE <- list(
  dem_birthyear     = c(1920, 2010),
  dem_training_year = c(1930, 2026)
)
OLD_NOT_TRIMMED <- c("tp_score", "risk_score")

old_outliers_to_na <- function(values, variable) {
  if (variable %in% OLD_NOT_TRIMMED) return(values)

  range <- OLD_VALID_RANGE[[variable]]
  if (!is.null(range)) values[which(values < range[1] | values > range[2])] <- NA

  if (any(!is.na(values))) {
    limits <- quantile(values, OLD_TRIM_QUANTILES, names = FALSE, na.rm = TRUE)
    values[which(values < limits[1] | values > limits[2])] <- NA
  }
  values
}

# 1.5 R/descriptives/00_settings.R and 02_data.R, sections 5 and 6 --------------
# As functions, so that they can run on any sample. co2_est_low and co2_est_high
# had no place in OUTLIER_VARIABLES of the descriptives; the data contract adds
# them, under the same rule.
OLD_OUTLIER_VARIABLES <- c("dem_birthyear", "dem_training_year", "firm_employees",
                           "adv_households", "co2_est_low", "co2_est_high")
OLD_SHARE_SUFFIXES <- c("hp", "pellet", "dh", "gas", "oil", "hybrid", "other")
OLD_SHARE_QUESTIONS <- list(
  "Q10b (recommendations)" = paste0("adv_share_", OLD_SHARE_SUFFIXES),
  "Q11b (installations)" = paste0("mix25_", OLD_SHARE_SUFFIXES)
)

old_outlier_section <- function(experts) {
  outlier_counts <- tibble()
  for (variable in OLD_OUTLIER_VARIABLES) {
    values <- experts[[variable]]
    cleaned <- old_outliers_to_na(values, variable)
    experts[[paste0(variable, "_clean")]] <- cleaned

    valid_range <- OLD_VALID_RANGE[[variable]]
    if (is.null(valid_range)) {
      n_impossible <- 0
    } else {
      n_impossible <- sum(values < valid_range[1] | values > valid_range[2],
                          na.rm = TRUE)
    }
    n_answered <- sum(!is.na(values))
    n_used <- sum(!is.na(cleaned))
    outlier_counts <- bind_rows(outlier_counts, tibble(
      variable = variable, answered = n_answered, impossible = n_impossible,
      trimmed = n_answered - n_impossible - n_used, used = n_used
    ))
  }
  list(data = experts, counts = outlier_counts)
}

old_share_section <- function(experts) {
  share_sums <- tibble()
  for (question in names(OLD_SHARE_QUESTIONS)) {
    share_variables <- OLD_SHARE_QUESTIONS[[question]]
    n_outside <- sum(experts[share_variables] < 0 |
                       experts[share_variables] > 100, na.rm = TRUE)
    if (n_outside > 0) {
      stop(n_outside, " shares of ", question, " lie outside 0 to 100: how ",
           "should they count?")
    }
    total <- rowSums(experts[share_variables], na.rm = TRUE)
    gave_shares <- total > 0
    for (variable in share_variables) {
      experts[[paste0(variable, "_filled")]] <- if_else(
        gave_shares, coalesce(as.numeric(experts[[variable]]), 0), NA_real_
      )
    }
    share_sums <- bind_rows(share_sums, tibble(
      question = question, answered = sum(gave_shares),
      not_100 = sum(gave_shares & abs(total - 100) > 1e-6)
    ))
  }
  list(data = experts, sums = share_sums)
}

# 1.6 R/analysis/01_run_specifications.R, section 2.1 ---------------------------
# As a function, so that it can run on any sample. The script's own copy of the
# price rule, the expected rises, the heating at home and the degree days. Left
# out: the treatments of the prices, which clean_reported_prices() has too; the log
# prices (_lc), a regressor of a model; and the stops on few answers, on prices at
# or below 0, and on nuts3 and degree days, which this comparison cannot reach.
OLD_OWN_HEATING_ANSWERS <- c("Wärmepumpe", "Pelletheizung", "Fernwärme",
                             "Gasheizung", "Ölheizung", "Hybridheizung", "Sonstiges")
OLD_HDD_UNIT <- 1000

old_section_2_1 <- function(experts_all) {
  in_groups <- experts_all$berufsgruppe %in% OLD_BERUFSGRUPPEN
  price_reference <- tibble()
  price_quartiles <- tibble()
  for (fuel in OLD_PRICE_FUELS) {
    reported <- experts_all[[paste0("price_", fuel)]]

    # (1) euro instead of cent
    is_euro <- !is.na(reported) & reported < OLD_PRICE_EURO_BELOW
    price <- if_else(is_euro, reported * 100, reported)

    # (2) oil per litre, (3) pellets per kg
    is_per_unit <- rep(FALSE, length(price))
    if (fuel == "oil") {
      is_per_unit <- !is.na(price) &
        price >= OLD_OIL_PER_LITRE[1] & price <= OLD_OIL_PER_LITRE[2]
      price <- if_else(is_per_unit, price / 10, price)
    }
    if (fuel == "pellet") {
      is_per_unit <- !is.na(price) &
        price >= OLD_PELLETS_PER_KG[1] & price <= OLD_PELLETS_PER_KG[2]
      price <- if_else(is_per_unit, price / 5, price)
    }

    # (4) far from the median answer: not a price per kWh
    price_median <- median(price[in_groups], na.rm = TRUE)
    lower <- price_median / OLD_PRICE_VALID_FACTOR
    upper <- price_median * OLD_PRICE_VALID_FACTOR
    is_unusable <- !is.na(price) & (price < lower | price > upper)
    price[is_unusable] <- NA

    price_mean <- mean(price[in_groups], na.rm = TRUE)
    experts_all[[paste0("price_", fuel, "_na")]] <- if_else(is.na(price), 1, 0)
    experts_all[[paste0("price_", fuel, "_c")]] <-
      if_else(is.na(price), 0, price - price_mean)

    price_reference <- bind_rows(price_reference, tibble(
      fuel = fuel, median = price_median, lower = lower, upper = upper,
      mean = price_mean
    ))
    quartiles <- quantile(price[in_groups], c(0.25, 0.75), na.rm = TRUE)
    price_quartiles <- bind_rows(price_quartiles, tibble(
      fuel = fuel, q25 = quartiles[[1]], q75 = quartiles[[2]]
    ))
  }

  pexp_reference <- tibble()
  for (fuel in OLD_PRICE_FUELS) {
    answer <- as.character(experts_all[[paste0("pexp_", fuel)]])
    unknown_answers <- setdiff(unique(answer[!is.na(answer)]), OLD_PEXP_ANSWERS)
    if (length(unknown_answers) > 0) {
      stop("pexp_", fuel, " has answers that are not in PEXP_ANSWERS: ",
           paste(unknown_answers, collapse = ", "))
    }
    expects_rise <- if_else(answer %in% OLD_PEXP_RISE, 1, 0)
    is_answered <- in_groups & !is.na(answer)
    rise_share <- mean(expects_rise[is_answered])
    experts_all[[paste0("pexp_", fuel, "_na")]] <- if_else(is.na(answer), 1, 0)
    experts_all[[paste0("pexp_", fuel, "_c")]] <-
      if_else(is.na(answer), 0, expects_rise - rise_share)
    pexp_reference <- bind_rows(pexp_reference, tibble(
      fuel = fuel, answered = sum(is_answered), share_rise = rise_share
    ))
  }

  own_heating <- as.character(experts_all$dem_own_heating)
  experts_all$own_heating <- if_else(is.na(own_heating), "Keine Angabe",
                                     own_heating)
  own_heating_share <- numeric(0)
  for (answer_level in c(OLD_OWN_HEATING_ANSWERS, "Keine Angabe")) {
    own_heating_share[answer_level] <-
      mean(experts_all$own_heating[in_groups] == answer_level)
  }
  experts_all$heat_pump_at_home <-
    if_else(experts_all$own_heating == "Wärmepumpe", 1, 0) -
    own_heating_share[["Wärmepumpe"]]

  hdd <- read.csv(HDD_FILE) %>%
    filter(indic_nrg == "HDD", TIME_PERIOD == OLD_HDD_YEAR) %>%
    mutate(nuts3 = geo, hdd = OBS_VALUE) %>%
    select(nuts3, hdd)
  experts_all <- experts_all %>%
    left_join(hdd, by = "nuts3", relationship = "many-to-one")
  hdd_mean <- mean(experts_all$hdd[in_groups], na.rm = TRUE)
  experts_all <- experts_all %>%
    mutate(hdd_c = if_else(is.na(hdd), 0, (hdd - hdd_mean) / OLD_HDD_UNIT))
  n_with_hdd <- sum(!is.na(experts_all$hdd[in_groups]))
  hdd_quartiles <- quantile(experts_all$hdd[in_groups], c(0.25, 0.75),
                            na.rm = TRUE)

  list(data = experts_all, price_reference = price_reference,
       price_quartiles = price_quartiles, pexp_reference = pexp_reference,
       own_heating_share = own_heating_share, hdd_mean = hdd_mean,
       hdd_quartiles = hdd_quartiles, n_with_hdd = n_with_hdd,
       n_experts = sum(in_groups))
}

# 2. The comparison ------------------------------------------------------------

#' How many values of two equally long vectors differ, with NA equal to NA and
#' numbers equal up to rounding error
#'
#' @param results the table so far
#' @param check   what is compared
#' @param old,new the two vectors
#' @return results, with one more row
compare_values <- function(results, check, old, new) {
  if (length(old) != length(new)) {
    stop(check, ": ", length(old), " old values against ", length(new), " new ones")
  }
  if (is.numeric(old) && is.numeric(new)) {
    is_same_value <- abs(old - new) < 1e-9
  } else {
    is_same_value <- as.character(old) == as.character(new)
  }
  is_same <- (is.na(old) & is.na(new)) | coalesce(is_same_value, FALSE)
  bind_rows(results, tibble(check = check, compared = length(old),
                            differ = sum(!is_same)))
}

# 3. A stressed copy of the data -----------------------------------------------
# What the simulation does not produce. Every change is one the real data can
# hold.
synthetic_dir <- CLEAN_DATA_DIR
stressed_dir <- file.path(SECURE_DIR, "stressed")
dir.create(stressed_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20261110)
stressed_main <- readRDS(file.path(synthetic_dir, "survey_main.rds"))
stressed_long <- readRDS(file.path(synthetic_dir, "vignettes_long.rds"))

# Repeat completions: 40 completers start again and complete again, on the same
# page or a later one. Their second answers differ, so that the row that stays
# matters.
repeat_rows <- stressed_main %>%
  filter(is_complete, !is.na(meta_token), !arm_conflict) %>%
  slice_sample(n = 40) %>%
  mutate(new_id = meta_id + 100000L,
         new_uid = paste0(wave, "_", new_id))
repeat_mains <- repeat_rows %>%
  mutate(resp_uid = new_uid, meta_id = new_id,
         meta_lastpage = meta_lastpage +
           sample(c(0L, 0L, 1L), n(), replace = TRUE),
         price_gas = price_gas * 1.2,
         price_hpelec = price_hpelec * 0.8) %>%
  select(-new_id, -new_uid)
repeat_vignettes <- stressed_long %>%
  inner_join(repeat_rows %>% select(resp_uid, new_id, new_uid), by = "resp_uid",
             relationship = "many-to-one") %>%
  mutate(resp_uid = new_uid, meta_id = new_id) %>%
  select(-new_id, -new_uid)
stressed_main <- bind_rows(stressed_main, repeat_mains) %>%
  arrange(resp_uid)
stressed_long <- bind_rows(stressed_long, repeat_vignettes) %>%
  arrange(resp_uid, vig_num, vig_arm)

# An expert in both arms who is not a further attempt: stays in experts, not in
# vignettes, as in the real data set
conflict_candidates <- stressed_main %>%
  group_by(wave, meta_token) %>%
  mutate(n_rows = n()) %>%
  ungroup() %>%
  filter(!is_complete, n_rows == 1, !is.na(meta_token),
         n_vignettes_answered >= 2, n_vignettes_answered < 6, !arm_conflict)
stressed_conflict_uid <- conflict_candidates$resp_uid[1]
conflict_rows <- stressed_long %>%
  filter(resp_uid == stressed_conflict_uid, !is.na(vig_rec))
conflict_other_arm <- if_else(conflict_rows$vig_arm[1] == "fw", "nofw", "fw")
stressed_long <- bind_rows(
  stressed_long %>% filter(resp_uid != stressed_conflict_uid),
  conflict_rows %>% mutate(arm_conflict = TRUE),
  conflict_rows %>%
    mutate(arm_conflict = TRUE,
           vig_arm = factor(conflict_other_arm, levels = c("nofw", "fw")))
) %>%
  arrange(resp_uid, vig_num, vig_arm)
is_conflict <- stressed_main$resp_uid == stressed_conflict_uid
stressed_main$arm_conflict[is_conflict] <- TRUE
stressed_main$vig_arm[is_conflict] <- NA

# Prices: euro per litre or kg, the edges of the bands, zero, negative, not usable
answered_oil <- sample(which(!is.na(stressed_main$price_oil)), 11)
stressed_main$price_oil[answered_oil] <-
  c(0.9, 0.4, 1, 1.4, 40, 140, 39.9, 140.1, 0, -5, 1000)
answered_pellet <- sample(which(!is.na(stressed_main$price_pellet)), 10)
stressed_main$price_pellet[answered_pellet] <-
  c(0.4, 0.2, 0.7, 20, 70, 19.9, 70.1, 0, -5, 1000)
answered_gas <- sample(which(!is.na(stressed_main$price_gas)), 4)
stressed_main$price_gas[answered_gas] <- c(1, 0.999, 0, 1e6)

# Q2: a page with a tick but no answer for HVAC; "Sonstige" next to a trade; an
# office of engineers only: every adviser of the simulation ticks energy advice
q2_rows <- sample(which(!is.na(stressed_main$firm_cat_hvac)), 18)
stressed_main$firm_cat_hvac[q2_rows[1:4]] <- NA
stressed_main$firm_cat_chimney[q2_rows[1:4]] <- "Ja"
stressed_main$firm_cat_other[q2_rows[5:8]] <- "Sonstige Beratung"
stressed_main$firm_cat_hvac[q2_rows[5:6]] <- "Ja"
stressed_main$firm_cat_chimney[q2_rows[9:12]] <- NA
for (column in c("firm_cat_hvac", "firm_cat_chimney", "firm_cat_energyadvice")) {
  stressed_main[[column]][q2_rows[13:18]] <- "Nicht Gewählt"
}
stressed_main$firm_cat_engineering[q2_rows[13:18]] <- "Ja"

# Staircases: a skipped level, and an answer on a branch never entered
skipped_rows <- which(!is.na(stressed_main$tp_3aa) | !is.na(stressed_main$tp_3ab))
skipped_rows <- sample(skipped_rows, 10)
stressed_main$tp_2a[skipped_rows] <- NA
abandoned_rows <- sample(which(!is.na(stressed_main$risk_2a)), 10)
stressed_main$risk_2b[abandoned_rows] <- levels(stressed_main$risk_2b)[1]

# LimeSurvey has no page for an attempt that never submitted one
lastpage_rows <- sample(which(stressed_main$meta_lastpage < 3), 20)
stressed_main$meta_lastpage[lastpage_rows] <- NA

# Shares that are all zero count as no shares
zero_share_rows <- sample(which(!is.na(stressed_main$adv_share_hp)), 6)
for (suffix in OLD_SHARE_SUFFIXES) {
  stressed_main[[paste0("adv_share_", suffix)]][zero_share_rows] <- 0
}

saveRDS(stressed_main, file.path(stressed_dir, "survey_main.rds"))
saveRDS(stressed_long, file.path(stressed_dir, "vignettes_long.rds"))

# 4. Old against new, on both data sets ----------------------------------------
results <- tibble()
outlier_pool_differences <- tibble()
repeat_completion_differences <- tibble()
scenario_dirs <- c("as simulated" = synthetic_dir, "stressed" = stressed_dir)
# The columns that docs/data_contract.md promises to the other scripts
contract_fuels <- c("gas", "hpelec", "dh", "oil", "pellet")
contract_technologies <- c("hp", "pellet", "dh", "gas", "oil", "hybrid", "other")
contract_expert_columns <- c(
  "resp_uid", "person", "n_attempts", "is_complete", "expert_group",
  "expert_group_note", "arm", "last_question", "last_section",
  "last_question_position", paste0("price_", contract_fuels, "_kwh"),
  paste0("price_", contract_fuels, "_treatment"),
  paste0("pexp_", contract_fuels, "_rise"), "own_heating", "heat_pump_at_home",
  "hdd", "tp_score", "tp_cell", "tp_levels", "tp_complete", "risk_score",
  "risk_cell", "risk_levels", "risk_complete", "dem_birthyear_clean",
  "dem_training_year_clean", "firm_employees_clean", "adv_households_clean",
  "co2_est_low_clean", "co2_est_high_clean", "age", "interview_minutes",
  paste0("adv_share_", contract_technologies, "_filled"),
  paste0("mix25_", contract_technologies, "_filled")
)
contract_vignette_columns <- c(
  "resp_uid", "vig_num", "vig_arm", "arm_source", "vig_rec", "vig_rec_other",
  "vignette_id", "att_image", "att_couple_age", "att_income",
  "att_current_heating", "att_replacement_timing", "att_build_year",
  "att_heat_demand", "att_heat_distribution", "att_renovation", "vig_time",
  "vig_group_time", "vig_cost", "vig_cost_source", "vig_cost_time",
  "expert_group", "is_complete"
)
for (scenario in names(scenario_dirs)) {
  CLEAN_DATA_DIR <- scenario_dirs[[scenario]]
  compared <- tibble()

  # capture.output(): the tables that the step prints stay out of this log
  invisible(capture.output(
    source(file.path("R", "cleaning", "06_experts_vignettes.R"),
           encoding = "UTF-8")
  ))

  # 4.1 The persons ------------------------------------------------------------
  old_persons <- old_load_respondents() %>%
    arrange(resp_uid)
  compared <- compare_values(compared, "persons: the same rows stay",
                             old_persons$resp_uid, experts$resp_uid)
  compared <- compare_values(compared, "persons: is_complete",
                             old_persons$is_complete, experts$is_complete)
  person_counts <- survey_main %>%
    mutate(person = coalesce(meta_token, resp_uid)) %>%
    count(wave, person, name = "attempts")
  counts_of_kept <- experts %>%
    select(wave, person) %>%
    left_join(person_counts, by = c("wave", "person"),
              relationship = "one-to-one")
  compared <- compare_values(compared, "persons: n_attempts",
                             counts_of_kept$attempts, experts$n_attempts)

  # 4.2 The expert group, the last question and the preference scores ----------
  # Run by the old code over all attempts, then read at the rows that stay
  old_all <- old_load_analysis_data(completers_only = FALSE)
  old_kept <- old_all$main[match(experts$resp_uid, old_all$main$resp_uid), ]

  old_group_names <- c("SHK-Handwerk" = "HVAC installers",
                       "Schornsteinfeger" = "Chimney sweeps",
                       "Energieberater" = "Energy advisers",
                       "Keine Zuordnung" = NA_character_)
  compared <- compare_values(
    compared, "expert_group against add_berufsgruppe()",
    unname(old_group_names[as.character(old_kept$berufsgruppe)]),
    as.character(experts$expert_group)
  )
  compared <- compare_values(
    compared, "expert_group_note: no group exactly as NA in the old code",
    is.na(old_kept$berufsgruppe) | old_kept$berufsgruppe == "Keine Zuordnung",
    experts$expert_group_note != "assigned"
  )

  compared <- compare_values(compared, "last question: the same order of items",
                             OLD_QUESTIONNAIRE$item, QUESTIONNAIRE$item)
  compared <- compare_values(compared, "last_question_position",
                             old_kept$last_question_position,
                             experts$last_question_position)
  has_question <- experts$last_question_position > 0
  position <- experts$last_question_position[has_question]
  compared <- compare_values(compared, "last_question: the label of its position",
                             QUESTIONNAIRE$label[position],
                             experts$last_question[has_question])
  compared <- compare_values(compared, "last_section: the section of its position",
                             as.character(QUESTIONNAIRE$section[position]),
                             experts$last_section[has_question])
  compared <- compare_values(compared, "last_section: NA without a question",
                             is.na(old_kept$last_section),
                             is.na(experts$last_section))

  for (staircase_name in c("tp", "risk")) {
    for (suffix in c("score", "cell", "levels", "complete")) {
      column <- paste0(staircase_name, "_", suffix)
      compared <- compare_values(compared, column,
                                 old_kept[[column]], experts[[column]])
    }
    walked <- old_walk_staircase(experts, OLD_STAIRCASES[[staircase_name]])
    report_row <- preference_report %>%
      filter(staircase == staircase_name)
    compared <- compare_values(
      compared, paste0("preference report: ", staircase_name),
      c(sum(!is.na(walked$score)),
        sum(walked$depth > 0 & walked$depth < walked$max_depth),
        sum(walked$stray > 0)),
      c(report_row$scored, report_row$broke_off, report_row$with_stray_answers)
    )
  }

  # 4.3 The prices -------------------------------------------------------------
  in_reference_new <- experts$is_complete & !is.na(experts$expert_group)
  old_prices <- old_clean_reported_prices(experts, in_reference_new)
  for (fuel in OLD_PRICE_FUELS) {
    for (suffix in c("_kwh", "_treatment")) {
      column <- paste0("price_", fuel, suffix)
      compared <- compare_values(compared, column,
                                 old_prices$data[[column]], experts[[column]])
    }
  }
  for (column in c("median", "lower", "upper", "mean")) {
    compared <- compare_values(
      compared, paste0("reference prices: ", column, " of clean_reported_prices()"),
      old_prices$reference[[column]], reference_values$prices[[column]]
    )
  }

  # 4.4 The rest of section 2.1, on the persons --------------------------------
  old_kept_completers <- old_load_analysis_data(completers_only = TRUE)$main %>%
    filter(resp_uid %in% experts$resp_uid)
  old_section <- old_section_2_1(old_kept_completers)
  old_columns <- old_section$data
  new_at_old <- experts[match(old_columns$resp_uid, experts$resp_uid), ]

  for (column in c("median", "lower", "upper", "mean")) {
    compared <- compare_values(
      compared, paste0("reference prices: ", column, " of section 2.1"),
      old_section$price_reference[[column]], reference_values$prices[[column]]
    )
  }
  for (column in c("q25", "q75")) {
    compared <- compare_values(
      compared, paste0("reference prices: ", column, " of section 2.1"),
      old_section$price_quartiles[[column]], reference_values$prices[[column]]
    )
  }
  for (k in seq_along(OLD_PRICE_FUELS)) {
    fuel <- OLD_PRICE_FUELS[k]
    price_new <- new_at_old[[paste0("price_", fuel, "_kwh")]]
    price_centred <- price_new - reference_values$prices$mean[k]
    compared <- compare_values(
      compared, paste0("price_", fuel, "_na of section 2.1"),
      old_columns[[paste0("price_", fuel, "_na")]], if_else(is.na(price_new), 1, 0)
    )
    compared <- compare_values(
      compared, paste0("price_", fuel, "_c of section 2.1"),
      old_columns[[paste0("price_", fuel, "_c")]], coalesce(price_centred, 0)
    )
    rise_new <- new_at_old[[paste0("pexp_", fuel, "_rise")]]
    rise_centred <- rise_new - reference_values$expectations$share_rise[k]
    compared <- compare_values(
      compared, paste0("pexp_", fuel, "_na of section 2.1"),
      old_columns[[paste0("pexp_", fuel, "_na")]], if_else(is.na(rise_new), 1, 0)
    )
    compared <- compare_values(
      compared, paste0("pexp_", fuel, "_c of section 2.1"),
      old_columns[[paste0("pexp_", fuel, "_c")]], coalesce(rise_centred, 0)
    )
  }
  for (column in c("answered", "share_rise")) {
    compared <- compare_values(
      compared, paste0("reference expectations: ", column),
      old_section$pexp_reference[[column]], reference_values$expectations[[column]]
    )
  }
  compared <- compare_values(compared, "reference own heating: names",
                             names(old_section$own_heating_share),
                             names(reference_values$own_heating))
  compared <- compare_values(compared, "reference own heating: shares",
                             unname(old_section$own_heating_share),
                             unname(reference_values$own_heating))
  compared <- compare_values(compared, "own_heating",
                             old_columns$own_heating, new_at_old$own_heating)
  compared <- compare_values(
    compared, "heat_pump_at_home, centred as in section 2.1",
    old_columns$heat_pump_at_home,
    new_at_old$heat_pump_at_home - reference_values$own_heating[["Wärmepumpe"]]
  )
  compared <- compare_values(
    compared, "reference degree days: mean, q25, q75, n",
    c(old_section$hdd_mean, unname(old_section$hdd_quartiles),
      old_section$n_with_hdd),
    c(reference_values$hdd$mean, reference_values$hdd$q25,
      reference_values$hdd$q75, reference_values$hdd$n)
  )
  hdd_centred <- (new_at_old$hdd - reference_values$hdd$mean) / OLD_HDD_UNIT
  compared <- compare_values(compared, "hdd_c of section 2.1",
                             old_columns$hdd_c, coalesce(hdd_centred, 0))
  compared <- compare_values(compared, "reference values: n_experts",
                             old_section$n_experts, reference_values$n_experts)

  # 4.5 The degree days, all persons -------------------------------------------
  old_hdd <- old_add_heating_degree_days(experts %>% select(-hdd))
  compared <- compare_values(compared, "hdd of add_heating_degree_days()",
                             old_hdd$hdd, experts$hdd)

  # 4.6 The shares, all persons ------------------------------------------------
  old_shares <- old_share_section(experts)
  for (question in names(OLD_SHARE_QUESTIONS)) {
    for (variable in OLD_SHARE_QUESTIONS[[question]]) {
      column <- paste0(variable, "_filled")
      compared <- compare_values(compared, column,
                                 old_shares$data[[column]], experts[[column]])
    }
  }
  compared <- compare_values(compared, "share_sums: answered",
                             old_shares$sums$answered, share_sums$answered)
  compared <- compare_values(compared, "share_sums: not_100",
                             old_shares$sums$not_100, share_sums$not_100)

  # 4.7 The outlier rule -------------------------------------------------------
  # The old rule over everyone who answered gives the new columns...
  old_outliers <- old_outlier_section(experts)
  for (variable in OLD_OUTLIER_VARIABLES) {
    column <- paste0(variable, "_clean")
    compared <- compare_values(compared, column,
                               old_outliers$data[[column]], experts[[column]])
  }
  for (column in c("answered", "impossible", "trimmed", "used")) {
    compared <- compare_values(compared, paste0("outlier_counts: ", column),
                               old_outliers$counts[[column]],
                               outlier_counts[[column]])
  }
  compared <- compare_values(compared, "age",
                             SURVEY_YEAR - old_outliers$data$dem_birthyear_clean,
                             experts$age)
  # As R/descriptives/07_response_times.R and R/summary_statistics/02_sample.R
  compared <- compare_values(compared, "interview_minutes",
                             experts$time_interview / 60,
                             experts$interview_minutes)

  # ...and this is what the pool changes: the old rule over the completers of the
  # three groups, against the new columns, at those experts. Intended (Max,
  # 2026-11-09), but it should be small.
  old_pool <- old_kept_completers %>%
    filter(berufsgruppe %in% OLD_BERUFSGRUPPEN)
  new_at_pool <- experts[match(old_pool$resp_uid, experts$resp_uid), ]
  for (variable in OLD_OUTLIER_VARIABLES) {
    answers <- old_pool[[variable]]
    old_clean <- old_outliers_to_na(answers, variable)
    new_clean <- new_at_pool[[paste0(variable, "_clean")]]
    outlier_pool_differences <- bind_rows(outlier_pool_differences, tibble(
      scenario = scenario, variable = variable,
      answered = sum(!is.na(answers)),
      only_old_keeps = sum(!is.na(old_clean) & is.na(new_clean)),
      only_new_keeps = sum(is.na(old_clean) & !is.na(new_clean))
    ))
  }

  # 4.8 The repeat completions -------------------------------------------------
  # The old models counted every completed questionnaire. Section 2.1 on all of
  # them, against the reference values of the persons. Equal without repeat
  # completions, as in the simulation; else this is what one row per person
  # changes.
  old_all_completers <- old_load_analysis_data(completers_only = TRUE)$main
  old_section_all <- old_section_2_1(old_all_completers)
  n_repeat_completions <- sum(survey_main$is_complete) - sum(experts$is_complete)
  if (n_repeat_completions == 0) {
    compared <- compare_values(
      compared, "without repeat completions: reference of all questionnaires",
      c(old_section_all$price_reference$mean,
        old_section_all$pexp_reference$share_rise, old_section_all$hdd_mean),
      c(reference_values$prices$mean, reference_values$expectations$share_rise,
        reference_values$hdd$mean)
    )
  }
  for (k in seq_along(OLD_PRICE_FUELS)) {
    repeat_completion_differences <- bind_rows(
      repeat_completion_differences,
      tibble(scenario = scenario, repeat_completions = n_repeat_completions,
             fuel = OLD_PRICE_FUELS[k],
             old_mean = old_section_all$price_reference$mean[k],
             new_mean = reference_values$prices$mean[k])
    )
  }

  # 4.9 The arms and the vignettes ---------------------------------------------
  old_vignettes <- old_all$vignettes %>%
    filter(resp_uid %in% experts$resp_uid, !arm_conflict) %>%
    arrange(resp_uid, vig_num)
  old_arms <- old_vignettes %>%
    filter(!is.na(vig_arm)) %>%
    mutate(arm = as.character(vig_arm)) %>%
    distinct(resp_uid, arm)
  stopifnot(!anyDuplicated(old_arms$resp_uid))
  compared <- compare_values(
    compared, "arm",
    old_arms$arm[match(experts$resp_uid, old_arms$resp_uid)],
    as.character(experts$arm)
  )
  compared <- compare_values(compared, "vignettes: the same rows",
                             paste(old_vignettes$resp_uid, old_vignettes$vig_num),
                             paste(vignettes$resp_uid, vignettes$vig_num))
  vignette_columns <- c(
    "vig_arm", "arm_source", "vig_rec", "vig_rec_other", "vignette_id",
    "vig_time", "vig_group_time", "vig_cost", "vig_cost_source", "vig_cost_time",
    "vig_group_cost_time", "att_image", "att_couple_age", "att_income",
    "att_current_heating", "att_replacement_timing", "att_build_year",
    "att_heat_demand", "att_heat_distribution", "att_renovation"
  )
  for (column in vignette_columns) {
    compared <- compare_values(compared, paste0("vignettes: ", column),
                               old_vignettes[[column]], vignettes[[column]])
  }
  compared <- compare_values(
    compared, "vignettes: expert_group",
    unname(old_group_names[as.character(old_vignettes$berufsgruppe)]),
    as.character(vignettes$expert_group)
  )
  n_rows_of_person <- vignettes %>%
    count(resp_uid)
  compared <- compare_values(compared, "vignettes: six rows per person",
                             rep(N_VIGNETTES, nrow(n_rows_of_person)),
                             n_rows_of_person$n)
  compared <- compare_values(
    compared, "vignettes: persons left out are those in both arms",
    experts$arm_conflict, !(experts$resp_uid %in% vignettes$resp_uid)
  )

  # 4.10 The columns of the data contract --------------------------------------
  compared <- compare_values(compared, "contract: the columns of experts",
                             rep(TRUE, length(contract_expert_columns)),
                             contract_expert_columns %in% names(experts))
  compared <- compare_values(compared, "contract: the columns of vignettes",
                             rep(TRUE, length(contract_vignette_columns)),
                             contract_vignette_columns %in% names(vignettes))
  compared <- compare_values(compared, "contract: the levels of expert_group",
                             EXPERT_GROUPS, levels(experts$expert_group))
  compared <- compare_values(compared, "contract: the levels of arm",
                             c("fw", "nofw"), levels(experts$arm))

  results <- bind_rows(results, compared %>%
                         mutate(scenario = scenario, .before = 1))
  message(scenario, ": ", nrow(survey_main), " questionnaires, ", nrow(experts),
          " persons, ", sum(experts$arm_conflict), " in both arms, ",
          n_repeat_completions, " repeat completions")
}
CLEAN_DATA_DIR <- synthetic_dir

# 5. The report on the stressed data, with the sheets of the raw stage ---------
# The objects of step 06 above are still those of the stressed data. The raw
# stage does not run on synthetic data, so its tables are stand-ins here: this
# shows that the sheets are written, not what they say.
source(file.path("R", "cleaning", "00_functions.R"), encoding = "UTF-8")
REPORT_DIR <- file.path(stressed_dir, "report")
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)
raw_checks <- tibble(check = "stand-in", value = "1")
harmonisation_log <- tibble(variable = "stand-in", action = "stand-in",
                            detail = "stand-in")
recode_summary <- tibble(variable = "stand-in", source = "stand-in", n = 1L)
data_quality <- tibble(variable = "stand-in", rule = "stand-in", n = 1L,
                       note = "stand-in")
source(file.path("R", "cleaning", "07_report.R"), encoding = "UTF-8")

report_sheets <- excel_sheets(file.path(REPORT_DIR, "cleaning_report.xlsx"))
raw_stage_sheets <- c("raw_checks", "harmonisation", "recodes", "data_quality")
n_repeat_in_report <-
  attempts_sheet$n[attempts_sheet$step == "Repeat completions of one invitation"]
persons_row <- overview[overview$step == "Furthest attempt of each invitation", ]
oil_of_everyone <- prices[prices$fuel == "oil" & prices$sample == "All persons", ]
compared <- tibble()
compared <- compare_values(compared, "report: the sheets of the raw stage exist",
                           rep(TRUE, 4), raw_stage_sheets %in% report_sheets)
compared <- compare_values(compared, "report: xlsx and rds have the same sheets",
                           names(cleaning_report), report_sheets)
compared <- compare_values(compared, "report: the person in both arms",
                           sum(experts$arm_conflict), persons_row$arm_both)
compared <- compare_values(compared, "report: the repeat completions",
                           n_repeat_completions, n_repeat_in_report)
compared <- compare_values(
  compared, "report: oil, euro per litre",
  sum(experts$price_oil_treatment == "euro and per litre or kg"),
  oil_of_everyone$euro_and_per_litre_or_kg
)
compared <- compare_values(compared, "report: a description for every column",
                           0L, sum(variables$description == "Not documented"))
results <- bind_rows(results, compared %>%
                       mutate(scenario = "stressed, the report", .before = 1))

# 6. Result --------------------------------------------------------------------
results_by_scenario <- results %>%
  group_by(scenario) %>%
  summarise(comparisons = n(), values_compared = sum(compared),
            comparisons_that_differ = sum(differ > 0))
print(results_by_scenario)

cat("\nIntended: the outlier rule over everyone who answered, at the completers",
    "of the three groups\n")
print(outlier_pool_differences, n = Inf)

cat("\nIntended: one row per person. The mean reference price in cent: over all",
    "completed questionnaires, as in the old models, and over the persons\n")
print(repeat_completion_differences, n = Inf)

differing_comparisons <- results %>%
  filter(differ > 0)
if (nrow(differing_comparisons) > 0) {
  cat("\nThese comparisons differ:\n")
  print(differing_comparisons, n = Inf)
  message(nrow(differing_comparisons), " comparisons differ")
  if (!interactive()) {
    quit(status = 1)
  }
} else {
  message("All ", nrow(results),
          " comparisons equal: the port reproduces the old functions.")
}
