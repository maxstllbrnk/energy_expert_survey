# =============================================================================
# prepare_analysis_data.R  -  bring the cleaned data into the shape analyses use
# =============================================================================
# The cleaning pipeline stops at clean data and deliberately makes no analysis
# decisions. The decisions that every analysis shares live here, as functions,
# so that figures, tables and regressions all make them the same way:
#
#   add_berufsgruppe()     one occupational group per expert, from Q2
#   question_times()       response times per question, in questionnaire order
#   add_last_question()    the last question an expert answered
#   load_analysis_data()   reads both data sets and applies all of the above,
#                          plus the time and risk preference scores
#
# and two that an analysis applies itself, because they depend on its sample:
#
#   clean_reported_prices()    the energy prices of Q15 as cent per kWh
#   add_heating_degree_days()  the degree days of the expert's NUTS3 region
#
# Usage, from the project root:
#
#   source("R/prepare_analysis_data.R")      # also loads config.R
#   dat <- load_analysis_data()                          # every expert
#   dat <- load_analysis_data(completers_only = TRUE)    # submitted only
#   dat$main        one row per expert
#   dat$vignettes   one row per expert x vignette
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).", call. = FALSE)

source("R/add_preferences.R")   # add_preference_scores(); sources config.R itself


# =============================================================================
# Berufsgruppe
# =============================================================================
# Q2 ("Welcher Kategorie bzw. welchen Kategorien ordnen Sie Ihr Unternehmen
# zu?") allows several ticks, so one expert can fall into several categories -
# 70 tick both SHK and Energieberatung, 150 both Schornsteinfeger and
# Energieberatung. Every expert gets exactly ONE group, by this priority:
#
#   1. SHK-Handwerk      ticked "Sanitär-, Heizungs-, Klimatechnik"
#   2. Schornsteinfeger  ticked "Schornsteinfeger", but not SHK
#   3. Energieberater    ticked "Energieberatungsbüro" or
#                        "Architektur- / Bauingenieurbüro / sonstiges
#                        Ingenieurbüro", or filled in "Sonstige" but neither trade
#   -  NA                did not answer Q2
#
# So the trades take priority over energy advice, SHK over Schornsteinfeger,
# and architecture and engineering offices count as Energieberater. This rule
# was set by the project team in September 2026. To change it, reorder the
# case_when() below; nothing else depends on the order.
#
# On the data of September 2026: 617 SHK-Handwerk, 670 Schornsteinfeger,
# 2,460 Energieberater, 177 Keine Zuordnung, 252 did not answer Q2.
BERUFSGRUPPEN <- c("SHK-Handwerk", "Schornsteinfeger", "Energieberater")

add_berufsgruppe <- function(main) {
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
      berufsgruppe = factor(berufsgruppe, levels = c(BERUFSGRUPPEN, "Keine Zuordnung"))
    )
}

# =============================================================================
# the questionnaire, in the order it is shown
# =============================================================================
# One row per question (page), with a short German label and its section.
# `item` is the name of the question's response-time column in survey_main,
# with two exceptions that question_times() builds:
#   - each staircase decision is one item (time_tp_2a and time_tp_2b are both
#     "time_tp_2": every expert sees exactly one of them);
#   - vignette_1 ... vignette_6 and vignette_cost come from vignettes_long.
# Questions hidden by routing stay in the list; experts who did not see them
# simply have no time for them.
QUESTIONNAIRE <- tribble(
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


# =============================================================================
# response times per question
# =============================================================================
# One row per expert x question they submitted, with the seconds they spent on
# it and the question's label, section and position from QUESTIONNAIRE.
question_times <- function(main, vignettes) {
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
    inner_join(QUESTIONNAIRE, by = "item")
}


# =============================================================================
# where did an expert stop?
# =============================================================================
# LimeSurvey's own page counter, meta_lastpage, cannot be read as a question:
# pages hidden by routing still count, so the same page number stands for
# different questions for different experts. The response times can: a question
# has a time exactly when the expert submitted its page. The last question with
# a time is therefore the last question the expert answered.
#
# Adds last_question (label), last_section and last_question_position (0 for
# experts who never submitted a page).
add_last_question <- function(main, vignettes) {
  last <- question_times(main, vignettes) %>%
    slice_max(position, n = 1, with_ties = FALSE, by = resp_uid) %>%
    select(resp_uid, last_question_position = position,
           last_question = label, last_section = section)

  main %>%
    left_join(last, by = "resp_uid") %>%
    mutate(last_question_position = coalesce(last_question_position, 0L),
           last_question          = coalesce(last_question, "Keine Frage beantwortet"))
}


# =============================================================================
# load_analysis_data()
# =============================================================================
# Reads survey_main and vignettes_long from CLEAN_DATA_DIR and returns
#   $main        one row per expert, with berufsgruppe, last_question and the
#                preference scores (tp_* and risk_*, see R/add_preferences.R)
#   $vignettes   one row per expert x vignette, with berufsgruppe
#
# completers_only = TRUE keeps only experts who submitted the questionnaire.
# The preference z-scores are standardised on whichever sample is kept.
load_analysis_data <- function(completers_only = FALSE) {
  main      <- readRDS(file.path(CLEAN_DATA_DIR, "survey_main.rds"))
  vignettes <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes_long.rds"))

  main <- main %>%
    add_berufsgruppe() %>%
    add_last_question(vignettes)

  if (completers_only) main <- main %>% filter(is_complete)

  main <- add_preference_scores(main, quiet = TRUE)

  vignettes <- vignettes %>%
    inner_join(main %>% select(resp_uid, berufsgruppe), by = "resp_uid")

  list(main = main, vignettes = vignettes)
}


# =============================================================================
# energy prices (Q15) and expected price developments (Q17)
# =============================================================================
# Q15 asks what households among the expert's clients paid per kWh of each
# fuel in January and February 2026, in cent. The question asks experts to
# leave a field blank if none of their clients uses the fuel; a blank also
# covers "do not know". Answers in another unit are converted, in this order:
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
#
# These are the rules of the mixed logit models. Section 1 of
# R/analysis/01_run_specifications.R still has its own copy of them; it moves
# to clean_reported_prices() once its estimation run of October 2026 has
# finished (Max, 2026-10-28).
PRICE_FUELS        <- c("gas", "hpelec", "dh", "oil", "pellet")
PRICE_EURO_BELOW   <- 1
OIL_PER_LITRE      <- c(40, 140)
PELLETS_PER_KG     <- c(20, 70)
PRICE_VALID_FACTOR <- 3

# Q17: the price of each fuel in five years, compared with the start of 2026.
# The answers exactly as in the data (note "10%" against "10 %"); the question
# asks experts to leave a fuel blank if it is not available in their area. An
# expert expects a rise if the answer is one of PEXP_RISE, both answers
# "höher" (Max, 2026-10-27). The question asks about the price, not the price
# after inflation: with inflation, "bis zu 10 % höher" is about flat in real
# terms.
PEXP_ANSWERS <- c("mehr als 10% niedriger", "bis zu 10 % niedriger",
                  "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher")
PEXP_RISE    <- c("bis zu 10 % höher", "mehr als 10% höher")

#' The reported prices of Q15 as cent per kWh, by rules (1) to (4) above
#'
#' @param main         one row per expert, with price_<fuel> for PRICE_FUELS
#' @param in_reference TRUE for the experts whose answers set the median of
#'                     rule (4) and the mean: in the mixed logit models, the
#'                     completers of the three Berufsgruppen in both arms
#' @return list:
#'   data       main, plus price_<fuel>_kwh (cent per kWh; NA if blank or not
#'              usable) and price_<fuel>_treatment ("blank", "as reported",
#'              "euro", "per litre or kg", "euro and per litre or kg",
#'              "not usable")
#'   reference  one row per fuel: median (after (1) to (3)), lower and upper
#'              bound of rule (4), mean of the usable prices
clean_reported_prices <- function(main, in_reference) {
  reference <- tibble()
  for (fuel in PRICE_FUELS) {
    reported <- main[[paste0("price_", fuel)]]
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
    price_median <- median(price[in_reference], na.rm = TRUE)
    lower <- price_median / PRICE_VALID_FACTOR
    upper <- price_median * PRICE_VALID_FACTOR
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


# =============================================================================
# heating degree days
# =============================================================================
# Of the NUTS3 region of the expert's postcode (nuts3, R/cleaning/03_recode.R)
# in HDD_YEAR, from Eurostat (HDD_FILE in config.R), one year as in the mixed
# logit models (Max, 2026-10-27). In 2025 the German regions range from 2,255
# to 3,425 degree days.
HDD_YEAR <- 2025

#' Adds `hdd`, the heating degree days of the expert's NUTS3 region in
#' HDD_YEAR: NA for an expert without nuts3 (no valid postcode, or a P.O.-box or
#' large-customer postcode), and NA for a region without degree days. Whether
#' that may happen depends on the sample, so the caller checks it.
#'
#' @param main one row per expert, with nuts3
#' @return main with the column hdd, in the same rows
add_heating_degree_days <- function(main) {
  hdd <- read.csv(HDD_FILE) %>%
    filter(indic_nrg == "HDD", TIME_PERIOD == HDD_YEAR) %>%
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
