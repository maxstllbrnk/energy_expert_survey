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
