# =============================================================================
# 00_settings.R  -  every choice the summary statistics depend on
# =============================================================================
# Change things here rather than in the scripts: samples and groups, the
# outlier rule, which questions are plotted, labels, colours and figure size.
# Sourced by R/run_summary_statistics.R.
# =============================================================================


# =============================================================================
# samples and groups
# =============================================================================
# Every figure is produced once for each sample x group, in its own folder.
#
#   alle_befragten   every expert in survey_main, including those who broke off
#   completer        only experts who submitted the questionnaire (is_complete)
SAMPLES <- tribble(
  ~folder,          ~completers_only, ~label,
  "alle_befragten", FALSE,            "Alle Befragten",
  "completer",      TRUE,             "Nur abgeschlossene Fragebögen"
)

# All experts first, then each Berufsgruppe. How an expert is assigned to a
# Berufsgruppe - including the priority when Q2 has several ticks - is set in
# add_berufsgruppe() in R/prepare_analysis_data.R.
ALL_GROUP <- "Alle Berufsgruppen"
GROUPS    <- c(ALL_GROUP, BERUFSGRUPPEN)


# =============================================================================
# outliers in continuous variables
# =============================================================================
# A handful of typos - a birth year of -1957, a gas price of 1,000 per kWh -
# stretch the axis of a histogram until the real distribution is a single bar.
# Before a continuous variable is plotted or summarised, two rules are applied,
# in this order:
#
#   1. VALID_RANGE     impossible values are dropped. Only for variables with a
#                      hard logical bound; the bounds are those the cleaning
#                      pipeline flags in the data_quality sheet of
#                      build_report.xlsx.
#   2. TRIM_QUANTILES  of the values that are left, those below the lower and
#                      above the upper quantile are dropped. c(0.005, 0.995)
#                      cuts the lowest and the highest 0.5 %; c(0, 1) switches
#                      trimming off.
#
# Every histogram says in its caption how many values were dropped, and the
# mean and median it shows are those of the remaining values. tabellen.xlsx
# uses the same rule. The data themselves are never changed.
TRIM_QUANTILES <- c(0.005, 0.995)

VALID_RANGE <- list(
  dem_birthyear     = c(1920, 2010),
  dem_training_year = c(1930, 2026)
)

# Scores that are bounded by construction and are never trimmed.
NOT_TRIMMED <- c("tp_score", "risk_score")

# A figure is skipped when fewer experts than this answered, and a bar of a
# stacked chart is left out when it rests on fewer answers. This mostly matters
# for the Berufsgruppen, because some questions were only shown to some of them
# (Q11b only to SHK firms, Q8 and Q10 only to Energieberatung and
# Schornsteinfeger firms).
MIN_N <- 10


# =============================================================================
# which questions are plotted
# =============================================================================
# One row per figure. `section` is the sub-folder the figure is written to.
# Titles are the question text from the codebook, unless TITLES says otherwise.

# Continuous variables -> histogram. `binwidth` NA = 30 bins. `log_scale` for
# counts spread over several orders of magnitude; their binwidth is measured on
# the log scale (0.5 keeps 1, 2, 3, ... from falling into empty bins).
HISTOGRAMS <- tribble(
  ~variable,           ~section,                    ~x_label,                        ~binwidth, ~log_scale,
  "firm_employees",    "04_unternehmen",            "Beschäftigte (log. Skala)",     0.5,       TRUE,
  "price_gas",         "06_energiepreise",          "Angegebener Preis je kWh",      1,         FALSE,
  "price_hpelec",      "06_energiepreise",          "Angegebener Preis je kWh",      1,         FALSE,
  "price_dh",          "06_energiepreise",          "Angegebener Preis je kWh",      1,         FALSE,
  "price_oil",         "06_energiepreise",          "Angegebener Preis je kWh",      1,         FALSE,
  "price_pellet",      "06_energiepreise",          "Angegebener Preis je kWh",      1,         FALSE,
  "co2_est_low",       "06_energiepreise",          "Euro je Tonne CO₂",             5,         FALSE,
  "co2_est_high",      "06_energiepreise",          "Euro je Tonne CO₂",             5,         FALSE,
  "mix25_hp",          "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "mix25_pellet",      "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "mix25_dh",          "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "mix25_gas",         "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "mix25_hybrid",      "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "mix25_oil",         "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "mix25_other",       "07_heiztechnologien",       "Anteil in %",                   5,         FALSE,
  "adv_households",    "09_dienstleistungen_markt", "Beratene Haushalte (log. Skala)", NA,      TRUE,
  "adv_share_hp",      "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "adv_share_pellet",  "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "adv_share_dh",      "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "adv_share_gas",     "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "adv_share_oil",     "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "adv_share_hybrid",  "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "adv_share_other",   "09_dienstleistungen_markt", "Anteil in %",                   5,         FALSE,
  "dem_birthyear",     "10_person",                 "Geburtsjahr",                   NA,        FALSE,
  "dem_training_year", "10_person",                 "Jahr",                          NA,        FALSE
)

# Categorical variables -> bar chart. `height` in cm, for many categories.
BARS <- tribble(
  ~variable,               ~section,           ~height,
  "resp_is_owner",         "04_unternehmen",   7,
  "firm_revenue",          "04_unternehmen",   10,
  "firm_radius_recoded",   "04_unternehmen",   10,
  "firm_dh_in_area",       "04_unternehmen",   9,
  "bundesland",            "04_unternehmen",   15,
  "co2_heard",             "06_energiepreise", 7,
  "dem_education_recoded", "10_person",        9,
  "dem_own_heating",       "10_person",        10
)

# "Tick all that apply" (and Q24, a yes/no per topic) -> share choosing each option.
BARRIERS <- c("none", "funds", "labor", "progcomplex", "quality", "skepticism",
              "regchange", "opcost", "capex", "infra", "other")

MULTIPLE_CHOICE <- tribble(
  ~file,                          ~section,                    ~variables,
  "Q2_unternehmenskategorie",     "04_unternehmen",            c("firm_cat_engineering", "firm_cat_energyadvice",
                                                                 "firm_cat_hvac", "firm_cat_chimney"),
  "Q8_leistungen",                "09_dienstleistungen_markt", c("svc_advice_resid", "svc_advice_nonresid",
                                                                 "svc_expert_report", "svc_energy_cert",
                                                                 "svc_funding_advice", "svc_other"),
  "Q19a_hemmnisse_erneuerbare",   "09_dienstleistungen_markt", str_c("barr_ren_", BARRIERS),
  "Q19b_hemmnisse_fernwaerme",    "09_dienstleistungen_markt", str_c("barr_dh_", BARRIERS),
  "Q19c_hemmnisse_konventionell", "09_dienstleistungen_markt", str_c("barr_conv_", BARRIERS),
  "Q24_fortbildungen",            "10_person",                 c("cpd_hp", "cpd_hp_electrician", "cpd_renovplan",
                                                                 "cpd_energyadvice", "cpd_begfunding", "cpd_other")
)

# Ranking questions -> two figures each: mentions per option, stacked by rank,
# and <file>_gewichte, the mean weight per option (see ranking_weights() in
# 01_plot_functions.R).
RANKINGS <- tribble(
  ~file,                      ~section,                    ~variables,
  "Q7_kundengruppen",         "09_dienstleistungen_markt", c("cust_rank1", "cust_rank2"),
  "Q28_kriterien_der_kunden", "09_dienstleistungen_markt", c("crit_rank1", "crit_rank2", "crit_rank3")
)

# The vignette attributes the recommendations are broken down by. att_image is
# left out (it only illustrates the other attributes), and att_heat_demand too:
# it is the same split as att_renovation, whose labels include the demand.
VIGNETTE_ATTRIBUTES <- tribble(
  ~variable,                ~file,                ~title,
  "att_couple_age",         "alter",              "Alter des Ehepaars",
  "att_income",             "einkommen",          "Haushaltseinkommen",
  "att_current_heating",    "bestehende_heizung", "Bestehende Heizung",
  "att_replacement_timing", "ersatzzeitpunkt",    "Ersatzzeitpunkt der bestehenden Heizung",
  "att_build_year",         "baujahr",            "Baujahr des Gebäudes",
  "att_renovation",         "sanierungsstand",    "Sanierungsstand und Wärmebedarf",
  "att_heat_distribution",  "waermeverteilung",   "Wärmeverteilung"
)


# =============================================================================
# labels
# =============================================================================
# Titles for variables whose codebook text is not the question (derived
# variables) or is too long or technical to be a title.
TITLES <- c(
  berufsgruppe         = "Berufsgruppe der Befragten (abgeleitet aus Q2)",
  bundesland           = "Bundesland des Unternehmenssitzes (abgeleitet aus der Postleitzahl, Q0)",
  brand_top1           = "Von welchem Hersteller verkaufen Sie am meisten Heizungen Ihrer meistinstallierten Technologie?",
  tp_1                 = "Würden Sie lieber 100 Euro heute oder 154 Euro in 12 Monaten erhalten?",
  risk_1               = "Was würden Sie bevorzugen: eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro oder eine sichere Zahlung von 160 Euro?",
  tp_score             = "Zeitpräferenz (Geduld), Score von 0 bis 1",
  risk_score           = "Risikopräferenz (Risikobereitschaft), Score von 0 bis 1",
  vig_rec              = "Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?",
  vig_cost             = "Welche Investitionskosten erwarten Sie für Anschaffung und Einbau der Anlage ohne mögliche Förderung zu berücksichtigen?",
  bearbeitungszeit_min = "Bearbeitungszeit des Fragebogens in Minuten (abgeschlossene Fragebögen)"
)

# Subtitles where the answer-option text of the codebook does not read well.
SUBTITLES <- c(
  co2_est_low  = "Untere Grenze der geschätzten Spanne",
  co2_est_high = "Obere Grenze der geschätzten Spanne"
)

# Shorter answer labels for bar charts; the data are not changed.
SHORT_LABELS <- list(
  firm_dh_in_area = c(
    "Ja, eine Fernwärmeversorgung ist vorhanden." = "Ja, vorhanden",
    "Nein, eine Fernwärmeversorgung ist nicht vorhanden. Eine Versorgung ist jedoch in den nächsten Jahren geplant." = "Nein, aber in den nächsten Jahren geplant",
    "Nein, eine Fernwärmeversorgung ist nicht vorhanden und diese wird in den nächsten Jahren auch nicht geplant." = "Nein, auch nicht geplant"
  ),
  dem_education_recoded = c(
    "Abgeschlossene Lehre oder vergleichbarer Abschluss an einer Berufsschule" = "Abgeschlossene Lehre o. Ä.",
    "Meister, Techniker oder vergleichbarer Abschluss" = "Meister, Techniker o. Ä.",
    "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)" = "Hochschulabschluss"
  ),
  risk_1 = c(
    "Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro" = "Verlosung (50 % Chance auf 300 Euro)"
  )
)

# The two Fernwärme arms of the vignette experiment (vig_arm).
ARM_LABELS <- c(nofw = "Ohne Fernwärme-Option", fw = "Mit Fernwärme-Option")


# =============================================================================
# colours and figure size
# =============================================================================
# Taken from a validated reference palette. Categorical colours are used in its
# documented slot order, which keeps neighbouring segments of a stacked bar
# distinguishable for colour-blind readers; do not reorder them.
INK_PRIMARY   <- "#0b0b0b"
INK_SECONDARY <- "#52514e"
INK_MUTED     <- "#898781"
GRID_COLOR    <- "#e1e0d9"
COL_BAR       <- "#2a78d6"   # every single-colour bar and histogram
COL_BAR_LIGHT <- "#b7d3f6"   # the de-emphasised bars next to it

# Heating technologies, in the order they are stacked.
TECH_COLORS <- c(
  "Wärmepumpe"       = "#2a78d6",
  "Fernwärme"        = "#eb6834",
  "Pelletheizung"    = "#1baf7a",
  "Hybridheizung"    = "#eda100",
  "Gasheizung"       = "#e87ba4",
  "Ölheizung"        = "#008300",
  "Keine Empfehlung" = "#898781"
)

ARM_COLORS <- set_names(c("#2a78d6", "#eb6834"), ARM_LABELS)

# Ordered answers: one hue from light to dark, or two hues around a grey middle.
COST_COLORS <- c(
  "unter 10.000€"     = "#86b6ef",
  "10.000€ - 19.999€" = "#5598e7",
  "20.000€ - 29.999€" = "#2a78d6",
  "30.000€ - 39.999€" = "#1c5cab",
  "40.000€ - 49.999€" = "#104281",
  "über 50.000€"      = "#0d366b"
)

EXPECTATION_COLORS <- c(
  "mehr als 10% niedriger" = "#256abf",
  "bis zu 10 % niedriger"  = "#86b6ef",
  "ungefähr gleich"        = "#c3c2b7",
  "bis zu 10 % höher"      = "#f0a09f",
  "mehr als 10% höher"     = "#e34948"
)

RANK_COLORS <- c("Rang 1" = "#184f95", "Rang 2" = "#3987e5", "Rang 3" = "#86b6ef")

MAP_LOW  <- "#cde2fb"   # choropleth: fewest experts
MAP_HIGH <- "#0d366b"   # choropleth: most experts
MAP_LAND <- "#f2f1ee"   # background of the postcode map

FIG_WIDTH  <- 16    # cm
FIG_HEIGHT <- 10    # cm, unless a figure asks for more
FIG_DPI    <- 300
BASE_SIZE  <- 10    # pt


# =============================================================================
# report
# =============================================================================
# 08_report.R puts all figures of a sample on one page (bericht.html) and
# prints it to bericht.pdf. Headings of the sections, by sub-folder:
SECTION_LABELS <- c(
  "01_stichprobe"             = "Stichprobe und Abbruch",
  "02_bearbeitungszeit"       = "Bearbeitungszeit",
  "03_karten"                 = "Regionale Verteilung",
  "04_unternehmen"            = "Unternehmen",
  "05_vignetten"              = "Vignetten",
  "06_energiepreise"          = "Energiepreise und CO₂-Preis",
  "07_heiztechnologien"       = "Heiztechnologien und Hersteller",
  "08_praeferenzen"           = "Zeit- und Risikopräferenzen",
  "09_dienstleistungen_markt" = "Dienstleistungen, Markt und Kunden",
  "10_person"                 = "Persönliche Angaben"
)

# The PDF is printed by the first of these browsers that is installed. Add the
# path of yours if it is somewhere else; with none, only bericht.html is written.
REPORT_BROWSERS <- c(
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
)
