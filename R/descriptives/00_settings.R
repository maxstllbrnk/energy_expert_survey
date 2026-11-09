################################################################################
# 00_settings.R — every choice the English descriptives depend on
# Inputs : none; sourced by R/run_descriptives.R
# Outputs: none
# Checks : the labels of Q17 are the answers of PEXP_ANSWERS, in their order
################################################################################
# The sample, the files the data are checked against, the rows of the tables,
# the English labels, the colours and the sizes. Rules that the descriptives
# share with other analyses are not repeated here:
#   the Berufsgruppe, the price rules,      R/prepare_analysis_data.R
#   PEXP_RISE and HDD_YEAR
#   the outlier rule (VALID_RANGE,          R/summary_statistics/00_settings.R
#   TRIM_QUANTILES) and MIN_N
#   MIN_KONTAKTE                            R/ruecklauf/00_settings.R
# Labels are plain text; latex_text() turns them into LaTeX for the tables.
# American English, as the slides.

# 1. Sample and inputs ---------------------------------------------------------

# Completed questionnaires of experts in one of the three Berufsgruppen, in
# both arms (Max, 2026-10-28): the experts of the mixed logit models, and
# those over whom R/analysis/01_run_specifications.R takes the reference
# prices. Only the response rates (03_recruitment.R) count everyone invited
# and everyone who answered.

# The year of the survey: age is SURVEY_YEAR minus the year of birth
SURVEY_YEAR <- 2026

# The saved results of the mixed logit models in the deck: their price rules,
# reference prices and estimation sample must be those of the descriptives
# (02_data.R). One run of R/analysis/01_run_specifications.R per arm (its
# folder in RC_LOGIT_DIR), and the models as in MODELS of
# R/analysis/02_output.R. A model without saved results gives a warning.
MODEL_RUNS <- tribble(
  ~arm, ~folder,
  "fw", "rc_logit_fw_ref_waermepumpe"
)
# (6) was dropped and (6pe) added (Max, 2026-10-29)
MODELS_IN_DECK <- c("m2", "m4", "m4i", "m6i", "m4p", "m6p", "m4ip",
                    "m4pa", "m4pd", "m4pl", "m4pe", "m6pe")

# The tests of the price expectations, written by
# R/first_policy_brief/02_kl_divergence_prices.R, and its names of the fuels
PRICE_TESTS_FILE <- file.path(FIRST_POLICY_BRIEF_DIR,
                              "preiserwartungen_tests.xlsx")
PRICE_TESTS_FUELS <- c(
  "Gas" = "gas",
  "Wärmepumpenstrom" = "hpelec",
  "Fernwärme" = "dh",
  "Heizöl" = "oil",
  "Pellets" = "pellet"
)

# Continuous answers that go through the outlier rule of the summary
# statistics (VALID_RANGE, then TRIM_QUANTILES), once on the pooled sample,
# so that an answer is kept or removed in every column alike (Max,
# 2026-10-28). The prices follow the rules of the models instead. The degree
# days are regional data, not answers, and the response times are only shown
# as medians and quartiles: neither is trimmed. The shares of Q10b and Q11b
# lie between 0 and 100 and are not trimmed either (SHARE_QUESTIONS).
OUTLIER_VARIABLES <- c(
  dem_birthyear = "Year of birth",
  dem_training_year = "Year training was completed",
  firm_employees = "Employees",
  adv_households = "Households advised, last year"
)

# The shares of the technologies among the recommendations of the last year
# (Q10b, asked of firms offering energy advice or chimney sweeping) and among
# the installations of 2025 (Q11b, HVAC firms), in the order of the tables.
# An expert gave shares in a question if they sum to more than 0; a
# technology left blank then counts as 0 (Max, 2026-10-29). They are used as
# reported, also where they do not sum to 100 (the rule of the build report),
# and those that do not are counted.
SHARE_TECHNOLOGIES <- c(
  hp = "Heat pump",
  pellet = "Pellet heating",
  dh = "District heating",
  gas = "Gas heating",
  oil = "Oil heating",
  hybrid = "Hybrid heating",
  other = "Other"
)
SHARE_QUESTIONS <- list(
  "Q10b (recommendations)" = paste0("adv_share_", names(SHARE_TECHNOLOGIES)),
  "Q11b (installations)" = paste0("mix25_", names(SHARE_TECHNOLOGIES))
)

# Q28 asks for the three criteria that matter most to the customers, as in
# RANKINGS of the summary statistics
CRITERIA_RANKS <- c("crit_rank1", "crit_rank2", "crit_rank3")

# 2. Labels --------------------------------------------------------------------

# The Berufsgruppen, with the names and in the order of R/analysis/02_output.R
# (the reference group of the models first)
GROUP_LABELS <- c(
  "Energieberater" = "Energy advisers",
  "SHK-Handwerk" = "HVAC installers",
  "Schornsteinfeger" = "Chimney sweeps"
)
ALL_EXPERTS <- "All experts"
# The columns of a table by group
GROUP_COLUMNS <- c(unname(GROUP_LABELS), ALL_EXPERTS)
# The same inside a sentence or a caption
GROUP_LABELS_IN_TEXT <- c(
  "Energy advisers" = "energy advisers",
  "HVAC installers" = "HVAC installers",
  "Chimney sweeps" = "chimney sweeps"
)
# Column headers of the tables, over two lines (header_cell()), and in one
# word where a table has a column per group and arm
GROUP_HEADERS <- c(
  "Energy advisers" = "Energy\\\\advisers",
  "HVAC installers" = "HVAC\\\\installers",
  "Chimney sweeps" = "Chimney\\\\sweeps",
  "All experts" = "All\\\\experts"
)
GROUP_HEADERS_SHORT <- c(
  "Energy advisers" = "Advisers",
  "HVAC installers" = "HVAC",
  "Chimney sweeps" = "Sweeps",
  "All experts" = "All"
)

# The two arms of the vignettes (vig_arm), set by Q6a
ARMS <- c(
  fw = "With the district-heating option",
  nofw = "Without the district-heating option"
)

# The recommendations, with the names and in the order of
# R/analysis/02_output.R
TECHNOLOGY_LABELS <- c(
  "Wärmepumpe" = "Heat pump",
  "Pelletheizung" = "Pellet heating",
  "Hybridheizung" = "Hybrid heating",
  "Gasheizung" = "Gas heating",
  "Ölheizung" = "Oil heating",
  "Fernwärme" = "District heating",
  "Keine Empfehlung" = "No recommendation"
)

# The fuels of Q15 and Q17, in the order of PRICE_FUELS, and as LaTeX column
# headers over two lines (header_cell())
FUEL_LABELS <- c(
  gas = "Gas",
  hpelec = "Heat-pump electricity",
  dh = "District heating",
  oil = "Heating oil",
  pellet = "Pellets"
)
FUEL_HEADERS <- c(
  gas = "Gas",
  hpelec = "Heat-pump\\\\electricity",
  dh = "District\\\\heating",
  oil = "Heating\\\\oil",
  pellet = "Pellets"
)

# Q17, in the order of PEXP_ANSWERS
EXPECTATION_LABELS <- c(
  "mehr als 10% niedriger" = "More than 10% lower",
  "bis zu 10 % niedriger" = "Up to 10% lower",
  "ungefähr gleich" = "About the same",
  "bis zu 10 % höher" = "Up to 10% higher",
  "mehr als 10% höher" = "More than 10% higher"
)
stopifnot(identical(names(EXPECTATION_LABELS), PEXP_ANSWERS))

# The options of Q19a-c (barr_<question>_<option>), the same in all three
BARRIER_LABELS <- c(
  none = "I see no barriers",
  funds = "Customers lack the funds",
  labor = "Shortage of skilled workers in own firm",
  progcomplex = "Subsidy programs unknown or too complex",
  quality = "Insufficient product quality",
  skepticism = "Customers skeptical of the technology",
  regchange = "Frequent changes to the legal framework",
  opcost = "Uncertain operating costs",
  capex = "High investment costs",
  infra = "Missing infrastructure (e.g. district heating, gas grid)",
  other = "Other"
)
# The three questions: Q19a in the main part, Q19b and Q19c in the appendix.
# `note` names the macro of the note below the figure (numbers_desc.tex);
# `demand_for` completes the question in the notes.
BARRIER_QUESTIONS <- tribble(
  ~question, ~prefix,      ~file,                       ~note,                             ~caption,                                          ~demand_for,
  "Q19a",    "barr_ren_",  "barriers_renewable",        "descNoteBarriersRenewable",       "Barriers to the demand for renewable heating",    "renewable heating, including heat pumps,",
  "Q19b",    "barr_dh_",   "barriers_district_heating", "descNoteBarriersDistrictHeating", "Barriers to the demand for district heating",     "district heating",
  "Q19c",    "barr_conv_", "barriers_conventional",     "descNoteBarriersConventional",    "Barriers to the demand for conventional heating", "conventional heating"
)

# Q28
CRITERION_LABELS <- c(
  "Ausreichende Heizleistung bei sehr kalten Temperaturen" = "Enough heat in very cold weather",
  "Langfristige Gesamtkosten für den Haushalt" = "Long-run total cost for the household",
  "Höhe der Investitionskosten" = "Investment cost",
  "Technische Umsetzbarkeit im Gebäude" = "Technical feasibility in the building",
  "Fördermöglichkeiten" = "Available subsidies",
  "Zukunftssicherheit" = "Future-proof",
  "Langlebigkeit der Anlage" = "Durability of the system",
  "Zeitnahe Umsetzbarkeit" = "Quick installation"
)

# The sections of the questionnaire (QUESTIONNAIRE in
# R/prepare_analysis_data.R)
SECTION_LABELS_ENGLISH <- c(
  "Unternehmen" = "Firm",
  "Vignetten" = "Vignettes",
  "Energiepreise und CO₂-Preis" = "Energy and carbon prices",
  "Heiztechnologien und Hersteller" = "Heating technologies and manufacturers",
  "Zeitpräferenzen" = "Time preferences",
  "Risikopräferenzen" = "Risk preferences",
  "Dienstleistungen, Markt und Kunden" = "Services, market and customers",
  "Persönliche Angaben" = "Personal information",
  "Feedback" = "Feedback"
)

# The vignette attributes and their levels, exactly as in the data. The
# reference levels are REFERENCE_LEVELS of R/analysis/01_run_specifications.R,
# the reference vignette of the models. The heat demand is the same split as
# the renovation status (att_renovation), whose words for each demand are in
# RENOVATION_OF_DEMAND (checked in 06_vignettes.R).
DESIGN_LEVELS <- tribble(
  ~attribute,               ~attribute_label,           ~level,                                           ~level_label,                     ~is_reference,
  "att_couple_age",         "Age of the couple",        "Mitte 40",                                       "Mid-40s",                        TRUE,
  "att_couple_age",         "Age of the couple",        "Mitte 60",                                       "Mid-60s",                        FALSE,
  "att_income",             "Household income",         "65.000 € brutto im Jahr",                        "€65,000 gross a year",           TRUE,
  "att_income",             "Household income",         "unter 40.000 € brutto im Jahr",                  "Below €40,000 gross a year",     FALSE,
  "att_current_heating",    "Current heating",          "Gasheizung",                                     "Gas",                            TRUE,
  "att_current_heating",    "Current heating",          "Ölheizung",                                      "Oil",                            FALSE,
  "att_replacement_timing", "Replacement",              "muss in den nächsten Jahren ersetzt werden",     "Due in the next few years",      TRUE,
  "att_replacement_timing", "Replacement",              "ist defekt und muss kurzfristig ersetzt werden", "Broken, due at short notice",    FALSE,
  "att_build_year",         "Year of construction",     "1970",                                           "1970",                           TRUE,
  "att_build_year",         "Year of construction",     "2000",                                           "2000",                           FALSE,
  "att_heat_demand",        "Heat demand (renovation)", "120",                                            "120 kWh/m² (partly renovated)",  TRUE,
  "att_heat_demand",        "Heat demand (renovation)", "70",                                             "70 kWh/m² (fully renovated)",    FALSE,
  "att_heat_demand",        "Heat demand (renovation)", "200",                                            "200 kWh/m² (not renovated)",     FALSE,
  "att_heat_distribution",  "Heat distribution",        "Heizkörper",                                     "Radiators",                      TRUE,
  "att_heat_distribution",  "Heat distribution",        "eine Fußbodenheizung",                           "Underfloor heating",             FALSE
)
RENOVATION_OF_DEMAND <- c(
  "120" = "teilsaniert",
  "70" = "vollständig saniert",
  "200" = "nicht saniert"
)

# 3. Rows of the summary tables ------------------------------------------------
# One row per line of a table. kind:
#   mean, year  N, mean, SD and quartiles of the variable; "year" without a
#               thousands separator
#   share       N and the share of experts whose answer is `level`, in percent
#   level       the same, indented below a heading: one answer of a question
#   heading     a line with the label only
# The variables ending in _clean have gone through the outlier rule, those
# ending in _kwh through the price rules (02_data.R).

# The experts and their firms (tab_desc_experts), in two panels
EXPERT_ROWS <- tribble(
  ~panel,       ~variable,                 ~kind,     ~level,                                                                             ~label,                            ~digits,
  "The expert", "age",                     "mean",    NA,                                                                                 "Age (years)",                     1,
  "The expert", "dem_training_year_clean", "year",    NA,                                                                                 "Year training was completed",     0,
  "The expert", "resp_is_owner",           "share",   "Ja",                                                                               "Owner or managing director (%)",  1,
  "The expert", NA,                        "heading", NA,                                                                                 "Highest degree (%)",              NA,
  "The expert", "dem_education_recoded",   "level",   "Abgeschlossene Lehre oder vergleichbarer Abschluss an einer Berufsschule",         "Apprenticeship",                  1,
  "The expert", "dem_education_recoded",   "level",   "Meister, Techniker oder vergleichbarer Abschluss",                                 "Master craftsman or technician",  1,
  "The expert", "dem_education_recoded",   "level",   "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)", "University degree",               1,
  "The expert", "dem_education_recoded",   "level",   "Kein Abschluss",                                                                   "No degree",                       1,
  "The expert", "dem_education_recoded",   "level",   "Sonstiges",                                                                        "Other",                           1,
  "The firm",   "firm_employees_clean",    "mean",    NA,                                                                                 "Employees",                       1,
  "The firm",   NA,                        "heading", NA,                                                                                 "Revenue last year (%)",           NA,
  "The firm",   "firm_revenue",            "level",   "unter 100.000 €",                                                                  "Below €100,000",                  1,
  "The firm",   "firm_revenue",            "level",   "100.000 € bis unter 250.000 €",                                                    "€100,000 to €250,000",            1,
  "The firm",   "firm_revenue",            "level",   "250.000 € bis unter 500.000 €",                                                    "€250,000 to €500,000",            1,
  "The firm",   "firm_revenue",            "level",   "500.000 € bis unter 1 Million €",                                                  "€500,000 to €1 million",          1,
  "The firm",   "firm_revenue",            "level",   "1 Million € bis unter 3 Millionen €",                                              "€1 million to €3 million",        1,
  "The firm",   "firm_revenue",            "level",   "über 3 Millionen €",                                                               "Above €3 million",                1
)

# Advice and installations (tab_desc_advice): the households advised and the
# share of the heat pumps among the recommendations and the installations of
# each firm; the mean is over the firms, not the share of all
# recommendations. The questionnaire does not ask how many heating systems a
# firm installed.
ADVICE_ROWS <- tribble(
  ~panel,                       ~variable,              ~kind,  ~level, ~label,                                            ~digits,
  "Advice, last year (Q10)",    "adv_households_clean", "mean", NA,     "Households advised on a new heating",             1,
  "Advice, last year (Q10)",    "adv_share_hp_filled",  "mean", NA,     "Heat pumps among the firm's recommendations (%)", 1,
  "Installations, 2025 (Q11b)", "mix25_hp_filled",      "mean", NA,     "Heat pumps among the firm's installations (%)",   1
)

# The recommended and installed technologies (tab_desc_technologies): the
# share of each technology among the recommendations and the installations
# of each firm
TECHNOLOGY_ROWS <- bind_rows(
  tibble(panel = "Share among the firm's recommendations, last year (Q10b, %)",
         variable = paste0("adv_share_", names(SHARE_TECHNOLOGIES), "_filled"),
         kind = "mean", level = NA_character_,
         label = unname(SHARE_TECHNOLOGIES), digits = 1),
  tibble(panel = "Share among the firm's installations, 2025 (Q11b, %)",
         variable = paste0("mix25_", names(SHARE_TECHNOLOGIES), "_filled"),
         kind = "mean", level = NA_character_,
         label = unname(SHARE_TECHNOLOGIES), digits = 1)
)

# The energy prices, the climate and the heating, as they enter the models
# (tab_desc_prices_heating): the prices and the degree days in the upper
# part, HEATING_ROWS side by side below them.
# The prices: one row per fuel. The price per kWh of the energy carrier, as
# Q15 asked: of gas, electricity, heat (district heating), heating oil and
# pellets. The expected rises are in tab_desc_price_expectations.
PRICE_PANEL <- paste0("Price paid per kWh of the energy carrier, January and ",
                      "February 2026 (cent)")
PRICE_ROWS <- tibble()
for (fuel in PRICE_FUELS) {
  PRICE_ROWS <- bind_rows(PRICE_ROWS, tibble(
    panel = PRICE_PANEL, variable = paste0("price_", fuel, "_kwh"),
    kind = "mean", level = NA_character_, label = FUEL_LABELS[[fuel]],
    digits = 1
  ))
}
# The degree days without a panel title (panel ""): their label says what
# they are, and the table saves a line
PRICE_ROWS <- bind_rows(PRICE_ROWS, tibble(
  panel = "", variable = "hdd", kind = "mean", level = NA_character_,
  label = "Heating degree days of the firm's region, 2025", digits = 0
))

# District heating in the service area (Q6a) and the heating at home (Q20),
# as they enter the models: two questions with their answer options, side by
# side in the lower part of tab_desc_prices_heating
HEATING_ROWS <- tribble(
  ~panel,             ~variable,         ~kind,     ~level,                                                                                                          ~label,                                     ~digits,
  "District heating", NA,                "heading", NA,                                                                                                              "District heating in the service area (%)", NA,
  "District heating", "firm_dh_in_area", "level",   "Ja, eine Fernwärmeversorgung ist vorhanden.",                                                                   "Available",                                1,
  "District heating", "firm_dh_in_area", "level",   "Nein, eine Fernwärmeversorgung ist nicht vorhanden. Eine Versorgung ist jedoch in den nächsten Jahren geplant.", "Not available, planned",                   1,
  "District heating", "firm_dh_in_area", "level",   "Nein, eine Fernwärmeversorgung ist nicht vorhanden und diese wird in den nächsten Jahren auch nicht geplant.",  "Not available, not planned",               1,
  "District heating", "firm_dh_in_area", "level",   "Weiß nicht",                                                                                                    "Don't know",                               1,
  "Heating at home",  NA,                "heading", NA,                                                                                                              "Heating of the expert's home (%)",         NA,
  "Heating at home",  "dem_own_heating", "level",   "Wärmepumpe",                                                                                                    "Heat pump",                                1,
  "Heating at home",  "dem_own_heating", "level",   "Pelletheizung",                                                                                                 "Pellet heating",                           1,
  "Heating at home",  "dem_own_heating", "level",   "Fernwärme",                                                                                                     "District heating",                         1,
  "Heating at home",  "dem_own_heating", "level",   "Gasheizung",                                                                                                    "Gas heating",                              1,
  "Heating at home",  "dem_own_heating", "level",   "Ölheizung",                                                                                                     "Oil heating",                              1,
  "Heating at home",  "dem_own_heating", "level",   "Hybridheizung",                                                                                                 "Hybrid heating",                           1,
  "Heating at home",  "dem_own_heating", "level",   "Sonstiges",                                                                                                     "Other",                                    1
)

# Who is in each arm (tab_desc_arms): means and shares only. The rows of the
# prices are those of PRICE_ROWS.
ARM_ROWS <- bind_rows(
  tribble(
    ~panel,                   ~variable, ~kind,   ~level,            ~label,            ~digits,
    "Occupational group (%)", "group",   "share", "Energy advisers", "Energy advisers", 1,
    "Occupational group (%)", "group",   "share", "HVAC installers", "HVAC installers", 1,
    "Occupational group (%)", "group",   "share", "Chimney sweeps",  "Chimney sweeps",  1
  ),
  PRICE_ROWS %>%
    filter(panel == PRICE_PANEL),
  tribble(
    ~panel,            ~variable,         ~kind,   ~level,      ~label,                         ~digits,
    "Region and home", "hdd",             "mean",  NA,          "Heating degree days, 2025",    0,
    "Region and home", "dem_own_heating", "share", "Fernwärme", "District heating at home (%)", 1
  )
)

# 4. Colours and sizes ---------------------------------------------------------

# The ZEW chart colours (Max, 2026-10-28): series in this order
ZEW_LIME <- "#C8D202"    # zewchartlime, first series
ZEW_GRAY <- "#9C9E9F"    # zewchartgray, second series
ZEW_ICE <- "#ABDADC"     # zewchartice, third series
ZEW_SLATE <- "#35484F"   # zewchartslate, fourth series
ZEW_DARK <- "#4C4C4C"    # zewdark: text, and the outline of the symbols

# Where a figure tells the groups apart by colour (the response times), a
# group has this colour and symbol. Symbols with a fill (21 to 25), as in
# R/analysis/02_output.R, outlined in ZEW_DARK so that the light colours stand
# out on white. The bar charts with a panel per group need no group colours.
GROUP_COLOURS <- c(
  "Energy advisers" = ZEW_LIME,
  "HVAC installers" = ZEW_GRAY,
  "Chimney sweeps" = ZEW_ICE,
  "All experts" = ZEW_SLATE
)
GROUP_SHAPES <- c(
  "Energy advisers" = 21,
  "HVAC installers" = 24,
  "Chimney sweeps" = 22,
  "All experts" = 23
)

# Q17 from much lower to much higher: the strong answers in the dark and the
# bright colour, the weak ones in the light colours. The palette has no light
# lime, so "Up to 10% higher" is ZEW_LIME mixed half and half with white.
EXPECTATION_COLOURS <- c(
  "More than 10% lower" = ZEW_SLATE,
  "Up to 10% lower" = ZEW_ICE,
  "About the same" = ZEW_GRAY,
  "Up to 10% higher" = "#E4E981",
  "More than 10% higher" = ZEW_LIME
)

# Invited against completed (fig_desc_composition), both counted as persons
COMPOSITION_COLOURS <- c(
  "Completed (persons)" = ZEW_LIME,
  "Invited (persons)" = ZEW_GRAY
)

# The two arms, side by side in the figure of the recommendations
ARM_COLOURS <- c(ZEW_LIME, ZEW_GRAY)
names(ARM_COLOURS) <- unname(ARMS)

# The map of the response rate: from ZEW_ICE (lowest rate) to ZEW_SLATE
# (highest); a state that is not shown in light grey
RATE_MAP_LOW <- ZEW_ICE
RATE_MAP_HIGH <- ZEW_SLATE
RATE_MAP_NOT_SHOWN <- "#E6E6E6"
# The labels of the small states would cover their neighbours' labels: they
# are moved outside the map, by these kilometres to the east (dx) and the
# north (dy), with a line to the state. Brandenburg's label would cover the
# west of Berlin: it moves south-east, within Brandenburg, without a line.
# Checked on the rendered figure.
MAP_LABEL_NUDGES <- tribble(
  ~state,        ~dx_km, ~dy_km, ~with_line,
  "Berlin",      110,    70,     TRUE,
  "Hamburg",     -140,   60,     TRUE,
  "Bremen",      -150,   0,      TRUE,
  "Saarland",    -75,    -40,    TRUE,
  "Brandenburg", 80,     -60,    FALSE
)

# Sizes for the 16:9 slides of the economics-latex layout (text width 5.5
# in), as in R/analysis/02_output.R. The figures have no titles inside (the
# frame title carries the message) and leave room for a note of two lines
# below them (\slidenote{}), checked on a 16:9 frame.
FONT_SIZE <- 9
FIGURE_SIZES <- list(
  full = c(width = 5.5, height = 2.5),
  map = c(width = 3.3, height = 2.6)
)
# Text inside a figure (geom_text()), 1.5 points smaller than the axes, in
# ggplot's mm (.pt: points per mm)
TEXT_SIZE <- (FONT_SIZE - 1.5) / .pt
# The coloured squares of a legend, in pt: the default is twice the height of
# the text beside them
LEGEND_KEY_SIZE <- 9
# Tables: the font size, and the width of the column of the row labels in the
# summary tables, the two cleaning tables and the tables with a column per
# group. A fixed width makes the table, and so its notes, wider: the notes
# take fewer lines, and the table fits a 16:9 frame.
TABLE_FONT_SIZE <- "\\scriptsize"
LABEL_WIDTH_SUMMARY <- "8.0cm"
LABEL_WIDTH_CLEANING <- "5.8cm"
LABEL_WIDTH_GROUPS <- "6.4cm"
# The summary tables put the answer options of two questions side by side
# below the other rows, so that they fit a frame: the widths of the labels of
# the left and the right question. A heading wider than its label column
# wraps onto two lines.
OPTION_LABEL_WIDTHS <- c(left = "5.8cm", right = "5.0cm")
# The space between two panels of a table: a little less than booktabs'
# default, so that a summary table of three panels fits a frame
PANEL_SPACE <- "\\addlinespace[2pt]"
