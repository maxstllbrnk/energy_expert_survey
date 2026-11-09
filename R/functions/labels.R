################################################################################
# labels.R — every English label and colour of the tables and figures, in one
#            place. The data keep the German answers of the questionnaire; these
#            vectors translate them. The order of a vector is its order in every
#            table and figure.
# Inputs : none
# Outputs: none
# Checks : none
################################################################################
# Plain text: latex_text() turns a label into LaTeX. American English.

# 1. Experts -------------------------------------------------------------------
# The groups, the reference group of the models first. "All experts" is the
# version of an exhibit over all experts, also those without a group.
EXPERT_GROUPS <- c("Energy advisers", "HVAC installers", "Chimney sweeps")
ALL_EXPERTS <- "All experts"
# The name of a group in file names (group_slug())
GROUP_SLUGS <- c(
  "All experts" = "all",
  "Energy advisers" = "energy_advisers",
  "HVAC installers" = "hvac_installers",
  "Chimney sweeps" = "chimney_sweeps"
)
# The groups where a table has a column per arm x group
GROUP_LABELS_SHORT <- c(
  "Energy advisers" = "Advisers",
  "HVAC installers" = "HVAC",
  "Chimney sweeps" = "Sweeps"
)
# The registers the lists of the sampling frame were drawn from
FRAME_TYPE_LABELS <- c(
  "DENA" = "Energy advisers",
  "SHK" = "HVAC installers",
  "Schornsteinfeger" = "Chimney sweeps"
)
# The target group of a wave in the dispatch log
TARGET_GROUP_LABELS <- c(
  "Personen" = "Persons",
  "Firmen" = "Firms"
)
# The federal states in official key order, as in the data and on the map
STATE_LABELS <- c(
  "Schleswig-Holstein" = "Schleswig-Holstein",
  "Hamburg" = "Hamburg",
  "Niedersachsen" = "Lower Saxony",
  "Bremen" = "Bremen",
  "Nordrhein-Westfalen" = "North Rhine-Westphalia",
  "Hessen" = "Hesse",
  "Rheinland-Pfalz" = "Rhineland-Palatinate",
  "Baden-Württemberg" = "Baden-Württemberg",
  "Bayern" = "Bavaria",
  "Saarland" = "Saarland",
  "Berlin" = "Berlin",
  "Brandenburg" = "Brandenburg",
  "Mecklenburg-Vorpommern" = "Mecklenburg-Western Pomerania",
  "Sachsen" = "Saxony",
  "Sachsen-Anhalt" = "Saxony-Anhalt",
  "Thüringen" = "Thuringia"
)
# The variables of the outlier rule and the staircases, in the cleaning report
OUTLIER_LABELS <- c(
  dem_birthyear = "Year of birth",
  dem_training_year = "Year training was completed",
  firm_employees = "Employees",
  adv_households = "Households advised, last year",
  co2_est_low = "Carbon price, lower estimate (euro per tonne)",
  co2_est_high = "Carbon price, upper estimate (euro per tonne)"
)
STAIRCASE_LABELS <- c(
  tp = "Time preference (patience)",
  risk = "Risk preference (risk tolerance)"
)

# The two arms of the vignettes, set by the answer to Q6a
ARM_LABELS <- c(
  fw = "With the district-heating option",
  nofw = "Without the district-heating option"
)

REVENUE_LABELS <- c(
  "unter 100.000 €" = "Below €100,000",
  "100.000 € bis unter 250.000 €" = "€100,000 to €250,000",
  "250.000 € bis unter 500.000 €" = "€250,000 to €500,000",
  "500.000 € bis unter 1 Million €" = "€500,000 to €1 million",
  "1 Million € bis unter 3 Millionen €" = "€1 million to €3 million",
  "über 3 Millionen €" = "Above €3 million"
)
# Q6, recoded from its free text (firm_radius_recoded)
RADIUS_LABELS <- c(
  "bis 20 km" = "Up to 20 km",
  "bis 50 km" = "Up to 50 km",
  "bis 100 km" = "Up to 100 km",
  "bundesweit" = "Nationwide",
  "Bietet keine an" = "Offers none",
  "Sonstiges" = "Other"
)
DISTRICT_HEATING_LABELS <- c(
  "Ja, eine Fernwärmeversorgung ist vorhanden." = "Available",
  "Nein, eine Fernwärmeversorgung ist nicht vorhanden. Eine Versorgung ist jedoch in den nächsten Jahren geplant." = "Not available, planned",
  "Nein, eine Fernwärmeversorgung ist nicht vorhanden und diese wird in den nächsten Jahren auch nicht geplant." = "Not available, not planned",
  "Weiß nicht" = "Don't know"
)
# Q22, recoded from its free text (dem_education_recoded)
EDUCATION_LABELS <- c(
  "Abgeschlossene Lehre oder vergleichbarer Abschluss an einer Berufsschule" = "Apprenticeship",
  "Meister, Techniker oder vergleichbarer Abschluss" = "Master craftsman or technician",
  "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)" = "University degree",
  "Kein Abschluss" = "No degree",
  "Sonstiges" = "Other"
)
# Q2, several ticks possible
FIRM_CATEGORY_LABELS <- c(
  firm_cat_energyadvice = "Energy advice office",
  firm_cat_engineering = "Architecture or engineering office",
  firm_cat_hvac = "Sanitary, heating, air conditioning",
  firm_cat_chimney = "Chimney sweep"
)
# Q8, several ticks possible
SERVICE_LABELS <- c(
  svc_advice_resid = "Energy advice, residential buildings",
  svc_advice_nonresid = "Energy advice, non-residential buildings",
  svc_expert_report = "Expert reports, e.g. for subsidies",
  svc_energy_cert = "Energy performance certificates",
  svc_funding_advice = "Advice on subsidy programs",
  svc_other = "Other"
)
# Q24, one yes or no per topic
TRAINING_LABELS <- c(
  cpd_hp = "Heat pumps",
  cpd_hp_electrician = "Electrician for heat pumps",
  cpd_renovplan = "Renovation roadmap",
  cpd_energyadvice = "Energy advice",
  cpd_begfunding = "BEG subsidy programs",
  cpd_other = "Other"
)
# Q7, ranks of the customer groups
CUSTOMER_LABELS <- c(
  "Privathaushalte" = "Private households",
  "Immobilienwirtschaft" = "Real estate industry",
  "Öffentliche Hand" = "Public sector",
  "Industrie" = "Industry",
  "Sonstiges Gewerbe (GHD)" = "Other commerce and services"
)

# 2. Technologies, fuels and prices --------------------------------------------
# The recommendations, as in the data
TECHNOLOGY_LABELS <- c(
  "Wärmepumpe" = "Heat pump",
  "Pelletheizung" = "Pellet heating",
  "Hybridheizung" = "Hybrid heating",
  "Gasheizung" = "Gas heating",
  "Ölheizung" = "Oil heating",
  "Fernwärme" = "District heating",
  "Keine Empfehlung" = "No recommendation"
)
# The technologies of the heating at home (Q20)
OWN_HEATING_LABELS <- c(
  "Wärmepumpe" = "Heat pump",
  "Pelletheizung" = "Pellet heating",
  "Fernwärme" = "District heating",
  "Gasheizung" = "Gas heating",
  "Ölheizung" = "Oil heating",
  "Hybridheizung" = "Hybrid heating",
  "Sonstiges" = "Other",
  "Keine Angabe" = "No answer"
)
# The shares of Q10b and Q11b (adv_share_<suffix>, mix25_<suffix>)
SHARE_TECHNOLOGY_LABELS <- c(
  hp = "Heat pump",
  pellet = "Pellet heating",
  dh = "District heating",
  gas = "Gas heating",
  oil = "Oil heating",
  hybrid = "Hybrid heating",
  other = "Other"
)
# The fuels of Q15 and Q17 (price_<fuel>, pexp_<fuel>)
FUEL_LABELS <- c(
  gas = "Gas",
  hpelec = "Heat-pump electricity",
  dh = "District heating",
  oil = "Heating oil",
  pellet = "Pellets"
)
EXPECTATION_LABELS <- c(
  "mehr als 10% niedriger" = "More than 10% lower",
  "bis zu 10 % niedriger" = "Up to 10% lower",
  "ungefähr gleich" = "About the same",
  "bis zu 10 % höher" = "Up to 10% higher",
  "mehr als 10% höher" = "More than 10% higher"
)
# The cost question of the last vignette
COST_LABELS <- c(
  "unter 10.000€" = "Below €10,000",
  "10.000€ - 19.999€" = "€10,000 to €19,999",
  "20.000€ - 29.999€" = "€20,000 to €29,999",
  "30.000€ - 39.999€" = "€30,000 to €39,999",
  "40.000€ - 49.999€" = "€40,000 to €49,999",
  "über 50.000€" = "€50,000 and above"
)

# 3. Barriers and criteria -----------------------------------------------------
# The options of Q19a to Q19c (barr_<question>_<option>), the same in all three
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
BARRIER_QUESTIONS <- tribble(
  ~question, ~prefix,      ~file,               ~caption,
  "Q19a",    "barr_ren_",  "renewable",         "Barriers to the demand for renewable heating",
  "Q19b",    "barr_dh_",   "district_heating",  "Barriers to the demand for district heating",
  "Q19c",    "barr_conv_", "conventional",      "Barriers to the demand for conventional heating"
)
# The heads of the columns of the barriers table: the captions of BARRIER_QUESTIONS
# are too long for them
BARRIER_HEADERS <- c(
  Q19a = "Renewable heating",
  Q19b = "District heating",
  Q19c = "Conventional heating"
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

# 4. The vignette attributes ---------------------------------------------------
# The levels exactly as in the data. att_heat_demand is the same split as
# att_renovation; att_image only illustrates the others.
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

# 5. Colours -------------------------------------------------------------------
# The original palette of the project (commit ced89b0)
COLOUR_BAR <- "#2a78d6"         # every single-colour bar and histogram
COLOUR_BAR_LIGHT <- "#b7d3f6"   # the de-emphasised bars next to it
COLOUR_INK <- "#52514e"         # text and lines in a figure
COLOUR_GRID <- "#e1e0d9"
COLOUR_NOT_SHOWN <- "#f2f1ee"   # a region without enough answers on a map

TECHNOLOGY_COLOURS <- c(
  "Heat pump" = "#2a78d6",
  "District heating" = "#eb6834",
  "Pellet heating" = "#1baf7a",
  "Hybrid heating" = "#eda100",
  "Gas heating" = "#e87ba4",
  "Oil heating" = "#008300",
  "No recommendation" = "#898781",
  "Other" = "#c3c2b7",
  "No answer" = "#e1e0d9"
)
GROUP_COLOURS <- c(
  "Energy advisers" = "#2a78d6",
  "HVAC installers" = "#eb6834",
  "Chimney sweeps" = "#1baf7a",
  "All experts" = "#52514e"
)
# With four lines a figure must also work in greyscale
GROUP_LINETYPES <- c(
  "All experts" = "solid",
  "Energy advisers" = "longdash",
  "HVAC installers" = "dotdash",
  "Chimney sweeps" = "dotted"
)
ARM_COLOURS <- c(
  "Without the district-heating option" = "#2a78d6",
  "With the district-heating option" = "#eb6834"
)
EXPECTATION_COLOURS <- c(
  "More than 10% lower" = "#256abf",
  "Up to 10% lower" = "#86b6ef",
  "About the same" = "#c3c2b7",
  "Up to 10% higher" = "#f0a09f",
  "More than 10% higher" = "#e34948"
)
COST_COLOURS <- c(
  "Below €10,000" = "#86b6ef",
  "€10,000 to €19,999" = "#5598e7",
  "€20,000 to €29,999" = "#2a78d6",
  "€30,000 to €39,999" = "#1c5cab",
  "€40,000 to €49,999" = "#104281",
  "€50,000 and above" = "#0d366b"
)
RANK_COLOURS <- c("Rank 1" = "#184f95", "Rank 2" = "#3987e5", "Rank 3" = "#86b6ef")
# Maps: a count or a rate from light to dark blue; the break-off rate, a different
# quantity, in a warm ramp
MAP_LOW <- "#cde2fb"
MAP_HIGH <- "#0d366b"
MAP_BREAKOFF_LOW <- "#fde3d5"
MAP_BREAKOFF_HIGH <- "#8c2d0a"
