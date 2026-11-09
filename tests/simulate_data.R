################################################################################
# simulate_data.R — synthetic data, so that code can be tested without the real
#                   data (project profile: Claude Code may not touch them)
# Inputs : design matrix and postcode lookups (PUBLIC_DATA_DIR, non-sensitive)
# Outputs: in SECURE_DIR of a synthetic run (config.R):
#            clean/survey_main.rds, clean/vignettes_long.rds   as written by
#              R/cleaning/05_main.R: the real column names and answer levels,
#              with invented values
#            frame/frame.rds    a sampling frame with invented e-mail addresses
#            truth.rds          the parameters behind the recommendations
# Checks : none; this is test code
################################################################################
# From the project root, in a session with CLAUDECODE=1:
#   Rscript tests/simulate_data.R
#   Rscript R/run_cleaning.R                 # writes experts.rds, vignettes.rds
# N_EXPERTS sets the size. Nothing here is real: values are drawn at random, with
# the structure of the survey: routing, break-offs, repeat attempts, one expert in
# both arms, answers in the wrong unit, typos in years, shares that do not sum to
# 100, invalid postcodes.

source(file.path("R", "setup.R"))
if (!USE_SYNTHETIC_DATA) {
  stop("tests/simulate_data.R would overwrite the cleaned data of a real run")
}
set.seed(20261109)
N_EXPERTS <- as.integer(Sys.getenv("EES_N_EXPERTS", unset = "1500"))
dir.create(file.path(SECURE_DIR, "frame"), recursive = TRUE, showWarnings = FALSE)

# 1. Helpers -------------------------------------------------------------------
draw_factor <- function(n, levels, probabilities = NULL) {
  factor(sample(levels, n, replace = TRUE, prob = probabilities), levels = levels)
}
blank_unless <- function(values, condition) {
  values[!condition] <- NA
  values
}
position_of <- function(item) {
  QUESTIONNAIRE$position[QUESTIONNAIRE$item == item]
}
TICK_LEVELS <- c("Ja", "Nicht Gewählt")

# 2. Persons: group, region, how far they got ----------------------------------
# The real waves of the dispatch log, a public file, so that the join of the response
# rates by wave runs as on the real data
WAVES <- load_versand() %>%
  select(wave, target_group, versandrunde, is_softlaunch)

plz_lookup <- read_csv(PLZ_LOOKUP_FILE, show_col_types = FALSE,
                       col_types = cols(plz = col_character(), bl_code = col_character()))
state_levels <- plz_lookup %>%
  distinct(bl_code, bundesland) %>%
  arrange(bl_code) %>%
  pull(bundesland)
plz_nuts3 <- read_delim(PLZ_NUTS3_FILE, delim = ";", show_col_types = FALSE,
                        col_types = cols(.default = col_character())) %>%
  transmute(plz = str_remove_all(CODE, "'"), nuts3 = str_remove_all(NUTS3, "'"))
# Only regions with degree days in HDD_YEAR: a gap would stop the cleaning, as it
# should on the real data
hdd_regions <- read.csv(HDD_FILE) %>%
  filter(indic_nrg == "HDD", TIME_PERIOD == HDD_YEAR) %>%
  pull(geo)
geographic_plz <- plz_nuts3 %>%
  filter(nuts3 %in% hdd_regions, plz %in% plz_lookup$plz)
po_box_plz <- setdiff(plz_lookup$plz, plz_nuts3$plz)

persons <- tibble(
  person_number = seq_len(N_EXPERTS),
  wave = sample(WAVES$wave, N_EXPERTS, replace = TRUE),
  meta_token = paste0("tok", person_number),
  true_group = sample(EXPERT_GROUPS, N_EXPERTS, replace = TRUE, prob = c(.68, .17, .15)),
  # a completed questionnaire, or where the expert stopped: many stop at Q6a, when
  # the vignettes begin, and at the cost question, when the prices begin
  is_complete = runif(N_EXPERTS) < 0.65
) %>%
  left_join(WAVES, by = "wave")
persons$meta_token[sample(N_EXPERTS, 15)] <- NA

last_position_incomplete <- sample(
  c(0, 4, 8, 15, sample(1:46, 200, replace = TRUE)), N_EXPERTS, replace = TRUE,
  prob = c(.05, .05, .2, .1, rep(.6 / 200, 200))
)
persons$last_position <- if_else(persons$is_complete, nrow(QUESTIONNAIRE),
                                 as.integer(last_position_incomplete))

# Experts with a further, unfinished attempt under the same token
repeat_persons <- persons %>%
  filter(!is.na(meta_token)) %>%
  slice_sample(prop = 0.06) %>%
  mutate(is_complete = FALSE,
         last_position = as.integer(sample(0:12, n(), replace = TRUE)))
attempts <- bind_rows(persons, repeat_persons) %>%
  mutate(meta_id = row_number(),
         resp_uid = paste0(wave, "_", meta_id)) %>%
  arrange(resp_uid)
n <- nrow(attempts)

# Whether the expert submitted a page, by its position in the questionnaire
reached <- function(item) {
  attempts$last_position >= position_of(item)
}

# 3. The firm ------------------------------------------------------------------
main <- attempts %>%
  select(resp_uid, wave, meta_id, target_group, versandrunde, is_softlaunch,
         meta_token, is_complete, last_position, true_group, person_number)

# Postcodes: most geographic, a few P.O.-box postcodes, a few fragments
postcode_kind <- sample(c("geographic", "po_box", "fragment"), n, replace = TRUE,
                        prob = c(.93, .05, .02))
geographic_rows <- sample(nrow(geographic_plz), n, replace = TRUE)
plz <- geographic_plz$plz[geographic_rows]
plz[postcode_kind == "po_box"] <- sample(po_box_plz, sum(postcode_kind == "po_box"), replace = TRUE)
main$firm_zip <- as.numeric(plz)
main$firm_zip[postcode_kind == "fragment"] <- sample(1:99, sum(postcode_kind == "fragment"), replace = TRUE)
main$firm_zip <- blank_unless(main$firm_zip, reached("time_firm_zip"))
main <- main %>%
  mutate(
    plz_valid = if_else(is.na(firm_zip), NA, firm_zip >= 1001),
    plz = if_else(coalesce(plz_valid, FALSE), sprintf("%05d", as.integer(firm_zip)), NA_character_),
    plz_in_nuts_list = if_else(is.na(plz), NA, plz %in% plz_nuts3$plz),
    nuts3 = plz_nuts3$nuts3[match(plz, plz_nuts3$plz)],
    bundesland = factor(plz_lookup$bundesland[match(plz, plz_lookup$plz)], levels = state_levels),
    bundesland_source = if_else(is.na(plz), "no_postcode", "plz")
  )

main$resp_is_owner <- blank_unless(draw_factor(n, c("Ja", "Nein"), c(.8, .2)), reached("time_resp_is_owner"))
main$resp_role <- blank_unless(rep("Mitarbeiter", n), reached("time_resp_role") & coalesce(main$resp_is_owner == "Nein", FALSE))

# Q2, several ticks possible. A few experts submit the page without any tick, a few
# fill in only "Sonstige".
main <- main %>%
  mutate(
    firm_cat_hvac = factor(if_else(true_group == "HVAC installers", "Ja", "Nicht Gewählt"), levels = TICK_LEVELS),
    firm_cat_chimney = factor(if_else(true_group == "Chimney sweeps", "Ja", "Nicht Gewählt"), levels = TICK_LEVELS),
    # energy advice is ticked by every adviser and by some of the trades too
    firm_cat_energyadvice = factor(if_else(true_group == "Energy advisers" | runif(n) < .15, "Ja", "Nicht Gewählt"), levels = TICK_LEVELS),
    firm_cat_engineering = factor(if_else(runif(n) < .12, "Ja", "Nicht Gewählt"), levels = TICK_LEVELS),
    firm_cat_other = NA_character_
  )
no_tick <- runif(n) < .01
only_other <- runif(n) < .02 & !no_tick & main$true_group == "Energy advisers"
for (column in c("firm_cat_hvac", "firm_cat_chimney", "firm_cat_energyadvice", "firm_cat_engineering")) {
  main[[column]][no_tick | only_other] <- "Nicht Gewählt"
}
main$firm_cat_other[only_other] <- "Sonstige Beratung"
is_at_q2 <- reached("time_firm_cat")
for (column in c("firm_cat_hvac", "firm_cat_chimney", "firm_cat_energyadvice", "firm_cat_engineering", "firm_cat_other")) {
  main[[column]] <- blank_unless(main[[column]], is_at_q2)
}
answered_shk <- coalesce(main$firm_cat_hvac == "Ja", FALSE)
answered_advice <- coalesce(main$firm_cat_energyadvice == "Ja" | main$firm_cat_chimney == "Ja" | main$firm_cat_engineering == "Ja", FALSE)

main$firm_employees <- blank_unless(round(exp(rnorm(n, 1.2, 1.1))), reached("time_firm_employees"))
main$firm_employees[sample(n, 3)] <- 250000
main$firm_revenue <- blank_unless(draw_factor(n, names(REVENUE_LABELS)), reached("time_firm_revenue"))
radius_levels <- c("bis 20 km", "bis 50 km", "bis 100 km", "bundesweit")
main$firm_radius <- blank_unless(draw_factor(n, radius_levels, c(.2, .4, .3, .1)), reached("time_firm_radius"))
main$firm_radius_other <- NA_character_
main$firm_radius_recoded <- factor(as.character(main$firm_radius), levels = names(RADIUS_LABELS))
main$firm_radius_recode_source <- if_else(is.na(main$firm_radius), "no_answer", "original")
main$firm_dh_in_area <- blank_unless(
  draw_factor(n, names(DISTRICT_HEATING_LABELS), c(.6, .15, .2, .05)),
  reached("time_firm_dh_in_area")
)

# 4. Energy prices and expectations --------------------------------------------
# True prices in cent per kWh. Answers: blank, in euro, per litre or per kg, or
# unusable, in the shares the real questionnaire makes plausible.
true_price <- tibble(
  gas = rnorm(n, 12, 2), hpelec = rnorm(n, 28, 4), dh = rnorm(n, 14, 3),
  oil = rnorm(n, 11, 2), pellet = rnorm(n, 8, 1.5)
)
for (fuel in names(FUEL_LABELS)) {
  price <- pmax(true_price[[fuel]], 1)
  kind <- sample(c("ok", "blank", "euro", "unit", "garbage"), n, replace = TRUE,
                 prob = c(.78, .15, .03, .03, .01))
  answer <- price
  answer[kind == "euro"] <- price[kind == "euro"] / 100
  if (fuel == "oil") answer[kind == "unit"] <- price[kind == "unit"] * 10
  if (fuel == "pellet") answer[kind == "unit"] <- price[kind == "unit"] * 5
  answer[kind == "garbage"] <- price[kind == "garbage"] * 20
  answer[kind == "blank"] <- NA
  main[[paste0("price_", fuel)]] <- blank_unless(round(answer, 3), reached("time_price"))
  main[[paste0("pexp_", fuel)]] <- blank_unless(
    draw_factor(n, names(EXPECTATION_LABELS), c(.03, .07, .2, .3, .4)),
    reached("time_pexp") & runif(n) > .05
  )
}
main$co2_heard <- blank_unless(draw_factor(n, c("Ja", "Nein"), c(.85, .15)), reached("time_co2_heard"))
is_at_co2_estimate <- reached("time_co2_est") & coalesce(main$co2_heard == "Ja", FALSE)
main$co2_est_low <- blank_unless(round(rnorm(n, 45, 15)), is_at_co2_estimate)
main$co2_est_high <- blank_unless(round(main$co2_est_low + abs(rnorm(n, 15, 10))), is_at_co2_estimate)

# 5. Installed mix, services, shares -------------------------------------------
draw_shares <- function(n_rows, n_columns, condition) {
  raw <- matrix(rexp(n_rows * n_columns), n_rows, n_columns)
  shares <- round(100 * raw / rowSums(raw))
  # the rounding error goes to the first column; some experts do not sum to 100
  shares[, 1] <- shares[, 1] + 100 - rowSums(shares)
  shares[, 1] <- pmax(shares[, 1], 0)
  off <- runif(n_rows) < .05
  shares[off, 2] <- pmin(shares[off, 2] + 10, 100)
  # a technology with no share is left blank
  shares[shares == 0] <- NA
  shares[!condition, ] <- NA
  shares
}
share_suffixes <- names(SHARE_TECHNOLOGY_LABELS)
mix_shares <- draw_shares(n, 7, reached("time_mix25") & answered_shk)
advice_shares <- draw_shares(n, 7, reached("time_adv_share") & answered_advice)
for (k in seq_along(share_suffixes)) {
  main[[paste0("mix25_", share_suffixes[k])]] <- mix_shares[, k]
  main[[paste0("adv_share_", share_suffixes[k])]] <- advice_shares[, k]
}
main$brand_top1 <- blank_unless(
  sample(c("Hersteller A", "Hersteller B", "Hersteller C", "Sonstige"), n, replace = TRUE),
  reached("time_brand_top1") & answered_shk
)
for (service in names(SERVICE_LABELS)) {
  main[[service]] <- blank_unless(draw_factor(n, TICK_LEVELS, c(.5, .5)), reached("time_svc") & answered_advice)
}
main$adv_households <- blank_unless(round(exp(rnorm(n, 3.5, 1.1))), reached("time_adv_households") & answered_advice)
main$adv_households[sample(n, 2)] <- 900000

# 6. Staircases ----------------------------------------------------------------
# Every expert walks down one branch, up to the last page they submitted. The time
# column of a node is time_<prefix>_<node>, as in the real data.
tp_nodes <- c("1", "2a", "2b", "3aa", "3ab", "3ba", "3bb", "4aaa", "4aab", "4aba", "4abb", "4baa", "4bab", "4bba", "4bbb",
              "5aaaa", "5aaab", "5aaba", "5aabb", "5abaa", "5abab", "5baaa", "5baab", "5baba", "5babb", "5bbaa", "5bbab", "5bbba", "5bbbb")
risk_nodes <- c("1", "2a", "2b", "3aa", "3ab", "3ba", "3bb", "4aaa", "4aab", "4aba", "4abb", "4baa", "4bab", "4bba", "4bbb",
                "5aaaa", "5aaab", "5aaba", "5aabb", "5abaa", "5abab", "5abba", "5abbb", "5baaa", "5baab", "5baba", "5babb", "5bbaa", "5bbab", "5bbba", "5bbbb")
simulate_staircase <- function(main, prefix, nodes, option_a, option_b) {
  answers <- matrix(NA_character_, n, length(nodes), dimnames = list(NULL, nodes))
  for (i in seq_len(n)) {
    path <- ""
    for (level in 1:5) {
      item <- paste0("time_", prefix, "_", level)
      if (attempts$last_position[i] < position_of(item)) break
      node <- if (level == 1) "1" else paste0(level, tolower(path))
      # this branch has no question at this level
      if (!(node %in% nodes)) break
      letter <- sample(c("A", "B"), 1)
      answers[i, node] <- letter
      path <- paste0(path, letter)
    }
  }
  for (node in nodes) {
    column <- paste0(prefix, "_", node)
    labels <- if_else(answers[, node] == "A", option_a, option_b)
    main[[column]] <- factor(labels, levels = c(option_a, option_b))
    main[[paste0("time_", column)]] <- if_else(!is.na(answers[, node]), rexp(n, 1 / 12) + 3, NA_real_)
  }
  main
}
main <- simulate_staircase(main, "tp", tp_nodes, "100 Euro heute", "Euro in 12 Monaten")
main <- simulate_staircase(main, "risk", risk_nodes,
                           "Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro",
                           "Sichere Zahlung")

# 7. Barriers, rankings, the person --------------------------------------------
barrier_items <- list(ren = "time_barr_ren", dh = "time_barr_dh", conv = "time_barr_conv")
for (topic in names(barrier_items)) {
  for (option in names(BARRIER_LABELS)) {
    main[[paste0("barr_", topic, "_", option)]] <- blank_unless(
      draw_factor(n, TICK_LEVELS, c(.3, .7)), reached(barrier_items[[topic]])
    )
  }
}
for (rank_number in 1:6) {
  main[[paste0("cust_rank", rank_number)]] <- factor(NA, levels = names(CUSTOMER_LABELS))
}
for (rank_number in 1:8) {
  main[[paste0("crit_rank", rank_number)]] <- factor(NA, levels = names(CRITERION_LABELS))
}
for (i in which(reached("time_cust_rank"))) {
  ranked <- sample(names(CUSTOMER_LABELS), 2)
  main$cust_rank1[i] <- ranked[1]
  main$cust_rank2[i] <- ranked[2]
}
for (i in which(reached("time_crit_rank"))) {
  ranked <- sample(names(CRITERION_LABELS), 3)
  main$crit_rank1[i] <- ranked[1]
  main$crit_rank2[i] <- ranked[2]
  main$crit_rank3[i] <- ranked[3]
}

birth_year <- round(rnorm(n, 1970, 11))
birth_year[sample(n, 8)] <- sample(c(65, 72, -1957, 197, 19700), 8, replace = TRUE)
main$dem_birthyear <- blank_unless(birth_year, reached("time_dem_birthyear") & runif(n) > .05)
training_year <- pmin(round(rnorm(n, 1995, 11)), 2026)
training_year[sample(n, 6)] <- sample(c(95, 88, 1, 20255), 6, replace = TRUE)
main$dem_training_year <- blank_unless(training_year, reached("time_dem_training_year") & runif(n) > .05)
main$dem_education <- blank_unless(
  draw_factor(n, c("Kein Abschluss", names(EDUCATION_LABELS)[1:3]), c(.01, .3, .3, .39)),
  reached("time_dem_education")
)
main$dem_education_other <- NA_character_
main$dem_education_recoded <- factor(as.character(main$dem_education), levels = names(EDUCATION_LABELS))
main$dem_education_recode_source <- if_else(is.na(main$dem_education), "no_answer", "original")
for (topic in names(TRAINING_LABELS)) {
  main[[topic]] <- blank_unless(draw_factor(n, c("Ja", "Nein"), c(.4, .6)), reached("time_cpd"))
}
main$cpd_other_text <- blank_unless(rep("Sonstige Fortbildung", n), reached("time_cpd_other_text") & coalesce(main$cpd_other == "Ja", FALSE))
main$dem_own_heating <- blank_unless(
  draw_factor(n, names(OWN_HEATING_LABELS)[1:7], c(.3, .05, .1, .35, .08, .07, .05)),
  reached("time_dem_own_heating") & runif(n) > .03
)
main$fb_comment <- NA_character_

# 8. Times and meta data -------------------------------------------------------
routes <- list(
  time_resp_role = !is.na(main$resp_role),
  time_mix25 = answered_shk, time_brand_top1 = answered_shk,
  time_svc = answered_advice, time_adv_households = answered_advice,
  time_adv_share = answered_advice,
  time_co2_est = coalesce(main$co2_heard == "Ja", FALSE),
  time_cpd_other_text = !is.na(main$cpd_other_text)
)
page_items <- QUESTIONNAIRE$item[str_detect(QUESTIONNAIRE$item, "^time_") &
                                   !str_detect(QUESTIONNAIRE$item, "^time_(tp|risk)_")]
for (item in page_items) {
  seen <- reached(item)
  if (item %in% names(routes)) seen <- seen & routes[[item]]
  main[[item]] <- if_else(seen, rexp(n, 1 / 15) + 2, NA_real_)
}
main$time_interview <- if_else(main$last_position > 0, main$last_position * rexp(n, 1 / 25) + 60, NA_real_)
main$meta_lastpage <- as.integer(main$last_position + sample(0:2, n, replace = TRUE))
main$meta_startdate <- as.POSIXct("2026-07-01 08:00:00", tz = "UTC") + runif(n, 0, 40 * 86400)
main$meta_submitdate <- as.POSIXct(if_else(main$is_complete, main$meta_startdate + 1800, as.POSIXct(NA)))
main$meta_datestamp <- main$meta_startdate + 1800
main$meta_startlanguage <- "de"
main$meta_seed <- sample(1e8, n)

# 9. The vignettes -------------------------------------------------------------
# The design of the real experiment: 128 combinations, six drawn per expert
design <- read_excel(VIGNETTE_FILE) %>%
  mutate(across(everything(), as.character)) %>%
  rename(att_image = bild, att_couple_age = ehepaar_alter, att_income = einkommen,
         att_current_heating = bestehende_heizung, att_replacement_timing = ersatzzeitpunkt,
         att_build_year = baujahr, att_heat_demand = waermebedarf_kwh_m2,
         att_heat_distribution = heizsystem, att_renovation = sanierungsstand_waermebedarf) %>%
  mutate(vignette_id = as.integer(vignette_id))

routed_arm <- case_when(
  as.integer(main$firm_dh_in_area) %in% 1:2 ~ "fw",
  as.integer(main$firm_dh_in_area) %in% 3:4 ~ "nofw",
  TRUE ~ NA_character_
)
vignettes <- main %>%
  transmute(resp_uid, wave, meta_id, target_group, versandrunde, is_softlaunch,
            last_position, true_group, person_number, arm_routed = routed_arm) %>%
  cross_join(tibble(vig_num = 1:6)) %>%
  mutate(vignette_id = sample(design$vignette_id, n(), replace = TRUE)) %>%
  left_join(design, by = "vignette_id", relationship = "many-to-one")

# The recommendations come from a mixed logit with known parameters: correlated
# random constants, the attributes, the group, the region, the heating at home, the
# price of each technology and the expected rise.
OUTCOMES <- names(TECHNOLOGY_LABELS)
TECHNOLOGIES <- setdiff(OUTCOMES, "Wärmepumpe")
true_constants <- c("Pelletheizung" = -0.8, "Hybridheizung" = -0.3, "Gasheizung" = -1.0,
                    "Ölheizung" = -2.2, "Fernwärme" = -0.4, "Keine Empfehlung" = -1.6)
true_regressors <- c("age_60", "income_low", "heating_oil", "urgent", "built_2000",
                     "demand_70", "demand_200", "floor_heating",
                     "is_hvac", "is_sweep", "hdd_c", "heat_pump_home")
true_beta <- matrix(rnorm(length(true_regressors) * length(TECHNOLOGIES), 0, 0.3),
                    nrow = length(true_regressors),
                    dimnames = list(true_regressors, TECHNOLOGIES))
true_L <- matrix(0, 6, 6, dimnames = list(TECHNOLOGIES, TECHNOLOGIES))
true_L[lower.tri(true_L, diag = TRUE)] <- rnorm(21, 0, 0.3)
diag(true_L) <- abs(diag(true_L)) + 0.7
true_price_coefficient <- -0.10
true_rise_coefficient <- -0.20
HDD_MEAN_TRUE <- 3

constants_by_person <- t(true_constants + true_L %*% matrix(rnorm(6 * N_EXPERTS), 6, N_EXPERTS))
colnames(constants_by_person) <- TECHNOLOGIES
hdd_c_true <- rnorm(N_EXPERTS, 0, 0.25)
hp_home_true <- as.numeric(rbinom(N_EXPERTS, 1, 0.3))

v <- vignettes %>%
  mutate(age_60 = as.numeric(att_couple_age == "Mitte 60"),
         income_low = as.numeric(att_income == "unter 40.000 € brutto im Jahr"),
         heating_oil = as.numeric(att_current_heating == "Ölheizung"),
         urgent = as.numeric(att_replacement_timing == "ist defekt und muss kurzfristig ersetzt werden"),
         built_2000 = as.numeric(att_build_year == "2000"),
         demand_70 = as.numeric(att_heat_demand == "70"),
         demand_200 = as.numeric(att_heat_demand == "200"),
         floor_heating = as.numeric(att_heat_distribution == "eine Fußbodenheizung"),
         is_hvac = as.numeric(true_group == "HVAC installers"),
         is_sweep = as.numeric(true_group == "Chimney sweeps"),
         hdd_c = hdd_c_true[person_number],
         heat_pump_home = hp_home_true[person_number] - 0.3)
X <- as.matrix(v[, true_regressors])
rows <- match(v$resp_uid, main$resp_uid)
utility <- matrix(0, nrow(v), length(OUTCOMES), dimnames = list(NULL, OUTCOMES))
for (technology in TECHNOLOGIES) {
  utility[, technology] <- constants_by_person[v$person_number, technology] +
    as.vector(X %*% true_beta[, technology])
}
# The price of each technology, centred at the true means
price_terms <- tibble(
  "Wärmepumpe" = (true_price$hpelec - 28) / 3,
  "Gasheizung" = true_price$gas - 12,
  "Ölheizung" = true_price$oil - 11,
  "Pelletheizung" = true_price$pellet - 8,
  "Fernwärme" = true_price$dh - 14,
  "Hybridheizung" = 0.5 * (true_price$gas - 12) + 0.5 * (true_price$hpelec - 28) / 3
)
PEXP_RISE_LEVELS <- c("bis zu 10 % höher", "mehr als 10% höher")
rise_terms <- tibble(
  "Wärmepumpe" = as.numeric(main$pexp_hpelec %in% PEXP_RISE_LEVELS),
  "Gasheizung" = as.numeric(main$pexp_gas %in% PEXP_RISE_LEVELS),
  "Ölheizung" = as.numeric(main$pexp_oil %in% PEXP_RISE_LEVELS),
  "Pelletheizung" = as.numeric(main$pexp_pellet %in% PEXP_RISE_LEVELS),
  "Fernwärme" = as.numeric(main$pexp_dh %in% PEXP_RISE_LEVELS),
  "Hybridheizung" = 0.5 * as.numeric(main$pexp_gas %in% PEXP_RISE_LEVELS) + 0.5 * as.numeric(main$pexp_hpelec %in% PEXP_RISE_LEVELS)
)
for (technology in names(price_terms)) {
  utility[, technology] <- utility[, technology] +
    true_price_coefficient * price_terms[[technology]][rows] +
    true_rise_coefficient * rise_terms[[technology]][rows]
}
utility <- utility - log(-log(matrix(runif(length(utility)), nrow(utility), ncol(utility))))
# No district heating in the arm without it
utility[which(v$arm_routed != "fw" | is.na(v$arm_routed)), "Fernwärme"] <- -Inf
recommendation <- OUTCOMES[max.col(utility, ties.method = "first")]

vignette_positions <- position_of("vignette_1") + v$vig_num - 1
is_answered <- v$last_position >= vignette_positions & !is.na(v$arm_routed)
vignettes_long <- v %>%
  mutate(vig_rec = factor(if_else(is_answered, recommendation, NA_character_), levels = OUTCOMES),
         vig_rec_other = if_else(vig_rec == "Keine Empfehlung", "Freitext", NA_character_),
         vig_time = if_else(is_answered, rexp(n(), 1 / 30) + 5, NA_real_),
         vig_group_time = vig_time,
         vig_arm = arm_routed,
         arm_source = case_when(
           is.na(arm_routed) ~ "unknown",
           last_position >= position_of("vignette_1") ~ "answered",
           TRUE ~ "routed from Q6a"
         ),
         arm_conflict = FALSE)

# The cost question follows the last vignette
vignettes_long <- vignettes_long %>%
  mutate(has_cost = vig_num == 6 & last_position >= position_of("vignette_cost") & !is.na(vig_rec),
         vig_cost = factor(if_else(has_cost, sample(names(COST_LABELS), n(), replace = TRUE), NA_character_),
                           levels = names(COST_LABELS)),
         vig_cost_source = case_when(
           !has_cost ~ NA_character_,
           vig_arm == "nofw" ~ "nofw",
           vig_rec == "Fernwärme" ~ "fw_dhrec",
           TRUE ~ "fw"
         ),
         vig_cost_time = if_else(has_cost, rexp(n(), 1 / 15) + 2, NA_real_),
         vig_group_cost_time = vig_cost_time,
         vig_arm = factor(vig_arm, levels = c("nofw", "fw"))) %>%
  select(resp_uid, wave, meta_id, target_group, versandrunde, is_softlaunch,
         vig_num, vig_arm, arm_source, arm_conflict, vignette_id,
         vig_rec, vig_rec_other, vig_time, vig_group_time,
         vig_cost, vig_cost_source, vig_cost_time, vig_group_cost_time,
         starts_with("att_")) %>%
  arrange(resp_uid, vig_num, vig_arm)

# One expert answered in both arms, and did not complete
answered_counts <- vignettes_long %>%
  filter(!is.na(vig_rec)) %>%
  count(resp_uid) %>%
  filter(n >= 2, n < 6)
conflict_uid <- answered_counts$resp_uid[1]
conflict_rows <- vignettes_long %>%
  filter(resp_uid == conflict_uid, !is.na(vig_rec))
other_arm <- if_else(conflict_rows$vig_arm[1] == "fw", "nofw", "fw")
vignettes_long <- bind_rows(
  vignettes_long %>% filter(resp_uid != conflict_uid),
  conflict_rows %>% mutate(arm_conflict = TRUE),
  conflict_rows %>% mutate(arm_conflict = TRUE, vig_arm = factor(other_arm, levels = c("nofw", "fw")))
) %>%
  arrange(resp_uid, vig_num, vig_arm)

# 10. survey_main, as R/cleaning/05_main.R writes it ---------------------------
answered_per_expert <- vignettes_long %>%
  filter(!is.na(vig_rec)) %>%
  group_by(resp_uid) %>%
  summarise(n_vignettes_answered = n(), observed_arm = first(as.character(vig_arm)))
survey_main <- main %>%
  left_join(answered_per_expert, by = "resp_uid", relationship = "one-to-one") %>%
  mutate(n_vignettes_answered = coalesce(n_vignettes_answered, 0L),
         reached_vignettes = n_vignettes_answered > 0,
         arm_conflict = resp_uid == conflict_uid,
         vig_arm = if_else(arm_conflict, NA_character_, observed_arm),
         is_complete = !is.na(meta_submitdate)) %>%
  select(-last_position, -true_group, -person_number, -observed_arm) %>%
  select(resp_uid, wave, meta_id, target_group, versandrunde, is_softlaunch,
         is_complete, reached_vignettes, n_vignettes_answered, vig_arm, arm_conflict,
         everything()) %>%
  arrange(resp_uid)
saveRDS(survey_main, file.path(CLEAN_DATA_DIR, "survey_main.rds"))
saveRDS(vignettes_long, file.path(CLEAN_DATA_DIR, "vignettes_long.rds"))

# 11. The sampling frame and the truth -----------------------------------------
n_frame <- as.integer(N_EXPERTS * 12)
frame_types <- sample(c("DENA", "Schornsteinfeger", "SHK"), n_frame, replace = TRUE, prob = c(.3, .1, .6))
frame_hosts <- sample(c("example.org", "t-online.de"), n_frame, replace = TRUE, prob = c(.9, .1))
frame <- tibble(
  id = seq_len(n_frame),
  Email = paste0("person", id, "@", frame_hosts),
  type = frame_types,
  PLZ = sample(plz_lookup$plz, n_frame, replace = TRUE)
)
# A few contacts appear twice, a few without a usable postcode
frame <- bind_rows(frame, frame %>% slice_sample(prop = 0.02) %>% mutate(id = id + n_frame))
frame$PLZ[sample(nrow(frame), 50)] <- NA
saveRDS(as.data.frame(frame), FRAME_FILE)

saveRDS(list(constants = true_constants, beta = true_beta, L = true_L,
             price = true_price_coefficient, expects_rise = true_rise_coefficient,
             note = "Prices centred at gas 12, electricity 28 (over COP 3), district heating 14, oil 11, pellets 8; hybrid half gas, half heat pump."),
        file.path(SECURE_DIR, "truth.rds"))

message("Synthetic data in ", SECURE_DIR, ": ", nrow(survey_main), " questionnaires, ",
        sum(!is.na(vignettes_long$vig_rec)), " recommendations")
