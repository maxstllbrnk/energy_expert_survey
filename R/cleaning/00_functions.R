# =============================================================================
# 00_functions.R  -  shared helpers (definitions only, no side effects)
# =============================================================================
# Two ideas hold this pipeline together:
#
# 1. The raw CSV headers already carry the LimeSurvey question code, e.g.
#       "Q24[SQ005]. Haben Sie in den letzten 3 Jahren ..."
#       "Vignette3mitFWTime. Fragenzeit: Vignette3mitFW"
#    so every column can be found BY NAME. The old pipeline renamed columns by
#    POSITION, which is why the soft-launch waves (415 and 416 columns instead
#    of 417) could not be merged.
#
# 2. The LimeSurvey syntax files are used only as METADATA - question wording
#    and answer options - never to assign column positions.
# =============================================================================


# --- reading the LimeSurvey syntax files (metadata only) ---------------------

# "AO01","AO02"  ->  c("AO01","AO02")      (character levels)
# 1,2            ->  c("1","2")            (numeric levels, as written)
split_levels <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  if (str_detect(x, '"')) {
    x |> str_split('"\\s*,\\s*"') |> pluck(1) |> str_remove_all('^"|"$') |> str_trim()
  } else {
    x |> str_split(",") |> pluck(1) |> str_trim()
  }
}

# One row per variable: question id, question text, and the answer options.
read_syntax <- function(path) {
  lines <- read_lines(path, locale = locale(encoding = "UTF-8"))

  grab <- function(pattern) {
    hit <- str_subset(lines, pattern)
    tibble(
      col   = as.integer(str_match(hit, "\\[,?\\s*([0-9]+)\\]")[, 2]),
      value = str_match(hit, '^[^"]*"(.*)"[^"]*$')[, 2]
    )
  }

  codes  <- grab('^names\\(data\\)\\[[0-9]+\\] <- "')
  labels <- grab('^attributes\\(data\\)\\$variable\\.labels\\[[0-9]+\\] <- "')

  factors <- str_subset(lines, "^data\\[, ?[0-9]+\\] <- factor\\(")
  factors <- tibble(
    col        = as.integer(str_match(factors, "\\[,\\s*([0-9]+)\\]")[, 2]),
    val_codes  = str_match(factors, "levels=c\\((.*)\\),labels=c\\(")[, 2],
    val_labels = str_match(factors, ",labels=c\\((.*)\\)\\)\\s*$")[, 2]
  )

  coerce <- str_subset(lines, "^data\\[, ?[0-9]+\\] <- as\\.(numeric|character)\\(")
  coerce <- tibble(
    col     = as.integer(str_match(coerce, "\\[,\\s*([0-9]+)\\]")[, 2]),
    is_numeric = str_detect(coerce, "as\\.numeric\\(")
  )

  codes |>
    rename(question_id = value) |>
    left_join(labels |> rename(question_text = value), by = "col") |>
    left_join(factors, by = "col") |>
    left_join(coerce,  by = "col") |>
    arrange(col)
}

# Metadata for every wave found in SYNTAX_DIR, as one tibble.
read_all_syntax <- function(dir = SYNTAX_DIR) {
  tibble(path = list.files(dir, "^survey_[0-9]+_R_syntax_file\\.R$", full.names = TRUE)) |>
    mutate(wave = str_match(basename(path), "^survey_([0-9]+)_")[, 2],
           meta = map(path, read_syntax)) |>
    select(wave, meta) |>
    unnest(meta)
}


# --- reading the raw survey exports ------------------------------------------

# "Q24[SQ005]. Haben Sie ..."  ->  "Q24_SQ005"   (matches the syntax files)
# "interviewtime. Gesamtzeit"  ->  "interviewtime"
header_code <- function(x) {
  x |>
    str_remove("^﻿") |>
    str_remove("\\..*$") |>          # keep everything before the first "."
    str_replace_all("\\[([^]]+)\\]", "_\\1")
}

# "groupTime10885. Gruppenzeit: Vignette 1 (ohne FW)" -> "Gruppenzeit: Vignette 1 (ohne FW)"
header_label <- function(x) str_remove(x, "^[^.]*\\.\\s*")

# The numeric groupTime ids differ in EVERY wave, but the group labels are
# identical across all twelve. So the label is the only stable key.
GROUP_TIME_NAMES <- c(
  "Gruppenzeit: Allgemein"                                                  = "grouptime_general",
  "Gruppenzeit: Einleitung Vignetten"                                       = "grouptime_vig_intro",
  "Gruppenzeit: Vignette 1 (ohne FW)"                                       = "grouptime_vig1_nofw",
  "Gruppenzeit: Vignette 2 (ohne FW)"                                       = "grouptime_vig2_nofw",
  "Gruppenzeit: Vignette 3 (ohne FW)"                                       = "grouptime_vig3_nofw",
  "Gruppenzeit: Vignette 4 (ohne FW)"                                       = "grouptime_vig4_nofw",
  "Gruppenzeit: Vignette 5 (ohne FW)"                                       = "grouptime_vig5_nofw",
  "Gruppenzeit: Vignette 6 (ohne FW)"                                       = "grouptime_vig6_nofw",
  "Gruppenzeit: Vignette 1 (mit FW)"                                        = "grouptime_vig1_fw",
  "Gruppenzeit: Vignette 2 (mit FW)"                                        = "grouptime_vig2_fw",
  "Gruppenzeit: Vignette 3 (mit FW)"                                        = "grouptime_vig3_fw",
  "Gruppenzeit: Vignette 4 (mit FW)"                                        = "grouptime_vig4_fw",
  "Gruppenzeit: Vignette 5 (mit FW)"                                        = "grouptime_vig5_fw",
  "Gruppenzeit: Vignette 6 (mit FW)"                                        = "grouptime_vig6_fw",
  "Gruppenzeit: Kostenfrage letzte Vignette"                                = "grouptime_vig6_cost",
  "Gruppenzeit: Energiepreise und ETS"                                      = "grouptime_prices",
  "Gruppenzeit: Generelle Empfehlungen zu Heiztechnologien und Herstellern" = "grouptime_recommend",
  "Gruppenzeit: Zeitpräferenzen"                                            = "grouptime_timepref",
  "Gruppenzeit: Risikopräferenzen"                                          = "grouptime_riskpref",
  "Gruppenzeit: Marktentwicklung, Dienstleitungen und Kunden"               = "grouptime_market",
  "Gruppenzeit: Persönliche Angaben"                                        = "grouptime_personal",
  "Gruppenzeit: Feedback"                                                   = "grouptime_feedback"
)

# The separator is whatever was picked in the LimeSurvey export dialog: the
# first set of exports was semicolon-separated, the current one is comma-
# separated. Work it out from the header instead of hardcoding it - the wrong
# separator reads the whole file as a single column, which fails a long way
# downstream and looks like a naming bug.
DELIMS <- c(",", ";", "\t")

detect_delim <- function(path) {
  header <- read_lines(path, n_max = 1, locale = locale(encoding = "UTF-8"))
  n_fields <- map_int(DELIMS,
                      ~ length(scan(text = header, what = "", sep = .x,
                                    quote = '"', quiet = TRUE)))
  if (max(n_fields) < 2)
    stop("Cannot tell which separator ", basename(path), " uses; its header ",
         "parses as a single column with , ; and tab.", call. = FALSE)
  DELIMS[which.max(n_fields)]
}

# Read one raw export. Everything comes in as text; value labels and numeric
# types are applied later, once, so that waves can never disagree about a type.
read_raw_wave <- function(path) {
  raw <- read_delim(path, delim = detect_delim(path), quote = '"',
                    col_types = cols(.default = col_character()),
                    locale = locale(encoding = "UTF-8"), na = c("", "NA"),
                    name_repair = "minimal", progress = FALSE)

  header <- names(raw)
  names(raw) <- header_code(header)

  # classify each column
  kind <- case_when(
    names(raw) == "interviewtime"           ~ "interview_time",
    str_starts(names(raw), "groupTime")     ~ "group_time",
    str_ends(names(raw), "Time")            ~ "question_time",
    TRUE                                    ~ "question"
  )

  # group times get their stable name from the German group label
  gt <- kind == "group_time"
  labs <- header_label(header[gt])
  unknown <- setdiff(labs, names(GROUP_TIME_NAMES))
  if (length(unknown))
    stop("Unknown group-time label(s) in ", basename(path), ": ",
         str_c(unknown, collapse = " | "),
         "\nAdd them to GROUP_TIME_NAMES in R/cleaning/00_functions.R.", call. = FALSE)
  names(raw)[gt] <- GROUP_TIME_NAMES[labs]

  list(data = raw, kind = set_names(kind, names(raw)))
}


# --- the batch overview ------------------------------------------------------
# One row per wave. Versandrunde tells us which waves were the soft launch, so
# that is never hardcoded here.
load_versand <- function(path = VERSAND_FILE) {
  read_excel(path, sheet = "Versandübersicht", skip = 7, na = c("", "NA"),
             .name_repair = "unique_quiet") |>
    select(wave = `Umfrage-ID`, target_group = Zielgruppe,
           versandrunde = Versandrunde, versanddatum = Versanddatum) |>
    filter(!is.na(wave)) |>
    mutate(wave = as.character(wave)) |>
    summarise(target_group  = first(target_group),
              versandrunde  = first(versandrunde),
              first_sent    = suppressWarnings(min(as.Date(versanddatum), na.rm = TRUE)),
              n_batches     = n(),
              .by = wave) |>
    mutate(is_softlaunch = str_detect(str_to_lower(versandrunde), "soft"))
}


# --- analysis variable names -------------------------------------------------
# lowercase snake_case, <block>_<concept>[_<item>].
#   meta_   LimeSurvey admin fields      vig_/att_  vignette module
#   firm_   company characteristics      price_/pexp_  energy prices
#   co2_    CO2 price knowledge          mix25_  installed mix 2025
#   tp_     time preference              risk_   risk preference
#   svc_    services offered             adv_    advisory volume & shares
#   barr_   perceived barriers           cust_/crit_  ranking questions
#   dem_    respondent demographics      fb_     free-text feedback

NAME_MAP <- c(
  id = "meta_id", submitdate = "meta_submitdate", lastpage = "meta_lastpage",
  startlanguage = "meta_startlanguage", seed = "meta_seed", token = "meta_token",
  startdate = "meta_startdate", datestamp = "meta_datestamp",

  Q0 = "firm_zip", Q0a = "resp_is_owner", Q0b = "resp_role",
  Q2_SQ001 = "firm_cat_engineering", Q2_SQ002 = "firm_cat_energyadvice",
  Q2_SQ003 = "firm_cat_hvac",        Q2_SQ004 = "firm_cat_chimney",
  Q2_other = "firm_cat_other",
  Q3 = "firm_employees", Q4 = "firm_revenue",
  Q6 = "firm_radius", Q6_other = "firm_radius_other", Q6a = "firm_dh_in_area",

  G02Q120 = "vig_intro",
  KostenVignetteohneFW = "vig6_cost_nofw",
  KostenVignettemitFW  = "vig6_cost_fw",
  KostenVignetteFW     = "vig6_cost_fw_dhrec",

  Q15_SQ001 = "price_gas", Q15_SQ002 = "price_hpelec", Q15_SQ003 = "price_dh",
  Q15_SQ004 = "price_oil", Q15_SQ005 = "price_pellet",
  Q17_SQ001 = "pexp_gas",  Q17_SQ002 = "pexp_hpelec",  Q17_SQ003 = "pexp_dh",
  Q17_SQ004 = "pexp_oil",  Q17_SQ005 = "pexp_pellet",

  Q18 = "co2_heard", Q18b_SQ001 = "co2_est_low", Q18b_SQ002 = "co2_est_high",

  Q11b_WP = "mix25_hp",   Q11b_PEL = "mix25_pellet", Q11b_FW = "mix25_dh",
  Q11b_GAS = "mix25_gas", Q11b_HYB = "mix25_hybrid", Q11b_OEL = "mix25_oil",
  Q11b_SON = "mix25_other",
  Q11bTop1 = "mix25_top1", Q11bTop2 = "mix25_top2", Q11c1b = "brand_top1",

  Q8_SQ001 = "svc_advice_resid",   Q8_SQ002 = "svc_advice_nonresid",
  Q8_SQ003 = "svc_expert_report",  Q8_SQ004 = "svc_energy_cert",
  Q8_SQ006 = "svc_funding_advice", Q8_SQ005 = "svc_other",

  Q10a = "adv_households",
  Q10b_SQ001 = "adv_share_hp",     Q10b_SQ002 = "adv_share_pellet",
  Q10b_SQ003 = "adv_share_dh",     Q10b_SQ004 = "adv_share_gas",
  Q10b_SQ007 = "adv_share_oil",    Q10b_SQ005 = "adv_share_hybrid",
  Q10b_SQ006 = "adv_share_other",

  Q21 = "dem_birthyear", Q22 = "dem_education", Q22_other = "dem_education_other",
  Q23 = "dem_training_year",
  Q24_SQ001 = "cpd_hp",         Q24_SQ002 = "cpd_hp_electrician",
  Q24_SQ003 = "cpd_renovplan",  Q24_SQ004 = "cpd_energyadvice",
  Q24_SQ005 = "cpd_begfunding", Q24_SQ006 = "cpd_other",
  Q24a = "cpd_other_text",
  Q20 = "dem_own_heating", G22Q118 = "fb_comment"
)

# Array/battery questions never appear as data columns, but DO get a response
# time each. Without these their timings would all collapse onto one name.
ARRAY_NAME_MAP <- c(
  Q2 = "firm_cat", Q8 = "svc", Q10b = "adv_share", Q11b = "mix25",
  Q15 = "price", Q17 = "pexp", Q18b = "co2_est", Q24 = "cpd",
  Q19a = "barr_ren", Q19b = "barr_dh", Q19c = "barr_conv",
  Q7 = "cust_rank", Q28 = "crit_rank"
)

TECH_SUFFIX <- c(WP = "hp", PEL = "pellet", FW = "dh", GAS = "gas",
                 HYB = "hybrid", OEL = "oil", SON = "other")

BARR_ITEMS <- c(SQ001 = "none",       SQ002 = "funds",   SQ003 = "labor",
                SQ004 = "progcomplex", SQ005 = "quality", SQ006 = "skepticism",
                SQ007 = "regchange",  SQ008 = "opcost",  SQ009 = "capex",
                SQ010 = "infra",      SQ012 = "other")
BARR_TOPIC <- c(Q19a = "ren", Q19b = "dh", Q19c = "conv")

# The nine vignette design attributes, in the order attribute_1..9 within each
# vignette. Read off vignetten_kombinationen.xlsx (the authoritative design
# file); the German source names are kept in the codebook.
ATTRIBUTE_NAMES <- c("image", "couple_age", "income", "current_heating",
                     "replacement_timing", "build_year", "heat_demand",
                     "heat_distribution", "renovation")

ATTRIBUTE_SOURCE <- c(image = "bild", couple_age = "ehepaar_alter",
                      income = "einkommen", current_heating = "bestehende_heizung",
                      replacement_timing = "ersatzzeitpunkt", build_year = "baujahr",
                      heat_demand = "waermebedarf_kwh_m2",
                      heat_distribution = "heizsystem",
                      renovation = "sanierungsstand_waermebedarf")

# LimeSurvey question code -> analysis variable name.
var_name <- function(qid) {
  map_chr(qid, function(q) {
    if (q %in% names(NAME_MAP)) return(unname(NAME_MAP[q]))

    if (str_detect(q, "^Vignette[1-6](ohne|mit)FW(_other)?$")) {
      k   <- str_match(q, "^Vignette([1-6])")[, 2]
      arm <- if (str_detect(q, "ohneFW")) "nofw" else "fw"
      oth <- if (str_detect(q, "_other$")) "_other" else ""
      return(str_c("vig", k, "_rec_", arm, oth))
    }
    if (str_detect(q, "^Q11b(Rand|Score)")) {
      kind <- str_to_lower(str_match(q, "^Q11b(Rand|Score)")[, 2])
      tech <- str_remove(q, "^Q11b(Rand|Score)")
      return(str_c("mix25_", kind, "_", unname(TECH_SUFFIX[tech])))
    }
    if (str_detect(q, "^Time[0-9][A-B]*$")) return(str_c("tp_",   str_to_lower(str_remove(q, "^Time"))))
    if (str_detect(q, "^Risk[0-9][A-B]*$")) return(str_c("risk_", str_to_lower(str_remove(q, "^Risk"))))
    if (str_detect(q, "^Q19[abc]_SQ[0-9]+$"))
      return(str_c("barr_", unname(BARR_TOPIC[str_remove(q, "_.*$")]), "_",
                   unname(BARR_ITEMS[str_remove(q, "^.*_")])))
    if (str_detect(q, "^Q7_[0-9]+$"))  return(str_c("cust_rank", str_remove(q, "^Q7_")))
    if (str_detect(q, "^Q28_[0-9]+$")) return(str_c("crit_rank", str_remove(q, "^Q28_")))
    if (str_detect(q, "^attribute_[0-9]+$")) {
      n <- as.integer(str_remove(q, "^attribute_"))
      if (n > N_VIGNETTES * N_ATTRIBUTES) return(str_c("att_extra", n))
      v <- (n - 1L) %/% N_ATTRIBUTES + 1L
      s <- (n - 1L) %%  N_ATTRIBUTES + 1L
      return(str_c("att_v", v, "_", ATTRIBUTE_NAMES[s]))
    }
    NA_character_
  })
}

# Name for a response-time column. Array questions resolve through
# ARRAY_NAME_MAP; anything still unmapped falls back to the lowercased code, so
# two timings can never silently land on the same name.
time_var_name <- function(qid) {
  v <- var_name(qid)
  from_array <- is.na(v) & qid %in% names(ARRAY_NAME_MAP)
  v[from_array] <- unname(ARRAY_NAME_MAP[qid[from_array]])
  v[is.na(v)] <- str_to_lower(str_replace_all(qid[is.na(v)], "[^A-Za-z0-9]+", "_"))
  str_c("time_", v)
}
