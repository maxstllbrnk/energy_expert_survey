# =============================================================================
# 03_recode.R  -  repairs and flags on the stacked data
# =============================================================================
# Four things, all ADDITIVE - originals are never overwritten:
#
#   1. postcode      LimeSurvey stores Q0 as a number, so 01067 arrives as 1067.
#                    Adds plz (5-char string) and plz_valid.
#   2. bundesland    the federal state, looked up from plz.
#   3. "Sonstiges"   LimeSurvey writes "-oth-" into the answer column and the
#                    typed text into <question>_other. "-oth-" is not an answer
#                    code, so it labels to NA and looks like a nonresponse.
#                    Q6 and Q22 are recovered here from their free text.
#                    (The vignettes are handled in 02_read_stack.R, where
#                    "-oth-" becomes the 7th option "Keine Empfehlung".)
#   4. flags         completeness and the Fernwaerme branch.
# =============================================================================

# --- 1. postcode -------------------------------------------------------------
# German postcodes run 01001-99998. Anything that would pad to "00xxx" is a
# fragment someone typed (a single digit, or a two-digit region), not a
# postcode, and is marked invalid rather than guessed at.
survey_wide <- survey_wide |>
  mutate(
    .zip_num  = suppressWarnings(as.numeric(firm_zip)),
    plz_valid = if_else(is.na(.zip_num), NA,
                        .zip_num >= 1001 & .zip_num <= 99998 & .zip_num == floor(.zip_num)),
    plz       = if_else(coalesce(plz_valid, FALSE), sprintf("%05d", as.integer(.zip_num)),
                        NA_character_)
  ) |>
  select(-.zip_num)

# --- 2. bundesland -----------------------------------------------------------
# The questionnaire never asks for the federal state, only for the postcode, but
# the state is worth having as an expert characteristic: heating stock, district
# heating coverage and building age all differ sharply between states.
#
# The lookup is a plain CSV in Dropbox, built once by
# R/geodata/build_plz_bundesland.R from the official BKG boundaries and the
# GeoNames postcode file. Nothing is downloaded here and no spatial package is
# needed - see that script for where the data comes from and why the state is
# read off the AGS rather than from a point-in-polygon join.
#
# `bundesland_source` says how each answer was reached, so any part of it can be
# excluded from an analysis:
#   plz          the postcode is in the lookup and lies in one state only
#   plz_border   in the lookup, but the postcode straddles a state line; it is
#                assigned to the state holding most of its places
#   plz_prefix   not in the lookup, but every postcode sharing its first three
#                digits is in the same state (see PLZ_PREFIX_MIN_PURITY)
#   unmatched    a valid-looking postcode in a range that does not exist
#   no_postcode  Q0 was blank or unusable

# How unanimous the surrounding postcodes have to be before the first three
# digits are allowed to stand in for a postcode the lookup does not have.
PLZ_PREFIX_MIN_PURITY <- 0.9

if (!file.exists(PLZ_LOOKUP_FILE)) {

  survey_wide <- survey_wide |>
    mutate(bundesland = NA_character_, bundesland_source = NA_character_)
  message("NOTE: bundesland is NA - ", PLZ_LOOKUP_FILE, " does not exist yet.\n",
          "      Build it once with:  Rscript R/geodata/build_plz_bundesland.R")

} else {

  plz_lookup <- read_csv(PLZ_LOOKUP_FILE, col_types = cols(plz = col_character(),
    bl_code = col_character(), bundesland = col_character(),
    n_states = col_integer(), share = col_double(), n_places = col_integer()))

  # All sixteen states become levels whether or not anyone lives there, in
  # official key order, so tables and maps keep a stable, complete axis.
  BUNDESLAENDER <- plz_lookup |> distinct(bl_code, bundesland) |> arrange(bl_code)

  # The stand-in for postcodes the lookup does not have. Deutsche Post assigns
  # postcodes geographically, so the first three digits pin down a region far
  # more tightly than the first one or two: across the 10,812 known postcodes
  # this rule reproduces the right state 98.3% of the time (leave-one-out).
  # Only prefixes that are effectively unanimous are used at all.
  plz3_lookup <- plz_lookup |>
    count(plz3 = str_sub(plz, 1, 3), bundesland) |>
    mutate(purity = n / sum(n), .by = plz3) |>
    slice_max(n, n = 1, by = plz3, with_ties = FALSE) |>
    filter(purity >= PLZ_PREFIX_MIN_PURITY)

  exact  <- match(survey_wide$plz, plz_lookup$plz)
  prefix <- match(str_sub(survey_wide$plz, 1, 3), plz3_lookup$plz3)

  survey_wide <- survey_wide |>
    mutate(
      bundesland = factor(
        coalesce(plz_lookup$bundesland[exact], plz3_lookup$bundesland[prefix]),
        levels = BUNDESLAENDER$bundesland),
      bundesland_source = case_when(
        is.na(plz)                     ~ "no_postcode",
        !is.na(exact) & plz_lookup$n_states[exact] > 1 ~ "plz_border",
        !is.na(exact)                  ~ "plz",
        !is.na(prefix)                 ~ "plz_prefix",
        TRUE                           ~ "unmatched")
    )

  message("Bundesland: ", sum(!is.na(survey_wide$bundesland)), " of ", nrow(survey_wide),
          " respondents (", sum(survey_wide$bundesland_source == "plz_prefix"),
          " via a 3-digit prefix, ",
          sum(survey_wide$bundesland_source == "plz_border"), " on a border postcode, ",
          sum(survey_wide$bundesland_source == "unmatched"), " postcode(s) in no known range)")
}

# --- 3. "Sonstiges" free text ------------------------------------------------
# Q6, service radius. Levels: bis 20 km < bis 50 km < bis 100 km < bundesweit.
#   - "gar nicht" / "keine"      -> its own level, this is a real answer
#   - a distance in km           -> the matching band (largest if several given;
#                                   digits only count when followed by "km", so
#                                   postcodes in the text are not read as distances)
#   - nationwide wording         -> bundesweit
#   - anything else, including >100 km that is not nationwide -> Sonstiges
#     (the scale has no band between 100 km and nationwide, and calling a 150 km
#     radius "bundesweit" would overstate it)
recode_radius <- function(txt) {
  t <- str_squish(str_to_lower(txt))
  if (is.na(t) || t == "" || t == "-") return(NA_character_)
  if (str_detect(t, "gar nicht|nicht mehr|^keine$|^0$|^0 ")) return("Bietet keine an")

  km <- as.numeric(str_extract_all(t, "[0-9]+(?=\\s*km)")[[1]])
  if (!length(km) && str_detect(t, "^[0-9]+([.,-][0-9]+)?$"))
    km <- as.numeric(str_split_1(t, "[-,]"))
  km <- km[is.finite(km) & km > 0]

  if (length(km)) {
    d <- max(km)
    return(if (d <= 20) "bis 20 km" else if (d <= 50) "bis 50 km"
           else if (d <= 100) "bis 100 km" else "Sonstiges")
  }
  if (str_detect(t, "bundesweit|deutschlandweit|weltweit|europaweit|international|global"))
    return("bundesweit")
  "Sonstiges"
}

# Q22, highest qualification. Only unambiguous qualifications are mapped.
# Deliberately NOT mapped, because mapping them would be a guess:
#   - school-leaving certificates ("Abitur") - neither a completed
#     apprenticeship nor a degree, and "Kein Abschluss" would misrepresent them
#   - job titles and certifications ("Energieberater", "DENA geprüft") - these
#     describe what someone does, not their level of education
EDUCATION_MAP <- c(
  "dr.ing. arch."                                     = "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)",
  "ing. (fh)"                                         = "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)",
  "ing. + mba"                                        = "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)",
  "prof. für energieversorgung und energiewirtschaft" = "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)",
  "meister, dipl. ing architekt ; sachverständiger"   = "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)",
  # "Bachelor Professional" is the Meister-level qualification, not a degree.
  "bachelor gestellte fortbildung"                    = "Meister, Techniker oder vergleichbarer Abschluss"
)

recode_education <- function(txt) {
  t <- str_squish(str_to_lower(txt))
  if (is.na(t) || t == "") return(NA_character_)
  if (t %in% names(EDUCATION_MAP)) return(unname(EDUCATION_MAP[t]))
  "Sonstiges"
}

# Builds <var>_recoded and <var>_recode_source for one question.
add_recode <- function(data, var, other_var, fun, extra_levels) {
  original <- data[[var]]
  mapped   <- map_chr(as.character(data[[other_var]]), fun)

  data[[str_c(var, "_recoded")]] <- factor(
    case_when(!is.na(original) ~ as.character(original), TRUE ~ mapped),
    levels = c(levels(original), extra_levels))

  data[[str_c(var, "_recode_source")]] <- case_when(
    !is.na(original)      ~ "original",
    is.na(mapped)         ~ "no_answer",
    mapped == "Sonstiges" ~ "other_unmapped",
    TRUE                  ~ "mapped")
  data
}

survey_wide <- survey_wide |>
  add_recode("firm_radius",   "firm_radius_other",   recode_radius,
             extra_levels = c("Bietet keine an", "Sonstiges")) |>
  add_recode("dem_education", "dem_education_other", recode_education,
             extra_levels = "Sonstiges")

# --- 4. flags ----------------------------------------------------------------
# The Fernwaerme branch is between-subject: Q6a routes each respondent into
# exactly one of the two. `arm_conflict` catches the (rare) exception rather
# than hiding it.
rec_fw   <- str_c("vig", seq_len(N_VIGNETTES), "_rec_fw")
rec_nofw <- str_c("vig", seq_len(N_VIGNETTES), "_rec_nofw")

survey_wide <- survey_wide |>
  mutate(
    n_fw   = rowSums(!is.na(pick(all_of(rec_fw)))),
    n_nofw = rowSums(!is.na(pick(all_of(rec_nofw)))),
    n_vignettes_answered = n_fw + n_nofw,
    reached_vignettes    = n_vignettes_answered > 0,
    arm_conflict         = n_fw > 0 & n_nofw > 0,
    vig_arm = case_when(n_fw > 0 & n_nofw == 0 ~ "fw",
                        n_nofw > 0 & n_fw == 0 ~ "nofw",
                        TRUE                   ~ NA_character_),
    is_complete = !is.na(meta_submitdate)
  ) |>
  select(-n_fw, -n_nofw)

if (any(survey_wide$arm_conflict))
  warning(sum(survey_wide$arm_conflict), " respondent(s) answered in BOTH Fernwaerme ",
          "branches (arm_conflict = TRUE): ",
          str_c(survey_wide$resp_uid[survey_wide$arm_conflict], collapse = ", "),
          call. = FALSE, immediate. = TRUE)

message("Recoded: ", sum(!is.na(survey_wide$plz)), " valid postcodes, ",
        sum(survey_wide$firm_radius_recode_source == "mapped", na.rm = TRUE),
        " Q6 + ",
        sum(survey_wide$dem_education_recode_source == "mapped", na.rm = TRUE),
        " Q22 answers recovered from free text")
