# =============================================================================
# 05_main.R  -  DATA SET 3: one row per expert, everything outside the vignettes
# =============================================================================
# All 4,176 respondents from all twelve waves, including partial responses.
# Nothing is dropped: use is_complete / reached_vignettes / n_vignettes_answered
# to define the analysis sample explicitly.
#
# Join to the vignette data with `resp_uid`.
#
# Output: CLEAN_DATA_DIR/survey_main.{rds,csv,xlsx}; raw_checks, recode_summary
#         and data_quality for the cleaning report (07_report.R)
# =============================================================================

# everything that lives at vignette level belongs in data set 2, not here
vignette_cols <- names(survey_wide) |>
  str_subset("^(vig[0-9]_|att_v[0-9]_|att_extra|time_vig[0-9]_|grouptime_vig[0-9]_|vig_intro$|time_vig_intro$|grouptime_vig6_cost$|time_vig6_cost)")

survey_main <- survey_wide |>
  select(
    # keys and batch information
    resp_uid, wave, meta_id, target_group, versandrunde, is_softlaunch,
    # sample flags
    is_complete, reached_vignettes, n_vignettes_answered, vig_arm, arm_conflict,
    # everything else, minus the vignette module
    !any_of(c(vignette_cols, "vig_arm", "arm_conflict"))
  ) |>
  arrange(resp_uid)

stopifnot(!anyDuplicated(survey_main$resp_uid))
stopifnot(!anyDuplicated(names(survey_main)))

message("survey_main: ", nrow(survey_main), " respondents x ", ncol(survey_main), " variables")

# =============================================================================
# write both data sets
# =============================================================================
# .rds keeps factors, dates and NA exactly - use it for analysis.
# .csv is portable; read it back with read_csv(..., na = c("", "NA")).
# .xlsx is for inspection only.

write_out <- function(x, name) {
  saveRDS(x,   file.path(CLEAN_DATA_DIR, str_c(name, ".rds")))
  write_csv(x, file.path(CLEAN_DATA_DIR, str_c(name, ".csv")), na = "")
  write_xlsx(x, file.path(CLEAN_DATA_DIR, str_c(name, ".xlsx")))
}

write_out(vignettes_long, "vignettes_long")
write_out(survey_main,    "survey_main")

# =============================================================================
# what the raw stage did: the sheets raw_checks, harmonisation, recodes and
# data_quality of the cleaning report, written by 07_report.R
# =============================================================================
raw_checks <- tribble(
  ~check, ~value,
  "waves read",                          as.character(n_distinct(survey_main$wave)),
  "respondents (all waves)",             as.character(nrow(survey_main)),
  "  of which soft launch",              as.character(sum(survey_main$is_softlaunch)),
  "  of which submitted",                as.character(sum(survey_main$is_complete)),
  "  of which reached the vignettes",    as.character(sum(survey_main$reached_vignettes)),
  "vignette rows",                       as.character(nrow(vignettes_long)),
  "  with a recommendation",             as.character(sum(!is.na(vignettes_long$vig_rec))),
  "    of which free text with no coded answer", as.character(rec_from_other),
  "  matched to a design combination",   as.character(sum(!is.na(vignettes_long$vignette_id))),
  "respondents with 6 vignette rows",    as.character(sum(table(vignettes_long$resp_uid) == 6)),
  "respondents answering in both branches (should be 0-1)",
                                         as.character(n_distinct(survey_main$resp_uid[survey_main$arm_conflict])),
  "variables in survey_main",            as.character(ncol(survey_main)),
  "variables in vignettes_long",         as.character(ncol(vignettes_long))
)

recode_summary <- bind_rows(
  survey_main |> count(variable = "firm_radius", source = firm_radius_recode_source),
  survey_main |> count(variable = "dem_education", source = dem_education_recode_source),
  survey_main |> summarise(variable = "plz", source = "valid postcode", n = sum(!is.na(plz))),
  survey_main |> summarise(variable = "nuts3", source = "postcode in Eurostat's list",
                           n = sum(!is.na(nuts3)))
)

# --- data quality: FLAGGED, NEVER ALTERED -------------------------------------
# These values are as the respondents typed them. Nothing here is corrected,
# because how to treat them is a research decision, not a cleaning one. Decide
# in the analysis and document it there.
# Counts only. The old build report also gave examples for most rows: individual
# answers, among them postcodes of firms and the smallest values of a question.
# The rules for outputs allow aggregates only (docs/data_contract.md), so they went,
# as they had for the large-customer postcodes already.
implausible <- function(var, lo, hi, note) {
  x <- survey_main[[var]]
  bad <- !is.na(x) & (x < lo | x > hi)
  tibble(variable = var, rule = str_c("outside ", lo, "-", hi), n = sum(bad), note = note)
}

sums_to_100 <- function(label, pattern, note) {
  tot <- rowSums(survey_main |> select(matches(pattern)), na.rm = TRUE)
  tibble(variable = label, rule = "row total should be 100",
         n = sum(tot > 0 & abs(tot - 100) > 1e-6),
         note = note)
}

data_quality <- bind_rows(
  implausible("dem_birthyear", 1920, 2010,
              "Two-digit entries (e.g. 65) are probably 1965; others are clear typos."),
  implausible("dem_training_year", 1930, 2026,
              "Same pattern: two-digit years plus a few unusable entries."),
  implausible("firm_employees", 0, 10000,
              "Large values may be genuine for big firms - check against firm_revenue."),
  sums_to_100("mix25_* (installed mix 2025)", "^mix25_(hp|pellet|dh|gas|hybrid|oil|other)$",
              "Installed-mix shares that do not sum to 100."),
  sums_to_100("adv_share_* (recommendation shares)", "^adv_share_",
              "Recommendation shares that do not sum to 100."),
  tibble(variable = "plz", rule = "not a German postcode",
         n = sum(!coalesce(survey_main$plz_valid, TRUE)),
         note = "Fragments typed into Q0; plz is NA for these, plz_valid is FALSE."),
  tibble(variable = "bundesland", rule = "postcode in no known range",
         n = sum(survey_main$bundesland_source == "unmatched", na.rm = TRUE),
         note = str_c("Five digits, but no such postcode exists in Germany - typos. ",
                      "Left as NA rather than guessed.")),
  tibble(variable = "bundesland", rule = "assigned from the first three digits",
         n = sum(survey_main$bundesland_source == "plz_prefix", na.rm = TRUE),
         note = str_c("The postcode itself is not in the lookup, but every postcode ",
                      "sharing its first three digits is in one state. Exclude with ",
                      "bundesland_source if that is too generous.")),
  tibble(variable = "bundesland", rule = "postcode straddles a state line",
         n = sum(survey_main$bundesland_source == "plz_border", na.rm = TRUE),
         note = str_c("Postcodes are Deutsche Post routing areas and a few cross a ",
                      "state border. Assigned to the state holding most of the postcode.")),
  tibble(variable = "nuts3", rule = "postcode not in Eurostat's list",
         n = sum(!coalesce(survey_main$plz_in_nuts_list, TRUE)),
         note = str_c("A valid postcode, but not in Eurostat's list of geographic postcodes - ",
                      "mostly P.O.-box or large-customer postcodes. nuts3 is NA; ",
                      "plz and bundesland are kept.")),
  tibble(variable = "nuts3", rule = "Kreis (nuts3) outside the state (bundesland)",
         n = sum(str_sub(survey_main$nuts3, 1, 3) !=
                   BUNDESLAND_NUTS1[as.character(survey_main$bundesland)], na.rm = TRUE),
         note = str_c("nuts3 comes from Eurostat, bundesland from GeoNames/BKG, and the two ",
                      "lookups disagree for a few postcodes (docs/data_notes.md, section NUTS3). ",
                      "Use one of the two consistently in an analysis."))
)

message("\nCleaned data -> ", normalizePath(CLEAN_DATA_DIR))
message("  vignettes_long.rds / .csv / .xlsx")
message("  survey_main.rds / .csv / .xlsx")
message("Reports      -> ", normalizePath(REPORT_DIR))
message("  codebook.xlsx / .csv")
