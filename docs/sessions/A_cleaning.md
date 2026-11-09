# Session A: the cleaning report

**Owns:** `R/cleaning/`, `R/run_cleaning.R`, `tests/selftest.R`,
`tests/test_cleaning_equivalence.R` (new), `R/summary_statistics/section_cleaning.R`.

Max's request: "there should be a cleaning pipeline where all the cleaning and data manipulation
takes place, that includes adding the heating degree days, assigning experts to expert groups.
In cases where that is not possible to assign an expert to a group they get NA. This cleaning
pipeline should report the editing and data manipulation results."

`R/cleaning/06_experts_vignettes.R` exists and runs on synthetic data. It was ported from
`R/prepare_analysis_data.R`, `R/add_preferences.R`, `R/descriptives/02_data.R`, section 2.1 of
`R/analysis/01_run_specifications.R` and `R/ruecklauf/02_link.R` of the old code. You own it now.

## Tasks

1. **Prove the port.** `tests/test_cleaning_equivalence.R`: on the synthetic `survey_main` and
   `vignettes_long`, run the old functions (source only the needed function definitions from a copy
   under `tests/`, or paste them into the test) and show that the new columns equal the old ones:
   `add_berufsgruppe()` against `expert_group` (names mapped; `Keine Zuordnung` becomes `NA`),
   `clean_reported_prices()`, `add_last_question()` (positions), `add_preference_scores()`,
   the shares, the degree days, the dedup of `load_respondents()`. The one intended difference: the
   outlier rule runs over everyone who answered, instead of the completers of the three groups.
   Fix what differs, or report why it should.

2. **The report.** `R/cleaning/07_report.R`, sourced at the end of `R/run_cleaning.R`, writes into
   `REPORT_DIR`:
   - `cleaning_report.xlsx`, one sheet per topic, each readable on its own. `overview` (counts from
     raw rows to persons to completers to experts with a group, by arm); `waves`;
     `attempts`; `expert_groups` (who gets which group, how many `NA` and why, how many ticked
     several categories); `prices` (per fuel: answered, as reported, euro, per litre or kg, not
     usable, the median and the bounds; before and after means); `expectations`; `own_heating`;
     `outliers`; `shares`; `preferences`; `geography` (valid postcodes, `bundesland_source`,
     `nuts3`, `hdd`, lookups disagreeing); and from the raw stage, when it ran: `harmonisation`,
     `recodes`, `data_quality`; and `variables`, a data dictionary: every column of `experts` and
     `vignettes` with its type, N answered, and its question text from the codebook (or its
     definition for a derived column).
   - `cleaning_report.rds`: the same tables as a named list, read by `section_cleaning()`.
   - The raw-stage tables live in `05_main.R` today, which writes `build_report.xlsx`. Keep the
     objects, drop that file, and write them here. In a synthetic run the raw stage does not run, so
     those sheets are skipped.
   - The `cleaning_log` of step 06 is a start: reshape it as you need. A report row says what a rule
     did: how many answers it saw, changed, set to missing.

3. **`section_cleaning(dirs)`**, in `R/summary_statistics/section_cleaning.R`: reads
   `cleaning_report.rds` and writes LaTeX tables for the data appendix of the paper into
   `dirs$tables` with `table_custom()`: `tab_clean_sample.tex`, `tab_clean_groups.tex`,
   `tab_clean_prices.tex`, `tab_clean_outliers.tex`. Returns one `exhibit_row()` per table, block
   `"0. Data cleaning"`, group `"All experts"`, placement `"appendix"`. Returns `tibble()` if the file
   does not exist. `run_summary_statistics()` calls it for the full sample only.

4. **`tests/selftest.R`:** rewrite it for the new data sets. Keep the checks that matter for the real
   data run by Max: 12 waves, keys unique, every vignette row joins, six rows per person, every row
   matches a design combination, all 128 combinations appear, soft-launch alignment. Drop the rest.
   It must also run on synthetic data, skipping what only the raw stage can have. Look at the tail of
   the old file for checks on preferences.

5. Read `06_experts_vignettes.R` critically against the old code and fix mistakes.

## Done when

`tests/simulate_data.R`, `R/run_cleaning.R`, `tests/selftest.R` and
`tests/test_cleaning_equivalence.R` run clean in that order on synthetic data, and you have opened
`cleaning_report.xlsx` (with `readxl`) and read its sheets.
