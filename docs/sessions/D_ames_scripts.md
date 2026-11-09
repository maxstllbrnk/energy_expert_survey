# Session D: the AME function and the estimation scripts

**Owns:** `R/functions/compute_ames.R` (new), `R/functions/post_estimation.R`,
`R/functions/fit_mixed_logit.R`, `R/estimation/` (new scripts), `tests/test_compute_ames.R` (new),
`tests/test_estimation_equivalence.R` (new). Read `docs/results_contract.md`.

Max's request: "in the script where the models are estimated, I want the cleaned data to be loaded. I want
to be able to potentially alter the sample, e.g. only have experts with district heating in there. Then I
want to decide myself what variables to include, bring the data into the right format for mlogit and then
write the model myself. I want to be able to repeat and alter that for as many estimations as I want. An
estimation script for each of my different data files, e.g. one for district heating and one for no district
heating. Then I want to run the summary statistics just on this data. Once the estimation is done, I want to
call a function that estimates the robust standard errors etc. including the standard deviations and
correlations of the alternative specific constants, and then I would like to call a function that computes
the AMEs both for the attributes as well as for any of the covariates. I want to be able to fix the references
and effects myself and also flexibly across the different estimations in that script."

`fit_mixed_logit()`, `post_estimation()`, `simulated_likelihood()` exist and are tested
(`tests/test_estimation_core.R`, `tests/test_simulated_likelihood.R`). You own the first two now; the
likelihood is shared with Session E, so leave `simulated_likelihood.R` alone.

## Tasks

1. **`compute_ames()`** in `R/functions/compute_ames.R`, exactly as in `docs/results_contract.md`. One large
   function plus at most one or two helpers. It replaces `ame_krinsky_robb()`, `predict_probabilities()`,
   `probabilities_krinsky_robb()` of old `R/analysis/00_functions_estimation.R` and all of old
   `R/analysis/01_run_term_effects.R`. Port the overflow-protected matrix products of
   `predict_probabilities()` and its comment. Benchmark to beat: 5,000 Krinsky-Robb draws, 3 profiles, 9 states
   at 5,000 Halton draws on 8 workers in about 30 seconds.

2. **`write_results_excel()`** (in `post_estimation.R`): make it pleasant to read. Add a sheet
   `coefficients_by_technology`: one row per regressor, one column per technology, estimate and standard error
   in adjacent columns, then the terms with one coefficient for all technologies. Round the readable sheets
   sensibly; the long sheets keep every digit for reuse. `compute_ames()` calls it too.

3. **The scripts.** `R/estimation/district_heating.R` (arm `fw`, seven outcomes) and
   `R/estimation/no_district_heating.R` (arm `nofw`, six: no `Fernwärme`). Max edits these himself, so they
   are the most-read files of the project: short, plain, section after section, in this order:
   1. settings: the folder name, the sample label
   2. load `experts.rds`, `vignettes.rds`, `reference_values.rds`; the sample in one filter per line, with
      `count_sample()` logs
   3. `run_summary_statistics(experts, vignettes, file.path(est_dir, "summary_statistics"), label)`
   4. `make_regressors()`: one function, visible, that turns a row with source variables into the regressors
      in one row per state x alternative. Attribute dummies; group dummies; price centred at
      `reference_values`, a price not reported set to 0 with a dummy; COP and `HYBRID_GAS_SHARE`, stated above it
      as the placeholders they are (Max, 2026-10-19, 2026-10-20); expected rise centred at the share, blank at 0
      with a dummy; `hdd_c` in 1,000 degree days, the mean for an expert without `nuts3`, without a dummy;
      `heat_pump_at_home` centred. Definitions: section 2 of old `R/analysis/01_run_specifications.R` and
      `add_alternative_prices()`, `long_terms()` of old `00_functions_estimation.R`.
   5. the data in mlogit's format. Keep the alternatives alphabetical and the rows sorted by `resp_uid`, then
      `vig_num`: that order sets the Halton draws, and with it the acceptance test against the old estimates.
   6. models, each: the formula written out, `fit_mixed_logit()`, `post_estimation()`, `compute_ames()`.
      First version: (4), (4p), (4pe), (6pe) of old section 3, same terms, (4p) starting from nothing, (4pe)
      from (4p), (6pe) from (4pe). Comments say what each adds. A dummy for a blank in Q17 that is the same for
      every expert of the sample has no coefficient, which `make_regressors()` cannot know: state how to drop it.
   7. the effects: `at` and `changes` as visible lists above the calls. The attribute changes, as old
      `ATTRIBUTE_DUMMIES`; the term changes of old `term_changes()`: each fuel's price from q25 to q75, each price
      not reported, each expected rise from no to yes, each blank, the heat pump at home, the degree days from
      q25 to q75. Reference state: `neutral`, i.e. mean prices and degree days, no expected rise, no heat pump at
      home (Max, 2026-10-30, 2026-11-01). Show one more call with another reference state, as Max asked for
      flexibility.
   8. a placeholder comment for `compare_models()`, Session E's, with the `tests` list written out, commented out.
   
   Each script must run top to bottom on synthetic data with `N_DRAWS_MODEL` draws from `config.R`. Adding a
   variable must take one line in `make_regressors()` and one term in a formula: try it. Move what the two scripts
   share only if the duplication hurts: Max prefers to see everything in the script.

4. **Prove it.** `tests/test_estimation_equivalence.R`: on one small simulated model, against the old functions
   (paste the needed definitions from old `00_functions_estimation.R` into the test): `vcov_cluster_robust()` gives
   the same `vcov` (largest relative difference in the standard errors below 1e-3); `ame_krinsky_robb()` with the same
   parameter draws and the same standard draws gives the same AMEs and SEs; `start_from_common()` and
   `start_from_nested()` reproduce the starting log-likelihood. `tests/test_compute_ames.R`: probabilities sum to
   1; an effect of a change with `from` equal to `to` is 0; a change in `at` moves the reference; a model by group uses
   the group's constants; a covariate entering through `make_regressors()` shows up without other changes; at a
   simple logit with no random constants' spread (L near 0) the probabilities match the closed form; loading works.

## Done when

Both scripts run on synthetic data, and the estimates recover `truth.rds` of `tests/simulate_data.R` within a
reasonable distance with the small sample (say so if not); `results.xlsx` reads well; every test passes.
