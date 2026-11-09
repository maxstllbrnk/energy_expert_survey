# Session E: the model comparison

**Owns:** `R/functions/compare_models.R` (new), `tests/test_compare_models.R` (new). Read
`docs/results_contract.md` and `R/functions/simulated_likelihood.R`, `post_estimation.R`, `fit_mixed_logit.R`.

Max's request: "What are the right measures to compare different models and either accept or reject them? I
want this more systematic, in the sense that the results are robust and it is clear why we chose that model." "At
the end of the script I would like to create output that tells us how these models performed against each other.
The output should also be put into a folder."

## Why (from the seminar tables, 2 November 2026)

- (4pe) vs (6pe): LR 69.2, df 42, p 0.005, robust p below 0.001; but AIC and BIC favour (4pe).
- (4p) vs (6p): p 0.099, robust p 0.034: the tests disagree at 5%.
- (4p) vs (4pl): 0.7 log points; (4p) vs (4pd): 2.9. Fresh Halton draws alone move a log-likelihood with SD 2.0 and
  range 6.5, on simulated data at fixed parameters, and two October runs of one model differed by 4.5.

## The protocol: `compare_models()` runs all of it

Exact arguments in `docs/results_contract.md`. Reads `<est_dir>/<model>/results.rds` and `model.rds`.

| Step | Sheet | Measure |
|---|---|---|
| 1. Validity | `validity` | per model: code, iterations, scaled gradient, smallest eigenvalue, condition number, log-likelihood against every model it nests. A flag: valid or not, and why. An invalid model takes no part in a decision |
| 2. Noise | `fresh_draws` | log-likelihood of each model and LR of each test on `n_fresh_draws` fresh Halton sets: mean, SD, min, max. Randomly shifted: `(u + runif(1)) %% 1` per prime, `u` the Halton sequence, the same shifts for every model and set: one shift per prime, so that constants by group and common constants see the same draws. `prepare_likelihood(model, z_all = ...)`, `want_scores = FALSE`, on `N_WORKERS` sessions |
| 3. Nested tests | `nested_tests` | LR, df, classical p, robust p, mean weight: port `lr_tests()`, `lr_restrictions()` of old `R/analysis/00_functions_output.R` (Kent 1982; White 1982). Holm-adjusted robust p over all nested tests declared. Mean LR on fresh draws alongside |
| 4. Non-nested tests | `non_nested_tests` | Vuong (1989) z with BIC correction, from per-expert log-likelihoods (`log(fitted(model))` at the first decision of each expert, or `simulated_likelihood()`), aligned by expert. Note in the sheet: with many parameters it can over-reject (Shi 2015), so it never decides alone |
| 5. Parsimony | `fit` | per model: K, log-likelihood, McFadden R², AIC, BIC, TIC, and per test the differences |
| 6. Out of sample | `cross_validation` | for `cv_models` only: 5 folds by expert, stratified by group where a group column can be recovered, same folds for all models (a fixed seed set right before the draw). Re-estimate on 4 folds with `mlogit`, starting from the full-sample estimates; log-likelihood of the held-out experts with `iterlim = 0`. Per model: held-out log-likelihood per expert. Per pair: mean gain per expert with a paired SE over experts. Folds on `N_WORKERS` sessions. It costs re-estimation: note the minutes in the sheet |
| 7. Absolute fit | `absolute_fit` | predicted against observed shares of each recommendation, overall and by attribute level where the model's data hold the attribute dummies, from `model$probabilities`; root mean squared difference per model |
| 8. Substance | `ame_changes` | if `ame_name` is given and both models have it: largest and median absolute change in the AMEs and baseline probabilities between the two models, in pp, on the rows they share, and relative to the AME's SE |
| 9. Final model | `block_tests` | for `final_model`, robust Wald test of each block (`wald_test()` of old `00_functions_output.R`), names in `blocks` |

**Nesting.** Decide for each test: if the smaller model's parameters are among the larger's, the restriction is
the added parameters (`regressors`/`prices` of the old code, now read from the names). If the larger has constants by
group and the smaller one set with group shifts, it is `group_covariances` (old `lr_restrictions()`). Otherwise
non-nested. Check the df against the difference in parameters, and that both models share `sample_hash`, the reference
and the draws: stop otherwise. A test between a model with random constants and one with none, where a variance sits on
the boundary, needs a mixture of chi-squared distributions (Self and Liang 1987): say so in the sheet if it ever occurs;
it does not in the first version.

**The `decision` sheet, one row per test.** All key numbers, a verdict per criterion, and the rule's proposal.
Decision rule for a larger model, fixed before results are read:
- not valid: `not compared`;
- accept if the robust test rejects at `alpha` after Holm, the gain survives fresh draws (mean fresh LR above the
  critical value of the robust test's distribution, or above chi-squared with df if simpler; say which), and
  cross-validation improves by more than one paired SE (BIC falls, if cross-validation was not run);
- the test says yes, but cross-validation or BIC says no: `keep smaller: tests and parsimony disagree`;
- all say yes, but no AME moves by `ame_threshold` or more: `keep smaller as main; report larger as robustness`;
- the test says no: `keep smaller`.
Non-nested: `prefer <model>` if Vuong and cross-validation (BIC if not run) agree; otherwise `data cannot decide`.
Each proposal comes with its reason in words, the final call is the researchers'. A column `final_decision` stays empty
for them to fill.

**Output.** `<est_dir>/model_comparison/model_comparison.rds` (a named list of the sheets) and `model_comparison.xlsx`.
Aggregates only. Print the `decision` sheet at the end.

## Done when

`tests/test_compare_models.R`, on the synthetic cleaned data or simulated decisions with known truth and a small number
of draws:
- a model with a truly irrelevant block is rejected and a truly relevant one accepted;
- a case where a large model buys little is caught by BIC or cross-validation;
- non-nested levels against logs: runs and reports;
- the fresh-draw LR is close to the estimation LR when the gain is large, and the noise is reported;
- a smaller model that ends above the larger is flagged in `validity`;
- two models on different samples stop;
- `model_comparison.xlsx` reads well: open it with `readxl`.

Use `fit_mixed_logit()` and `post_estimation()` for the fixtures, with few draws and few experts
(`EES_N_EXPERTS` in `tests/simulate_data.R`), and keep the test under about 15 minutes.
