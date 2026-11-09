# The estimation functions and their output

## Output tree

```text
<OUTPUT_DIR>/
  cleaning/                         cleaning_report.xlsx, cleaning_report.rds, codebook
  full_sample/summary_statistics/   everything, including response rates and the deck
  district_heating/                 one folder per estimation script
    summary_statistics/             tables/ figures/ maps/ exhibits.csv summary_statistics.pdf
    m4pe/                           model.rds  results.rds  results.xlsx  cross_validation.rds
    model_comparison/               model_comparison.rds  model_comparison.xlsx
  no_district_heating/              the same
```

`model.rds` holds the fitted `mlogit` object, with the data in it. `results.rds` holds
aggregates only. `cross_validation.rds` exists for the models on the shortlist of
`compare_models()` and holds one held-out log-likelihood per expert, so that a new run need not
estimate the folds again. It is not an aggregate either, so it stays next to `model.rds`.

## `fit_mixed_logit()` and `post_estimation()`

Written and tested; see `R/functions/`. The fitted model has `$name` and `$fit_info`
(`reference`, `outcomes`, `random`, `fixed_parameters`, `start_from`, `log_likelihood_start`,
`n_draws`, `minutes`, `fingerprint`).

## `results.rds`: a list

| Field | Content |
|---|---|
| `name`, `label`, `folder`, `formula` (text), `reference`, `outcomes` (reference first) | what the model is |
| `random` | tibble: `name`, `column`, `technology` of each random constant, in mlogit's order |
| `fixed_parameters` | named vector of the elements of L fixed at 0, or `NULL` |
| `coefficients` | `coef(model)`, named |
| `vcov`, `inverse_hessian`, `meat` | K x K, named: clustered covariance, H^-1, sum of the outer products of the scores |
| `coefficient_table` | `parameter`, `estimate`, `se`, `z`, `p`, `ci_low`, `ci_high` |
| `constant_table` | `parameter` (`sd.<constant>`, `cor.<constant>:<constant>`), `type`, `estimate`, `se`, `ci_low_kr`, `ci_high_kr` |
| `theta_draws` | Krinsky-Robb draws, one row per draw, named columns |
| `fit` | one row: `n_choices`, `n_experts`, `n_parameters`, `n_draws_per_expert`, `log_likelihood`, `log_likelihood_constants_only`, `mcfadden_r2`, `aic`, `bic`, `trace_hinv_b`, `tic` |
| `diagnostics` | list: `code`, `iterations`, `max_gradient`, `scaled_gradient`, `smallest_eigenvalue`, `condition_number`, `start_from`, `log_likelihood_start`, `minutes_estimation` |
| `choice_counts`, `sample_hash` | recommendations in the sample; hash of the experts, to check that models share one sample |
| `ames` | named list, one element per call of `compute_ames()`; see below |
| `n_draws_kr`, `fingerprint`, `git_commit`, `date` | how it was made |

## `compute_ames()`

```r
ames <- compute_ames(
  results,            # from post_estimation()
  make_regressors,    # the script's function: one row per state -> one row per state x alternative
  name = "at_means",  # the name of this set of effects: sheets ame_<name>, baseline_<name>
  at = list(att_income = "65.000 € brutto im Jahr", price_gas_kwh = 12, heat_pump_at_home = 0),
  changes = list(
    income_low = list(to = list(att_income = "unter 40.000 € brutto im Jahr")),
    gas_q25_q75 = list(from = list(price_gas_kwh = 10), to = list(price_gas_kwh = 14))
  ),
  profiles = c("Energy advisers", "HVAC installers", "Chimney sweeps"),
  n_draws = N_DRAWS_PREDICTION,
  n_workers = N_WORKERS,
  force = FALSE
)
```

- `at`: the reference state in the source variables of `make_regressors()`, one value each.
- `changes`: named list. Each change sets `from` (optional, default: `at`) and `to`, on top of
  `at`. Several variables may change together. The effect is P(`to`) - P(`from`), for every
  outcome.
- `profiles`: a character vector sets `expert_group`; or a named list of lists for any other
  column, e.g. `list("All experts" = list(expert_group = "Energy advisers"))`.
- States go through `make_regressors()`, the same function that built the data, so no variable
  is coded twice. It must keep the columns it is given, make one row per state x alternative
  with the column `alternative`, and must not use `vig_rec`.
- Coefficient names tell how a column enters: `<column>:<technology>` (one coefficient per
  technology, `(Intercept):<technology>` is 1) and `<column>` (one coefficient for all, the value
  differs by alternative). Stops if a coefficient has no column.
- Prediction draws: Halton draws, `qnorm(mlogit:::halton(prime, n_draws, 100))`, primes 2, 3,
  5, ... by random constant, restarting per group. The same draws at the estimates and at every
  Krinsky-Robb draw, so that the spread reflects only the uncertainty in theta. Overflow
  protection as in the old `predict_probabilities()`.
- The Krinsky-Robb loop runs on `n_workers` R sessions (`rscript_args = "--vanilla"`), in
  chunks of draws.

Adds to `results$ames[[name]]` a list:

| Field | Content |
|---|---|
| `table` | `profile`, `effect`, `outcome`, `p_from`, `p_to`, `ame`, `se`, `z`, `p`, `ci_low`, `ci_high` (ame +- 1.96 se), `ci_low_kr`, `ci_high_kr` (percentiles) |
| `baseline` | `profile`, `outcome`, `probability`, `se`, `ci_low_kr`, `ci_high_kr`, at `at` |
| `draws` | per profile: states x outcomes x Krinsky-Robb draws, states named `baseline`, `<effect>\|from`, `<effect>\|to` |
| `at`, `changes`, `profiles`, `n_draws`, `fingerprint`, `date` | how it was made |

It saves `results.rds` and rewrites `results.xlsx`, and returns the updated `results`. A call with
the same arguments on the same coefficients loads instead of computing.

## `compare_models()`

```r
comparison <- compare_models(
  est_dir,
  tests = tribble(
    ~question,                     ~smaller, ~larger,
    "Do the prices matter?",       "m4",     "m4p",
    "Levels or logs?",             "m4p",    "m4pl",
    "Do covariances differ?",      "m4pe",   "m6pe"
  ),
  blocks = list(prices = c("price", "gas_price_na")),  # optional, Wald tests in the final model
  final_model = "m4pe",                                # optional
  ame_name = "at_means",                               # optional, the AMEs to compare
  cv_models = c("m4pe", "m6pe"),                       # optional, the shortlist for cross-validation
  ame_threshold = 0.01,   # an AME moving by less than this (1 pp) counts as immaterial
  alpha = 0.05,
  n_fresh_draws = 12,
  n_workers = N_WORKERS
)
```

It reads every model folder in `est_dir`, as a larger model must not lie below any model it
nests, whether or not a test names that model. The rule is at the top of
`R/functions/compare_models.R` and in the README. Writes
`<est_dir>/model_comparison/model_comparison.rds` and `.xlsx`, one sheet each: `decision`,
`validity`, `fit`, `nested_tests`, `non_nested_tests`, `fresh_draws`, `cross_validation`,
`absolute_fit`, `ame_changes`, `block_tests`. The column `final_decision` of `decision` is for the
researchers. A new run overwrites the workbook, and a workbook with `final_decision` filled in is
first kept as a copy, `model_comparison_<date_time>.xlsx`.
