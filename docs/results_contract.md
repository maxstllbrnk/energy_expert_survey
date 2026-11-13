# The estimation functions and their output

## Output tree

```text
<OUTPUT_DIR>/
  cleaning/                         cleaning_report.xlsx, cleaning_report.rds, codebook
  full_sample/summary_statistics/   everything, including response rates and the deck
  district_heating/                 one folder per estimation script
    summary_statistics/             tables/ figures/ maps/ exhibits.csv summary_statistics.pdf fingerprint.rds
    m4pe/                           model.rds  results.rds  results.xlsx  cross_validation.rds
    model_comparison/               model_comparison.rds  model_comparison.xlsx
  no_district_heating/              the same
```

`model.rds` holds the fitted `mlogit` object, with the data in it. `results.rds` holds
aggregates only. `cross_validation.rds` exists for the models of `cv_models` in
`compare_models()` and holds one held-out log-likelihood per expert, so that a new run need not
estimate the folds again. It is not an aggregate either, so it stays next to `model.rds`. It is
saved also if a fold failed: delete it to run its folds again. While the folds run, each writes
`cross_validation_fold_<k>.rds`, so that a run that is cut off resumes with the folds it had;
they go once `cross_validation.rds` is written.

A quick run (`QUICK_RUN` in `config.R`) writes the same tree to `output_quick/`.

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

- `name`: at most 22 characters, as an Excel sheet name holds 31.
- `at`: source variables of `make_regressors()`, one value each, set for every unit. Without a `population`, the whole reference state; with one, only what is the same for everyone, usually the reference vignette.
- `changes`: named list. Each change sets `from` (optional, default: the unit as it is, that is `at` and the profile's columns without a population) and `to`, on top of the unit. Several variables may change together. The effect is P(`to`) - P(`from`), for every outcome, each the average over the profile's units. A state is built in this order: the unit (`at` and the profile's columns without a population), then the change's, so a change can also move the group.
- `profiles`: without a population a character vector sets `expert_group`, or a named list of lists sets any column, e.g. `list("All experts" = list(expert_group = "Energy advisers"))`. With one the same lists select the units whose value is among the given ones: `list("All experts" = list())` is everyone, `list("Advisers with a heat pump" = list(expert_group = "Energy advisers", heat_pump_at_home = 1))` those.
- `population`: `NULL` (a reference expert per profile), or a table with one row per unit: `resp_uid` and the source variables of `make_regressors()` that `at` does not set. Keep only these columns, as each is copied for every state. A unit is an expert (one row per expert) or an answer (one row per expert x vignette).
- `exclude_na` (default `TRUE`): with a population, an effect leaves out the units whose own value of a variable it sets is missing, unless it sets that variable to `NA` in `from` or `to`. A price change from the 25th to the 75th percentile is thus averaged over the experts who reported that price; `n_experts` in the table says how many. `FALSE`: nobody is left out.
- `check_sample` (default `TRUE`): stops if the experts of the population are not those of the model's estimation sample (`sample_hash`). `FALSE` for another population on purpose.
- `n_draws`: Halton draws of the prediction. The Krinsky-Robb draws are those of the results (`N_DRAWS_KR`). Both are 5,000 in a real run, over observed experts too (Max, 2026-11-12).
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

### Four kinds of effect, one function

What the units are decides what an effect is. Every probability is the average of the model's probability over the profile's units, at the Halton draws of the random constants; an effect is the difference of two such averages.

| | `population` | `at` | An effect is |
|---|---|---|---|
| A | `NULL` | the whole reference state | the change at one fictitious reference expert per profile (the earlier "AME") |
| B | the experts | the reference vignette | the average change over the observed experts, each keeping their own characteristics, as if all had seen the reference vignette |
| C | the answers (expert x vignette) | `list()` | the same over the vignettes the experts answered, which is the design's mix of households |
| D | the experts | another household | as B for that household |

```r
# B, and D by another `at`: observed experts, the reference vignette set for everyone
population_experts <- experts %>%
  select(resp_uid, expert_group, hdd, heat_pump_at_home, ends_with("_kwh"), ends_with("_rise"))
results_m4pe <- compute_ames(
  results_m4pe, make_regressors, name = "observed",
  at = at_vignette,                      # only the att_* variables
  changes = c(attribute_changes, price_changes, expectation_changes, expert_changes),
  profiles = c(list("All experts" = list()),
               list("Energy advisers" = list(expert_group = "Energy advisers")),
               list("HVAC installers" = list(expert_group = "HVAC installers")),
               list("Chimney sweeps" = list(expert_group = "Chimney sweeps"))),
  population = population_experts
)
# C: the answers, nothing set; each attribute change with `from` at its reference level
# (attribute_changes_answers in the estimation scripts), see the notes below
population_answers <- decisions %>% select(resp_uid, vig_num, expert_group, hdd, ..., starts_with("att_"))
results_m4pe <- compute_ames(results_m4pe, make_regressors, name = "answers", at = list(),
                             changes = attribute_changes_answers, population = population_answers,
                             profiles = list("All experts" = list()))
```

Notes on the units:

- The change of a variable sets it for every unit that the effect includes: `from = list(price_gas_kwh = q25), to = list(price_gas_kwh = q75)` puts every included expert at the 25th and then at the 75th percentile, whatever they reported; `to` alone moves each expert from their own value, so every unit already at that value adds an effect of 0 to the average. For the usual AME over the answers, give `from` the reference level, as the estimation scripts do for version C. For the effect of not answering, give `from` the reference value (as the lists in the estimation scripts do), so that it is a reporter at the mean against a non-reporter, whatever the unit's own answer. With a population, a variable that one side sets to `NA` has to be set by the other side too: the call stops otherwise, as the experts who did not answer would add an effect of 0 to the average.
- An expert in no profile costs nothing; a profile that selects nobody stops.
- The probabilities are averaged inside the Krinsky-Robb loop, in blocks of units over `n_workers` sessions: `results.rds` holds the averages and nothing at the level of experts or answers. The size of the design matrix (units x states x outcomes x coefficients) is printed, and the call stops above 8 GB.
- The work is about units x states x outcomes x prediction draws x Krinsky-Robb draws, so a call at the 5,000 and 5,000 draws of a real run costs about 125 times the same call in a quick run (`QUICK_RUN` in `config.R`, 1,000 and 200 draws). Timing the quick run on the real data and multiplying by 125 is the best estimate. Measured on synthetic data (2026-11-12), (4pe) of `district_heating.R`: 703 experts, 7 outcomes, 31 changes (52 states per expert), quick-run draws, two sessions: about 130 seconds per call, up to 300 with other jobs on the computer. At full draws that is about 4.5 hours on two sessions and 1 to 1.5 on eight; per 1,000 experts about 1.5 to 3 hours per call on eight sessions. Each estimation script makes five such calls for (4pe): four households and the answers. The effects over the answers cost two states per answer and change (with `from`), so the attributes alone cost about twice as much as all changes over the experts. A reference expert costs a minute; `n_draws` can be lowered per call.
- Over all the answers of the sample with nothing set in `at`, the average probabilities are the shares of the recommendations up to the simulation error; a message says if they differ by more than 2 percentage points.
- The log says which effects leave experts out of the average (`exclude_na`) and by how many at most, and which effects no unit of any profile is included in. `results.rds` and `results.xlsx` show every count, however small (Max, 2026-11-12): they stay on the secure drive and are not shared. The output script that makes the exhibits applies the minimum cell.

Adds to `results$ames[[name]]` a list:

| Field | Content |
|---|---|
| `table` | `profile`, `effect`, `outcome`, `p_from`, `p_to`, `ame`, `se`, `z`, `p`, `ci_low`, `ci_high` (ame +- 1.96 se), `ci_low_kr`, `ci_high_kr` (percentiles), `n_units`, `n_experts` (the units and distinct experts the effect averages over: 1 and `NA` for a reference expert, 0 and blank numbers if no unit of the profile is included) |
| `baseline` | `profile`, `outcome`, `probability`, `se`, `ci_low_kr`, `ci_high_kr`, `n_units`, `n_experts`: the units as they are (at `at`) |
| `draws` | per profile: states x outcomes x Krinsky-Robb draws of the averages, states named `baseline`, `<effect>\|from`, `<effect>\|to` |
| `at`, `changes`, `profiles`, `n_draws`, `n_draws_kr`, `fingerprint`, `date` | how it was made |
| `population` | `NULL` without a population; otherwise `experts`, `units` and `exclude_na`, no data |

It saves `results.rds` and rewrites `results.xlsx`, and returns the updated `results`. A call with
the same arguments on the same coefficients loads instead of computing.

## `results.xlsx`

The sheets to read come first, rounded, with frozen headers: `overview`,
`coefficients_by_technology`, `constants_sd`, `constants_correlations`, `pp_<name>` (the effects in percentage points, one per call of `compute_ames()`), `effect_definitions`, `choice_counts`. The long sheets with every digit follow, with grey tabs: `fit`, `coefficients`, `ame_<name>`, `baseline_<name>`.

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
  cv_models = c("m4", "m4p", "m4pl", "m4pe", "m6pe"),  # optional, the models to cross-validate
  ame_threshold = 0.01,   # an AME moving by less than this (1 pp) counts as immaterial
  alpha = 0.05,
  n_fresh_draws = 12,
  n_workers = N_WORKERS
)
```

It reads every model folder in `est_dir`, as a larger model must not lie below any model it
nests, whether or not a test names that model. The rule is at the top of
`R/functions/compare_models.R` and in the README. Writes `<est_dir>/model_comparison/model_comparison.rds` and `.xlsx`, one sheet each: `decision`,
`validity`, `fit`, `nested_tests`, `non_nested_tests`, `fresh_draws`, `cross_validation`,
`absolute_fit`, `ame_changes`, `block_tests`. The column `final_decision` of `decision` is for the researchers. A new run overwrites the workbook, and a workbook with `final_decision` filled in is first kept as a copy, `model_comparison_<date_time>.xlsx`.

`ame_changes` has three quantities per test: `AME`, the effects both models define alike, as the
change between them; `AME the larger model adds`, the effects only the larger model has, as the
change from 0; and `baseline probability`. The rule's 1 pp step counts the rows of the first two
that move by 1 pp, an added AME only if its 95% Krinsky-Robb interval (`ci_low_kr`, `ci_high_kr`)
excludes 0 (Max, 2026-11-09). An effect of the same name that the two models define differently
counts in neither.
