# Energy experts survey: cleaning, summary statistics, mixed logit models

A survey of heating experts (energy advisers, HVAC installers, chimney sweeps) in Germany, twelve
mailing waves. Each expert sees six vignettes of a household and its heating system and recommends a heating technology. This repository holds the code from the raw LimeSurvey exports to the estimated models. It holds no data and no results.

## Where everything lives

All paths are built in `config.R` from two roots, which each machine sets in a git-ignored `config.local.R` in the project root: `SECURE_DIR`, the survey's folder on the secure drive (`.../wave_1_2026`), and `DROPBOX_DIR`, the Dropbox folder that holds `public_data_dropbox/`. Nothing else differs between machines.

```r
SECURE_DIR <- "I:/.../energy_experts_survey/wave_1_2026"
DROPBOX_DIR <- "C:/Users/<you>/Dropbox/Heating Transition/analysis_survey"
```

`SECURE_DIR` holds `260909_raw_energy_expert_survey_data/` (the raw exports, with `limesurvey-syntax/`), `260909_cleaned_energy_expert_survey_data/`, `260915_scraped_data/` (the sampling frame) and `output/`.

| | Holds | Shared with |
|---|---|---|
| **Secure drive** | raw exports, cleaned data, sampling frame, and all output, as fitted models contain the data | the project team on the drive; personal data |
| **Dropbox** | non-sensitive inputs in `public_data_dropbox/`: design matrix, batch overview, geodata, degree days | the coauthors |
| **This folder** | code only | everyone, through GitHub |

```text
wave_1_2026/output/                  on the secure drive
  cleaning/                          cleaning_report.xlsx, codebook
  full_sample/summary_statistics/    all summary statistics, with response rates
  district_heating/                  one folder per estimation script
    summary_statistics/              tables/  figures/  maps/  summary_statistics.pdf
    m4pe/                            model.rds  results.rds  results.xlsx
    m6pe/                            one folder per model
    model_comparison/                model_comparison.xlsx  model_comparison.rds
  no_district_heating/               the same
```

## How to run

```r
renv::restore()                                  # once per computer
source("R/run_cleaning.R")                       # raw exports to experts.rds, vignettes.rds, the report
source("tests/selftest.R")                       # checks on the result
source("R/run_summary_statistics.R")             # the full sample, with response rates
source("R/estimation/district_heating.R")        # sample, summary statistics, models, comparison
source("R/estimation/no_district_heating.R")     # the same for the other arm
source("R/check_results.R")                      # a text summary of every run, to share for checking
```

`R/check_results.R` writes `check_results.txt` to the output folder: for every estimation folder it finds, each model's formula, fit, convergence and coefficients, the comparison, and every set of effects with its checks, side by side. It needs no change when the estimation scripts change. It prints aggregates only, with anything that rests on fewer than 10 experts shown as `<10`, so the file can be shared with someone who has no access to the secure drive.

Open `energy_expert_survey.Rproj` first, or run `Rscript <file>` from this folder. Requires R 4.6.2. The decks need `pdflatex` (MiKTeX) on R's PATH: if `Sys.which("pdflatex")` is empty, the run skips them with a message, and the rest runs. A re-run reloads every model, result and set of summary statistics whose inputs have not changed, so changing one model re-estimates that model only.

**A first run to see that everything runs:** set `QUICK_RUN <- TRUE` in `config.R`. The models then use 100 draws, the AMEs 1,000 and Krinsky-Robb 200, nothing is cross-validated, and everything goes to `output_quick/`, so that it is never taken for the real output. Set it back to `FALSE` for the real run, which estimates every model again. The full run cross-validates every model, which adds five estimations per model; on four sessions that is several hours per script.

## An estimation script is yours to edit

There is one script per data set in `R/estimation/`. Each goes through the same steps in plain sight:

1. **The sample.** Load the cleaned data, one `filter()` per step.
2. **Summary statistics** of exactly that sample.
3. **`make_regressors()`**, one function that turns source variables into regressors. The same function builds the data and the states of the AMEs, so nothing is coded twice.
4. **The reference state and the effects**: `at_neutral`, the households, the lists of `changes` and the populations, in the source variables.
5. **The models.** You write each formula, then call `fit_mixed_logit()` and `post_estimation()`.
6. **The effects of every model**: one loop calls `compute_ames()` for each set of effects of each model, with the changes of that model's terms (`changes_of_model`).
7. **`compare_models()`** at the end.

To add a variable: one line in `make_regressors()`, one term in a formula, its reference value in `at_neutral`, and, for a variable of the expert, its name in `expert_variables`, which both populations carry. Its effect is a change in one of the lists, and that list goes into `changes_of_model` for every model that has the term. Models that do not use it stay loaded. To study another sample: copy a script and change its filters and its folder name.

| Function | What it does |
|---|---|
| `fit_mixed_logit()` | Estimates your formula with `mlogit`. Keeps the fixes that a direct call would miss: the order of the random constants, starting values from a nested model, Halton primes by group, the restart after code 3, the re-evaluation at code 2. Stops before estimating, and names the term, if a term has missing values or no finite estimate |
| `post_estimation()` | Standard errors clustered by expert, Krinsky-Robb draws, standard deviations and correlations of the constants, fit and diagnostics. Writes `results.rds` and `results.xlsx` |
| `compute_ames()` | The change in every probability for any change, of attributes or covariates, for any profile, with Krinsky-Robb standard errors and percentile intervals: at one reference expert, or averaged over the observed experts or their answers (`population`), at any reference vignette (`at`). Four kinds of effect, one function: `docs/results_contract.md` |
| `compare_models()` | The protocol below. Writes `model_comparison.xlsx` |
| `run_summary_statistics()` | All tables, figures and maps of a sample, and the deck |

`results.xlsx` starts with the sheets to read: overview and fit, coefficients by technology, standard
deviations and correlations of the constants, then one sheet `ame_<name>` per set of effects, the main set (`observed_reference`, `MAIN_AMES` in `config.R`) first: the probabilities at the baseline in %, then every effect in percentage points with its Krinsky-Robb standard error and 95% interval, with filters. The long sheets with every digit follow, with grey tabs. The fields of `results.rds` are in `docs/results_contract.md`, so that later scripts can build any table or figure from them.

On a model with 113 parameters and 1,500 simulated experts, on an 8-core laptop, a Hessian takes at most about 11 minutes, against about 2 hours before, and all AMEs of a model at a reference expert about a minute; the effects over the observed experts take about 125 times as long as in a quick run, some hours per set (per 1,000 experts about 1.5 to 3 hours on eight sessions at 5,000 prediction draws, `docs/results_contract.md`). The main set of every model uses 5,000 prediction draws, the other households and the answers 1,000 (`N_DRAWS_PREDICTION_OTHER`), which together take about a quarter of the time. With four models per script, a full run of both scripts was estimated at about 25 hours on eight sessions. With the eight steps of the covariates (12 models in arm fw, 11 in nofw), the effects alone take about five times as long, scaled from the synthetic runs, so a full run takes several days; none of this is measured on the real data yet. `mlogit()` itself takes as long as it takes. Lower `N_WORKERS` in `config.R` if something else runs on the computer. The cross-validation runs on four sessions, as a fold of (6pe) needs an estimated 3 to 4 GB per session.

## How models are compared

No single measure settles it. Among the seminar's models, the LR test preferred (6pe) to (4pe) while AIC and BIC did the opposite, and classical and robust tests disagreed on (4p) against (6p). `compare_models()` therefore runs one fixed protocol over the questions declared at the end of a script, and writes one `decision` sheet with one row per question:

| Step | Question | Measure |
|---|---|---|
| Validity | Is it a maximum? | Scaled gradient, condition number, log-likelihood at least that of every model it nests. A model that did not converge, or whose Hessian is not positive definite, stops the script earlier, in `fit_mixed_logit()` or `post_estimation()` |
| Noise | Is the gain more than simulation noise? | Log-likelihood and LR on fresh Halton draws |
| Nested test | Is the extra structure there? | Robust and classical LR, Holm correction across the declared tests |
| Non-nested test | Which is closer to the data? | Vuong test on per-expert log-likelihoods |
| Parsimony | Is it worth its parameters? | AIC, BIC with experts as N, robust TIC |
| Out of sample | Does it predict new experts? | 5-fold cross-validation by expert, for every model |
| Absolute fit | Does it reproduce the data? | Predicted and observed shares |
| Substance | Do the answers change? | Largest change in the AMEs both models have, and largest AME only the larger model has |
| Final model | Is it stable? | Robust Wald test of each block |

**The rule, fixed before results are read.** A larger model is accepted only if it is valid, the robust test rejects at 5% after Holm, the gain survives fresh draws, and cross-validation improves by more than one paired standard error (BIC falls, if it was not run or a fold failed). If tests and parsimony disagree, the smaller stays. If all agree but no AME moves by 1 percentage point, the smaller stays as the main model and the larger is reported as robustness. An AME that only the larger model has, such as a price effect in (4p) against (4), counts as moving from 0, if its 95% interval excludes 0. Of two models that do not nest each other, one is preferred only if Vuong's test and cross-validation (or BIC) agree on it; otherwise the data cannot decide. The rule proposes and the researchers decide: the column `final_decision` of the sheet `decision` is theirs, also for
terms of direct research interest that stay regardless. A nested test that cannot be computed, because its restrictions are not independent at the estimates, is not compared. The reason warns if the robust test rests on few experts.

**Changes to the rule.** On 2026-11-14 (Max), after the quick run on the real data had been read, the AME step switched from the effects at a reference expert (`neutral`) to the main AME, `observed_reference`, the average over the observed experts at the reference vignette, read for the three groups and not for the pooled row, whose narrower interval could let an added effect count on its own. A change to what the rule reads is recorded here, with name and date.
On 2026-11-14, with the eight steps of the covariates (Claude's proposals, open until Max decides): the sets of (4), (4p), (4pe) and (6pe) gain the joint change of year and demand (`built_2000_demand_70`), and those of (4pe) and (6pe) the large expected rise of each fuel (`expects_large_rise_<fuel>`, there equal to the rise of up to 10%), which count as added AMEs where the smaller model lacks them and as shared ones otherwise; and an effect leaves out the units whose regressors its change does not move, so that a row that was an exact 0 is blank and drops out of the comparison. With the steps, the Holm family grows from three questions to eleven in arm fw and from two to ten in nofw, so the earlier questions face a stricter level.
On 2026-11-15, with the price relative to gas (Claude's proposal, agreed by Max): the price changes keep their definition, each fuel's reported price from its 25th to its 75th percentile, but `price_gas` now moves every technology other than the gas heating and no recommendation, where it moved only the gas heating before. The price effects that (4p) adds over (4) change with it.

The gates are not redundant. In one run on synthetic data, with equal covariances by group and 100 draws per expert, the robust test wrongly rejected them (LR 61.2, robust p 0.006), and only the fresh draws (mean LR 39.9) and cross-validation caught it. That is one draw, not an error rate, and at 1,000 draws the fresh-draw gate will bind less often.

## Summary statistics

`run_summary_statistics()` works on any sample: what does not apply, such as response rates in a sub-sample, is skipped and listed in the deck. Exhibits exist for all experts and for each expert group, unless they compare the groups themselves.
`summary_statistics.pdf` shows the current version of everything: it is rebuilt whenever the sample, the code or an input changes, and a run that changes nothing leaves it alone. If it is open in a PDF viewer, the run says so; close it and run again. 
`exhibits.csv` lists each exhibit with its caption and whether it is meant for the main text or the appendix.

Tables are LaTeX fragments: numbers with N, mean, SD and quartiles, answers with N and shares. Figures and maps are PDFs, in the project's original colours. No statistic rests on fewer than 10 answers, no response rate on fewer than 30 contacts, and no table shows a minimum or maximum.

## Things to know before analysing

- **The arm is not randomised.** Q6a routes each expert into the arm with (`fw`) or without (`nofw`) the district-heating option, so comparisons between the two scripts are descriptive. `Fernwärme` is an answer only in `fw`.
- **One row per person.** A person who started twice keeps the furthest attempt: 4,176 questionnaires are 3,937 persons,  of whom 2,551 completed. A token is per survey, so someone invited in two waves counts twice.
- **Response rates by group mix two sources.** The denominator is the mailing list, the numerator the answer to Q2: without the token lists, a response cannot be matched to its list.
- **`NA` in `expert_group`** means Q2 was not answered or no category was ticked. Eight completers have no group.
- **`NA` in `own_heating` and `heat_pump_at_home`** means the expert stopped before Q20; `Keine Angabe` means Q20 was left blank. Completers all reached Q20, so the estimation samples hold no `NA` there.
- **`vig_rec` has seven levels:** the six technologies and `Keine Empfehlung`, the pre-registered seventh option. Free text without a coded answer counts as `Keine Empfehlung`. `NA` means the vignette was not answered.
- **Soft launch.** Q6 had no "bundesweit" option, and Q7 was a different ranking task. Filter on `is_softlaunch` before pooling those.
- **Design.** No house built in 2000 is unrenovated (200 kWh/m²) or has oil heating. `compute_ames()` leaves out of an effect every expert or answer that the change takes into such a house, and every one whose regressors it does not move (a household that already has the level named; a covariate that enters only for another group, such as the heat pump share installed for anyone but HVAC installers): such effects are blank, and `n_experts` says how many an effect averages over (Max, 2026-11-14; the regressors rather than the variables: Claude's proposal, 2026-11-14, open). A reference state itself is not checked. `att_heat_demand` is the same split as `att_renovation`.
- **Vignettes are nested in experts.** All standard errors are clustered by expert.
- **The sample of the models** (Max, 2026-11-14): completers with a group, age, year of training and the heat pump share of their group, in every model, as all models share one sample. After (4pe) eight steps add the covariates one block at a time, each model `m4pe_<k>` tested against the one before: year × demand, Q17 in four categories, the heating at home in four groups, the years since training, age, a course on heat pumps, the heat pump share of the past year, and the chimney sweeps' own slope of that share (`docs/open_issues.md`, section 1).
- **Prices** (Claude's proposal, agreed by Max, 2026-11-15): the term `log_price_vs_gas` holds, in each technology's rows, the log of its fuel's price over the gas price, 0 at the mean prices and 0 for the gas heating and no recommendation, with one coefficient for all technologies. So no COP enters the models. A blank price counts as the fuel's mean, with one dummy per fuel in its own technology's rows (electricity in the heat pump's and the hybrid's). Read the effects of those dummies (`price_na_<fuel>`) as "price not known", not as price effects. A price change still moves the reported price from its 25th to its 75th percentile; the gas price thus moves every other technology.
- **"AME"** has two meanings here, always averaged over the random constants. The main one (Max, 2026-11-14), set `observed_reference`: over the observed experts (`population` of `compute_ames()`), each expert keeps their own characteristics, the change is made to each of them at the reference vignette, and the changes in probability are averaged over the experts (a profile is a group of them; "All experts" weights the groups by their share of the sample). At a reference expert (no `population`, set `neutral`, kept to compare with earlier results): one fictitious expert, e.g. with mean prices, which is an effect at representative values. In a nonlinear model the two differ; the first is what Stata's `margins` computes for the observed experts (`margins, at()` for the reference vignette). The random constants are averaged over their distribution in the population, not conditioned on each expert's own answers. Prices, expected rises, a heat pump at home and degree days are not randomised, so their effects are associations either way.

## The code

```text
config.R                       the roots, every path, the survey's constants
R/setup.R                      packages, figure settings, sources all functions
R/run_cleaning.R               step 1, runs R/cleaning/
R/run_summary_statistics.R     step 2, the full sample
R/estimation/                  step 3, one script per data set; these are the files to edit
R/check_results.R              after a run, a text summary of every estimation folder, to share
R/functions/                   the estimation functions, labels and colours, table and figure helpers
R/summary_statistics/          one function per block of the summary statistics, and the deck
R/geodata/                     builds the postcode lookup, once
tests/                         synthetic data and tests
docs/                          the columns of the cleaned data, the results, notes on the data, open issues
```

Every data decision is made once, in `R/cleaning/06_experts_vignettes.R`, with its reason and date in the comments. English labels and colours are in `R/functions/labels.R`. More on the data, such as how the waves are merged and how the state is derived from the postcode, is in `docs/data_notes.md`. `docs/open_issues.md` holds what the rebuild left for the researchers: the decisions taken, what to check on the first real run, and the statistical and identification issues to keep in mind for the paper.

## Testing without the real data

`tests/simulate_data.R` writes synthetic data with the real column names, answer levels and waves and invented values: routing, break-offs, repeat attempts, answers in the wrong unit, typos, and recommendations from a mixed logit with known parameters. A session with `CLAUDECODE=1` reads and writes only these, in a temporary folder (`config.R`), and never the secure drive.

| Test                                                    | What it shows                                                                           |
| ------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| `selftest.R`                                            | Checks on the cleaned data; also on the real data, so run it after every cleaning       |
| `test_cleaning_equivalence.R`                           | The new cleaning step reproduces the old functions, apart from the intended differences |
| `test_estimation_equivalence.R`                         | Standard errors and AMEs reproduce the old functions                                    |
| `test_simulated_likelihood.R`, `test_estimation_core.R` | The fast likelihood matches `mlogit`; estimation and post-estimation work               |
| `test_compute_ames.R`                                   | AMEs against a computation by hand                                                      |
| `test_compare_models.R`                                 | The protocol reaches the right verdict where the truth is known                         |
| `test_sections.R`, `test_deck.R`                        | Every exhibit in every sample, the minimum cell, the deck                               |

`selftest.R` is the only one meant for the real data.

## Earlier code

The code of the first policy brief and of the seminar of 2 November 2026, with all its models, is at the git tag `pre-restructure`. Its results stay in Dropbox, `output_dropbox/`.
