# Energy experts survey: cleaning, summary statistics, mixed logit models

A survey of heating experts (energy advisers, HVAC installers, chimney sweeps) in Germany, twelve
mailing waves. Each expert sees six vignettes of a household and its heating system and recommends a
heating technology. This repository holds the code from the raw LimeSurvey exports to the estimated
models. It holds no data and no results.

## Where everything lives

All paths are set in `config.R`; a machine with other paths puts its three roots in a git-ignored
`config.local.R`.

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
```

Open `energy_expert_survey.Rproj` first, or run `Rscript <file>` from this folder. Requires R 4.6.1.
A re-run reloads every model and result whose formula and data have not changed, so changing one model
re-estimates that model only.

## An estimation script is yours to edit

There is one script per data set in `R/estimation/`. Each goes through the same steps in plain sight:

1. **The sample.** Load the cleaned data, one `filter()` per line.
2. **Summary statistics** of exactly that sample.
3. **`make_regressors()`**, one function that turns source variables into regressors. The same function
   builds the data and the states of the AMEs, so nothing is coded twice.
4. **The models.** You write each formula.
5. **`post_estimation()`** and **`compute_ames()`**, with the reference point and the effects you set per
   call.
6. **`compare_models()`** at the end.

To add a variable: one line in `make_regressors()`, one term in a formula. To study another sample: copy
a script and change its filters and its folder name.

| Function | What it does |
|---|---|
| `fit_mixed_logit()` | Estimates your formula with `mlogit`. Keeps the fixes that a direct call would miss: the order of the random constants, starting values from a nested model, Halton primes by group, the restart after code 3, the re-evaluation at code 2 |
| `post_estimation()` | Standard errors clustered by expert, Krinsky-Robb draws, standard deviations and correlations of the constants, fit and diagnostics. Writes `results.rds` and `results.xlsx` |
| `compute_ames()` | The change in every probability for any change, of attributes or covariates, at any reference point and for any profile, with Krinsky-Robb standard errors and percentile intervals |
| `compare_models()` | The protocol below. Writes `model_comparison.xlsx` |
| `run_summary_statistics()` | All tables, figures and maps of a sample, and the deck |

The fields of `results.rds` are in `docs/results_contract.md`, so that later scripts can build any table or
figure from them. On a model with 113 parameters and 1,500 simulated experts, on an 8-core laptop, a Hessian
takes about 11 minutes and all AMEs about one minute; `mlogit()` itself takes as long as it takes.

## How models are compared

No single measure settles it. Among the seminar's models, the LR test preferred (6pe) to (4pe) while AIC and BIC
did the opposite, and classical and robust tests disagreed on (4p) against (6p). `compare_models()` therefore runs
one fixed protocol over the questions declared at the end of a script, and writes one `decision` sheet with one
row per question:

| Step | Question | Measure |
|---|---|---|
| Validity | Is it a maximum? | Convergence, scaled gradient, positive definite Hessian, log-likelihood at least that of every model it nests |
| Noise | Is the gain more than simulation noise? | Log-likelihood and LR on fresh Halton draws |
| Nested test | Is the extra structure there? | Robust and classical LR, Holm correction across the declared tests |
| Non-nested test | Which is closer to the data? | Vuong test on per-expert log-likelihoods |
| Parsimony | Is it worth its parameters? | AIC, BIC with experts as N, robust TIC |
| Out of sample | Does it predict new experts? | 5-fold cross-validation by expert, for a shortlist |
| Absolute fit | Does it reproduce the data? | Predicted and observed shares |
| Substance | Do the answers change? | Largest change in the AMEs |
| Final model | Is it stable? | Robust Wald test of each block |

**The rule, fixed before results are read.** A larger model is accepted only if it is valid, the robust test rejects
at 5% after Holm, the gain survives fresh draws, and cross-validation improves (BIC falls, if it was not run). If tests
and parsimony disagree, the smaller stays. If all agree but no AME moves by 1 percentage point, the smaller stays as
the main model and the larger is reported as robustness. Terms of direct research interest may stay regardless, which the
table records. The rule proposes; the researchers decide.

## Summary statistics

`run_summary_statistics()` works on any sample: what does not apply, such as response rates in a sub-sample, is
skipped and listed in the deck. Every exhibit exists for all experts and for each expert group.
`summary_statistics.pdf` shows the current version of everything and is rebuilt at the end of every run.
`exhibits.csv` lists each exhibit with its caption and whether it is meant for the main text or the appendix.

Tables are LaTeX fragments: numbers with N, mean, SD and quartiles, answers with N and shares. Figures and maps are
PDFs, in the project's original colours. No statistic rests on fewer than 10 answers, no response rate on fewer than
30 contacts, and no table shows a minimum or maximum.

## Things to know before analysing

- **The arm is not randomised.** Q6a routes each expert into the arm with (`fw`) or without (`nofw`) the
  district-heating option, so comparisons between the two scripts are descriptive. `Fernwärme` is an answer only in
  `fw`.
- **One row per person.** A person who started twice keeps the furthest attempt: 4,176 questionnaires are 3,937 persons,
  of whom 2,551 completed. A token is per survey, so someone invited in two waves counts twice.
- **Response rates by group mix two sources.** The denominator is the mailing list, the numerator the answer to Q2: without
  the token lists, a response cannot be matched to its list.
- **`NA` in `expert_group`** means Q2 was not answered or no category was ticked. Eight completers have no group.
- **`vig_rec` has seven levels:** the six technologies and `Keine Empfehlung`, the pre-registered seventh option. Free
  text without a coded answer counts as `Keine Empfehlung`. `NA` means the vignette was not answered.
- **Soft launch.** Q6 had no "bundesweit" option, and Q7 was a different ranking task. Filter on `is_softlaunch` before
  pooling those.
- **Design.** No unrenovated house (200 kWh/m²) was built in 2000. `att_heat_demand` is the same split as `att_renovation`.
- **Vignettes are nested in experts.** All standard errors are clustered by expert.
- **"AME"** here means the change at a reference expert and vignette, averaged over the random constants only, not over the
  sample of experts.

## The code

```text
config.R                       the roots, every path, the survey's constants
R/setup.R                      packages, figure settings, sources all functions
R/run_cleaning.R               step 1, runs R/cleaning/
R/run_summary_statistics.R     step 2, the full sample
R/estimation/                  step 3, one script per data set; these are the files to edit
R/functions/                   the estimation functions, labels and colours, table and figure helpers
R/summary_statistics/          one function per block of the summary statistics, and the deck
R/geodata/                     builds the postcode lookup, once
tests/                         synthetic data and tests
docs/                          the columns of the cleaned data, the results, notes on the data
```

Every data decision is made once, in `R/cleaning/06_experts_vignettes.R`, with its reason and date in the comments. English
labels and colours are in `R/functions/labels.R`. More on the data, such as how the waves are merged and how the state is derived
from the postcode, is in `docs/data_notes.md`.

## Testing without the real data

`tests/simulate_data.R` writes synthetic data with the real column names and answer levels and invented values: routing,
break-offs, repeat attempts, answers in the wrong unit, typos, and recommendations from a mixed logit with known parameters. A
session with `CLAUDECODE=1` reads and writes only these, in a temporary folder (`config.R`), and never the secure drive.

## Earlier code

The code of the first policy brief and of the seminar of 2 November 2026, with all its models, is at the git tag
`pre-restructure`. Its results stay in Dropbox, `output_dropbox/`.
