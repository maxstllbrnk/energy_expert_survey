# Energieberater vignette survey — data cleaning and summary statistics

Turns the raw LimeSurvey exports of the Energieberater expert survey into three
clean data sets, and describes them in figures and tables. Twelve mailing waves,
including the two soft-launch waves.

The survey shows each expert **six vignettes** describing a household and its
existing heating system, and asks which heating technology they would recommend.

## Where everything lives

The project is split across three locations, by how freely each part can be
shared. All six paths are set at the top of `config.R`.

| | Holds | Shared with |
|---|---|---|
| **Secure** | the raw exports, the LimeSurvey syntax files, and the cleaned respondent-level data | nobody — personal data; moves to an external server (path TBD) |
| **Dropbox** | non-sensitive inputs (`public_data_dropbox/`) and every output (`output_dropbox/`) | the coauthors |
| **Project** | code only — this folder | everyone, via GitHub |

The pipeline **never writes into the project folder**, and respondent-level data
never leaves the secure location.

```
SECURE  (RAW_DIR)                      I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/260909_raw_energy_expert_survey_data
  survey_<id>_R_data_file.csv            the twelve raw exports
  limesurvey-syntax/                     the twelve R syntax exports (metadata)

SECURE  (CLEAN_DATA_DIR)               I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/260909_cleaned_energy_expert_survey_data
  survey_main.{rds,csv,xlsx}             ← written by the pipeline
  vignettes_long.{rds,csv,xlsx}          ← written by the pipeline
  preferences.{rds,csv}                  ← written by R/add_preferences.R
  rc_logit_models/                       ← fitted models of R/analysis/ (they contain the data)

DROPBOX (DROPBOX_DIR)                  …/Heating Transition/analysis_survey
  public_data_dropbox/                   inputs that are safe to share
    LimeSurvey_Versanduebersicht_010726.xlsx    batch overview
    vignetten_kombinationen.xlsx                vignette design matrix
    pc2025_DE_NUTS-2024_v1.0.csv                Eurostat's postcode → NUTS3 list
    geodata/                                    ← downloaded once, see below
      plz_bundesland.csv                          the postcode → state lookup
      provenance.txt                              where it came from
      bkg_vg2500/                                 official state polygons, for maps
      geonames/, raw/                             the downloads themselves
  output_dropbox/                        everything the project produces
    reports_cleaning/                    ← written by the pipeline
      codebook.{xlsx,csv}
      build_report.xlsx
      preferences_report.xlsx
    summary_statistics/                  ← written by R/run_summary_statistics.R
      alle_befragten/, completer/          bericht.html/.pdf, tabellen.xlsx, figures; see "Summary statistics"
    first_policy_brief/                  ← written by R/first_policy_brief/
      bericht_abbildungen.html/.pdf        the figures of the brief; see "First policy brief"
      abbildungen/                         the same figures as PNG and SVG
      preiserwartungen_tests.xlsx          do the Berufsgruppen expect different energy prices?
      rc_logit_<arm>_ref_<outcome>/        (mixed) logit models of the recommendations, one folder per run
    rc_logit_heterogeneity/              ← written by R/analysis/
      rc_logit_<arm>_ref_<outcome>/        mixed logit specifications: results, tables, figures; see
                                           "Heterogeneity of the recommendations"
    descriptives/                        ← written by R/run_descriptives.R
      tables/, figures/, excel/            English tables and figures for talks; see "Descriptives
                                           for talks"

PROJECT (this folder, on GitHub)
  config.R                    the three roots and the paths derived from them
  config.local.R              optional per-machine roots  (git-ignored)
  R/run_cleaning.R            entry point for the cleaning pipeline
  R/cleaning/
    00_functions.R              parsing, naming, shared helpers
    01_codebook.R               DATA SET 1
    02_read_stack.R             read and merge all twelve waves
    03_recode.R                 postcode repair, bundesland, nuts3, "Sonstiges", flags
    04_vignettes.R              DATA SET 2
    05_main.R                   DATA SET 3 + build report
  R/add_preferences.R         the GPS time- and risk-preference scores
  R/prepare_analysis_data.R   shared analysis decisions: Berufsgruppe, last question
                              answered, loads both data sets with the preference scores;
                              the price rules (Q15) and the heating degree days
  R/run_summary_statistics.R  entry point for the summary statistics
  R/summary_statistics/
    00_settings.R               samples, groups, outlier rule, what is plotted, labels
    01_plot_functions.R         the shared look and one function per kind of figure
    02_sample.R                 Berufsgruppe, completion, drop-out, response times
    03_questions.R              one figure per question
    04_vignettes.R              recommendations, times and costs, per Fernwärme arm
    05_preferences.R            time and risk preferences
    06_maps.R                   maps by federal state and by postcode
    07_tables.R                 tabellen.xlsx
    08_report.R                 bericht.html + bericht.pdf: all figures of a sample on one page
    report.css                  the look of that page
  R/first_policy_brief/       everything that belongs to the first policy brief
    00_figures.R                the few figures it needs in another form (sourced by the report)
    01_figure_report.R          its figures, picked from the summary statistics, in one report
    02_kl_divergence_prices.R   do the Berufsgruppen expect different energy prices? (Q17)
    03_rc_logit_with_ame.R      (mixed) logit models of the recommended technology, with AMEs
    03_run_all_specifications.R
                                runs 03_rc_logit_with_ame.R for both Fernwärme arms and two
                                reference outcomes
  R/analysis/                 mixed logit models beyond the policy brief: how do the
                              recommendations differ across experts?
    00_functions_estimation.R   estimation, covariance matrix, AMEs with Krinsky-Robb
                                standard errors, fit
    00_functions_output.R       the quantities of the tables, LR tests, LaTeX tables, figures
    01_run_specifications.R     settings, data and the models; estimates and saves each
    02_output.R                 tables, figures, tests and Excel from the saved results
  R/run_descriptives.R        entry point for the English descriptives for talks
  R/descriptives/
    00_settings.R               sample, rows of the tables, English labels, ZEW colours, sizes
    01_functions.R              statistics, LaTeX tables and the bar charts by group
    02_data.R                   the sample, the price and outlier rules, checks against the models
    03_recruitment.R            response rate by state, invited and responses by group, sample flow
    04_experts.R                summary statistics of the experts; the cleaning
    05_policy_brief.R           price expectations (with tests), barriers, criteria of customers
    06_vignettes.R              the design, recommendations by arm and group, who is in each arm
    07_response_times.R         the questionnaire, its sections and the vignettes
    08_excel_numbers.R          Excel, and the numbers and figure notes for the slides
  R/geodata/
    build_plz_bundesland.R      run ONCE: downloads and builds the postcode lookup
  tests/selftest.R            checks on the cleaned output
  renv.lock                   the exact package versions
```

Analyses go in a folder of their own under `R/`, with their own runner next to
`R/run_cleaning.R` - as `R/summary_statistics/` does with
`R/run_summary_statistics.R`. Keep cleaning and analysis separate: the cleaning
pipeline should not change when an analysis does. Decisions every analysis
shares (the Berufsgruppe, the preference scores) live in
`R/prepare_analysis_data.R`, so all analyses make them the same way. Tables and
figures belong under `output_dropbox/`, each in their own sub-folder next to
`reports_cleaning/`.

## What you get

| File | Written to | Grain | What it is |
|---|---|---|---|
| `survey_main.*` | secure (`CLEAN_DATA_DIR`) | one row per expert | everything outside the vignette module |
| `vignettes_long.*` | secure (`CLEAN_DATA_DIR`) | one row per expert × vignette | recommendation, branch, response times, the nine design attributes, `vignette_id` |
| `preferences.*` | secure (`CLEAN_DATA_DIR`) | one row per expert | time- and risk-preference scores — from `R/add_preferences.R`, not from the pipeline |
| `codebook.xlsx` | Dropbox (`REPORT_DIR`) | one row per variable | question text · LimeSurvey question id · analysis variable name |
| `build_report.xlsx` | Dropbox (`REPORT_DIR`) | — | run diagnostics — **read this after every run** |
| `summary_statistics/` | Dropbox (`SUMMARY_DIR`) | — | figures and tables describing the data — from `R/run_summary_statistics.R`, see [Summary statistics](#summary-statistics) |
| `first_policy_brief/` | Dropbox (`FIRST_POLICY_BRIEF_DIR`) | — | the figures of the first policy brief in one report, the tests of the price expectations and the logit models of the recommendations — from the scripts in `R/first_policy_brief/`, see [First policy brief](#first-policy-brief) |
| `rc_logit_heterogeneity/` | Dropbox (`RC_LOGIT_DIR`) | — | mixed logit specifications on how the recommendations differ across experts: aggregate results, tables, figures — from the scripts in `R/analysis/`, see [Heterogeneity of the recommendations](#heterogeneity-of-the-recommendations) |
| `rc_logit_models/` | secure (`RC_LOGIT_MODEL_DIR`) | one file per specification | the fitted models of `R/analysis/`; they contain the estimation data, so they stay on the secure drive |
| `descriptives/` | Dropbox (`DESCRIPTIVES_DIR`) | — | English LaTeX tables, PDF figures and Excel describing the experts, for talks — from `R/run_descriptives.R`, see [Descriptives for talks](#descriptives-for-talks) |

The two data sets are written as `.rds` (keeps factors, dates and `NA` exactly —
use this for analysis), `.csv` (portable) and `.xlsx` (for looking at).

Join them on **`resp_uid`**.

**Why the codebook goes to Dropbox and the data does not:** the codebook is built
entirely from the syntax files and contains no respondent-level data, so the
coauthors can have it without any access to the secure location. The two data
sets are respondent-level and stay behind.

## How to run

```r
renv::restore()                          # once per computer — install the recorded versions
source("R/geodata/build_plz_bundesland.R")  # ONCE ever — downloads the postcode geodata
source("R/run_cleaning.R")               # build all three data sets
source("tests/selftest.R")               # checks on the result
source("R/run_summary_statistics.R")     # figures and tables, see "Summary statistics"
source("R/first_policy_brief/01_figure_report.R")   # the figures of the first policy brief
source("R/first_policy_brief/02_kl_divergence_prices.R")    # tests of the price expectations
source("R/first_policy_brief/03_run_all_specifications.R")  # logit models of the recommendations; slow
source("R/run_descriptives.R")           # English tables and figures for talks; after 02_kl_divergence_prices.R
```

From a terminal, in the project folder:

```bash
Rscript R/run_cleaning.R && Rscript tests/selftest.R
```

**Before the first run**, open `config.R` and set the three roots at the top —
`RAW_DIR`, `CLEAN_DATA_DIR` and `DROPBOX_DIR`. Everything else is derived from
them. Use forward slashes: R reads `\` in a string as an escape character, so a
pasted Windows path is a syntax error.

`config.R` checks all six paths before the pipeline starts and names the ones it
cannot find, so a wrong root fails immediately rather than halfway through a run.

Requires **R 4.6.1** (the version recorded in `renv.lock`).

### Coauthors: `config.local.R`

Your Dropbox sits at a different path, and editing a tracked file to run the
pipeline means a merge conflict every time. Instead, create `config.local.R` in
the project root with just the roots that differ. Just edit the following R-code 
and save it as a script.

```r
# =============================================================================
# config.local.R  -  per-machine roots. Git-ignored, never leaves this computer.
# =============================================================================
# Sourced by config.R, which derives every other path from whatever is set here.
#
# ONLY root assignments belong in this file. Do not copy config.R into it: the
# copy would source itself and R would fail with "evaluation nested too deeply".
#
# The three roots you can override are RAW_DIR, CLEAN_DATA_DIR and DROPBOX_DIR.
# Set only the ones that differ from config.R; leave the rest out.
# =============================================================================

# The secure project drive. Same values as config.R at the moment - this file
# exists so that changing drives (or a coauthor's machine) needs no edit to a
# tracked file.
RAW_DIR        <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/260909_raw_energy_expert_survey_data"
CLEAN_DATA_DIR <- "I:/Projekte/Stiftungs- und Industrieprojekte/SFB UniMA/Daten/Umfrage Energieexperten/260909_cleaned_energy_expert_survey_data"

DROPBOX_DIR    <- "C:/Users/msk/Dropbox/Heating Transition/analysis_survey"
```

`config.R` sources it if it exists and derives everything from whatever it sets.
It is git-ignored, so it never leaves your machine.

Without access to the secure location you cannot run the cleaning pipeline, but
everything in `output_dropbox/` is produced for you.

The geodata under `public_data_dropbox/geodata/` is shared the same way: it is
downloaded once, by one person, and everyone else just has it.

### When the secure data moves to the server

Repoint `RAW_DIR` and `CLEAN_DATA_DIR` at the server. Nothing else in the
project changes — no script contains a path of its own.

## Working with renv

`renv` pins every package to an exact version so the pipeline gives the same
result on any machine and in a year's time.

Three pieces:

- **`renv.lock`** — the manifest: R 4.6.1 plus 129 packages with exact versions.
  This is the contract. **Committed to git.**
- **`renv/library/`** — the packages themselves, private to this project.
  Machine-specific, so **not committed**.
- **A global cache** under `AppData/Local/R/cache/`, shared across your renv
  projects. The project library mostly *links* into it, so a package another
  project already has costs no download.

`.Rprofile` contains one line, `source("renv/activate.R")`, which runs whenever
R starts in this folder and puts the project library first on the search path.
Nothing installed elsewhere on the machine can change what this project loads.

### The four commands

| Command | When |
|---|---|
| `renv::status()` | is the lockfile in sync with what I'm using? |
| `renv::snapshot()` | I added a package → record it in the lockfile |
| `renv::restore()` | new machine, or I just pulled → install what the lockfile says |
| `renv::update()` | deliberately move to newer versions |

`snapshot()` and `restore()` are opposites: snapshot writes library → lockfile,
restore writes lockfile → library.

### Adding a package

```r
install.packages("mlogit")   # installs into renv/library, not the system library
renv::snapshot()             # record it in renv.lock
```

Then commit `renv.lock`. **Forgetting `snapshot()` is the one mistake that
matters** — it works for you and breaks for everyone else.

### Updating packages

Only when you choose to. The point of `renv` is that you do not have to stay
current; an update you did not need is pure risk.

```r
renv::update()
source("R/run_cleaning.R")
source("tests/selftest.R")   # confirm nothing broke
renv::snapshot()             # lock the new versions in
```

Do this in its own commit, so a regression is easy to bisect.

Before committing, `renv::status()` should say *"No issues found"*.

### Notes

- `renv` finds dependencies by scanning `.R` files for `library()` calls, so a
  package used only in a comment or a deleted script will not be recorded.
- `renv` records the R version but does not install R. A coauthor on an older R
  may see `restore()` warn or fail.

## Things to know before analysing

- **The Fernwärme branch is between-subject.** `Q6a` routes each expert into the
  `fw` or `nofw` arm; `Fernwärme` is only available as an answer in `fw`. The
  arm is in `vig_arm`, and `arm_source` says whether it was observed or
  reconstructed from `Q6a`.
- **Nothing is dropped.** All 4,176 respondents are in `survey_main`, including
  break-offs. Define your sample explicitly with `is_complete`,
  `reached_vignettes` or `n_vignettes_answered`.
- **`vig_rec` has seven levels.** The six technologies plus `Keine Empfehlung`,
  the pre-registered seventh option (LimeSurvey stores it as `-oth-`); the free
  text is in `vig_rec_other`.
- **Free text with no coded answer counts as `Keine Empfehlung`.** In a handful
  of rows LimeSurvey saved the typed text into `vig_rec_other` but left the
  answer column empty, so the recommendation arrived as `NA` even though the
  expert had answered. A row carrying free text *is* the seventh option, so
  `04_vignettes.R` sets `vig_rec` to `Keine Empfehlung` wherever `vig_rec` is
  `NA` and `vig_rec_other` is not — currently **5 rows, 2 experts**. The count
  is in the build report (*of which free text with no coded answer*), and the
  original text stays untouched in `vig_rec_other`. `NA` in `vig_rec` therefore
  means the vignette really was not answered.
- **One expert has an arm conflict: `855854_429`.** They apparently saw the
  same vignette twice — once **with** and once **without** district heating —
  and answered in both Fernwärme branches, which the between-subject design
  rules out. They carry `arm_conflict == TRUE` (the only respondent who does),
  the pipeline warns about them on every run, and they keep one row per branch
  rather than a single arm, so `vig_arm` is the branch of that row and not a
  property of the expert. They **did not finish the survey**
  (`is_complete == FALSE`), so they are already outside the completer sample;
  if you analyse the **full** sample, consider excluding them with
  `filter(!arm_conflict)`.
- **`Time*` variables are not response times.** `Time1…Time5BBBB` are the
  time-preference staircase and are named `tp_*`; `Risk*` becomes `risk_*`.
  Response times are `time_*` (per question) and `grouptime_*` (per page). The
  scores built from them live in `preferences.*` — see below.
- **Soft-launch caveats.** `Q6` had no "bundesweit" option and `Q7` was a
  different ranking task. See the `harmonisation` sheet of the build report and
  the `softlaunch_diffs` sheet of the codebook. Filter on `is_softlaunch` before
  pooling those variables.
- **Data-quality issues are flagged, not fixed.** Implausible birth years,
  shares that do not sum to 100 and unusable postcodes are listed in the
  `data_quality` sheet of the build report and left exactly as the respondents
  typed them. How to treat them is an analysis decision.

## How waves are merged

Columns are matched to the raw exports **by question code**, never by position.
The CSV headers carry the code (`"Q24[SQ005]. Haben Sie …"`), so a wave with a
different number of columns still lines up.

This matters: the two soft-launch waves have 415 and 416 columns where the main
waves have 417. `Q24a` does not exist in either, wave 334335 has an extra sixth
rank on `Q7`, and the numeric `groupTime<id>` values differ in *every* wave — so
group times are matched on their German label, which is identical across all
twelve.

The column separator is read off each file's header rather than assumed. The
LimeSurvey export dialog decides it, and it has already changed once between
export batches (semicolon → comma); assuming the wrong one reads a whole file as
a single column and fails much later, looking like a naming bug.

The self-test proves the alignment: all nine design attributes must jointly
resolve to a row of `vignetten_kombinationen.xlsx`, which cannot happen by
chance if columns are misaligned. It currently matches **100% of rows in both
wave groups**.

## Time and risk preferences

`R/add_preferences.R` turns the two staircases (`tp_*`, `risk_*`) into scores.
It is **not** part of the cleaning pipeline: the score is a rank within a sample,
so standardising it makes every respondent's number depend on who else is in the
data — and cleaning has to give the same answer every time it runs.

Two ways to use it:

```r
source("R/add_preferences.R")        # defines the function, does nothing else
dat <- add_preference_scores(dat, standardise_on = dat$is_complete)
```

```bash
Rscript R/add_preferences.R          # writes preferences.* for all respondents
```

Prefer the first. Standardising on the sample you actually estimate on is what
the GPS convention means; the written file uses "everyone who answered at least
one question", which is a sensible default and nothing more.

Five columns per measure, `tp_` and `risk_`:

| | |
|---|---|
| `*_score` | midpoint of the respondent's cell, in (0, 1). 0 = most impatient / most risk averse. **Not** sample-dependent |
| `*_z` | the same, standardised over `standardise_on` |
| `*_cell` | the cell itself, ordered from least to most |
| `*_levels` | how many of the five choices were answered |
| `*_complete` | did the respondent go as deep as their branch allows? |

**What this is.** The quantitative staircase component of the Global Preference
Survey (Falk, Becker, Dohmen, Enke, Huffman & Sunde 2018, QJE, §II.B). Each
respondent makes up to five binary choices, each one setting the terms of the
next, closing in on their point of indifference. Five choices sort them into one
of 32 ordered cells.

**What it is not.** The published GPS measure combines the staircase with a
Likert self-assessment, weighted 71/29 for patience and 47/53 for risk taking.
This questionnaire has no such item, so these scores are the staircase alone —
a valid ordering of respondents, but not comparable to published GPS country
means.

**Two things to know before using them.**

- **Not everyone gets five levels.** LimeSurvey never generated `Time5ABBA` or
  `Time5ABBB`, so a respondent whose first three time answers were A, B, B stops
  after four — 178 of them. Others broke off. A shorter path gives a wider cell,
  and its midpoint is exactly the average of the two cells it contains, so
  nobody is pushed in either direction. Use `*_levels` to drop shallow paths;
  at depth 1 the score is only a coin-flip between 0.25 and 0.75.
- **`*_z` depends on who is in the frame.** Between "everyone who answered" and
  "completers only" the scores move by up to 0.07 sd. Small, but it is a choice,
  so make it explicitly.

## Bundesland

`bundesland` and `bundesland_source` are in `survey_main`, derived from `plz`.
The questionnaire never asks for the state.

The lookup is a plain CSV in Dropbox, built **once** by
`R/geodata/build_plz_bundesland.R`. Nothing in the pipeline downloads anything
and no spatial package is needed at run time — the cleaning run does a
`left_join` and stops there.

### Where the data comes from

| Source | For | Licence |
|---|---|---|
| [BKG **VG2500**](https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/) — Bundesamt für Kartographie und Geodäsie, the federal mapping agency | the official state keys and names, and the polygons for maps | dl-de/by-2-0 |
| [**GeoNames** postal codes DE](https://download.geonames.org/export/zip/DE.zip) | the postcodes, with the official AGS district key | CC-BY 4.0 |

BKG publishes an MD5 next to each archive and the build script checks the
download against it. Full provenance — URLs, vintage, checksum, licence, method
— is written to `geodata/provenance.txt` next to the data.

### How the state is derived

The first two digits of the AGS (*Amtlicher Gemeindeschlüssel*) **are** the
official federal state key, so the state is read straight off the postcode's
district — no geometry at all. Checked against a point-in-polygon join on the
BKG polygons, the two agree on **99.80%** of GeoNames' 23,297 rows, and the
exceptions are all border cases. The AGS route is both the more accurate one and
the one with no dependencies.

What is deliberately **not** used is GeoNames' own state column: it holds 23
spellings for 16 states ("Bayern" *and* "Bavaria", "Sachsen" *and* "Saxony",
"Land Berlin", "Lower Saxony") and agrees with the AGS-derived state only 90.3%
of the time. Every state name comes from the BKG table instead.

### `bundesland_source`

Postcodes are Deutsche Post routing areas, not administrative units, so the
assignment is not always clean. `bundesland_source` says how each one was
reached, and any part of it can be excluded:

| | |
|---|---|
| `plz` | in the lookup, and the postcode lies in one state only |
| `plz_border` | in the lookup, but the postcode straddles a state line — assigned to the state holding most of it (32 of 10,812 postcodes do this) |
| `plz_prefix` | not in the lookup, but every postcode sharing its first three digits is in one state. That rule reproduces the right state 98.3% of the time (leave-one-out) and only unanimous prefixes are used |
| `unmatched` | five digits, but no such postcode exists in Germany — a typo, left as `NA` rather than guessed |
| `no_postcode` | Q0 was blank or unusable |

On the current data: 99.0% of valid postcodes match exactly, the prefix rule
recovers most of the rest, and a handful of typos stay `NA`. The counts are in
the `data_quality` sheet of the build report.

### Maps

`BUNDESLAND_SHP` (in `config.R`) points at the BKG state polygons, so a
choropleth needs no download:

```r
library(sf)
states <- st_read(BUNDESLAND_SHP) |> dplyr::filter(GF == 9)   # GF 9 = land area
```

`sf` is in `renv.lock` because the summary statistics draw maps with it
(`R/summary_statistics/06_maps.R`); the cleaning pipeline itself does not need it.

### Rebuilding

Only when you want a newer vintage. It overwrites the lookup, so re-run the
cleaning pipeline afterwards.

```bash
Rscript R/geodata/build_plz_bundesland.R refresh
```

## NUTS3

`nuts3` and `plz_in_nuts_list` are in `survey_main`, looked up from `plz` in
Eurostat's postcode → NUTS correspondence (GISCO; postcodes 2025, NUTS 2024),
`public_data_dropbox/pc2025_DE_NUTS-2024_v1.0.csv` (`PLZ_NUTS3_FILE` in
`config.R`). The list puts every postcode in exactly one NUTS3 region (Kreis or
kreisfreie Stadt), so there is no border or prefix rule: a postcode is either in
the list or `nuts3` is `NA`.

The list holds only geographic postcodes. P.O.-box and large-customer postcodes
are valid, keep `plz` and `bundesland`, but get no NUTS3 region;
`plz_in_nuts_list` is `FALSE` for them. Of the 10,812 postcodes in the GeoNames
lookup, 2,502 are not in Eurostat's list. The count among respondents is in the
`data_quality` sheet of the build report (counts only: a large-customer
postcode can identify a firm).

24 postcodes are in Eurostat's list but not in GeoNames. Some look recent, but
several look miscoded, because Eurostat puts them in a different state than
their postcode area: 04728, 04730, 04731 and 04760 (Saxon postcodes) in
Nordrhein-Westfalen, 06280 (Sachsen-Anhalt) in Schleswig-Holstein, and 57320
and 57350 (Siegen area) in the Saarland. A respondent with one of these gets
the Kreis, and so the regional data, of the other state.

`bundesland` still comes from GeoNames and BKG (above). The first three
characters of `nuts3` (NUTS1, see `BUNDESLAND_NUTS1` in
`R/cleaning/00_functions.R`) are also the state. Of the 8,310 postcodes in both
lookups, 7 are put in different states: 4 that straddle a state line, and 22113,
26954 and 64754, which GeoNames places in one state only. So `nuts3` and
`bundesland` can disagree for those, for the codes above and for states from
the prefix rule. The build report counts the disagreements, and
`tests/selftest.R` fails if one is not among these known cases.

## Summary statistics

`R/run_summary_statistics.R` describes the cleaned data in figures and tables:
one figure per question, the vignette recommendations, drop-out, response
times, time and risk preferences, and maps. Titles are the German question
text.

```r
source("R/run_summary_statistics.R")     # a few minutes; run R/run_cleaning.R first
```

It reads the cleaned data from the secure location and writes only figures and
aggregate tables, to `output_dropbox/summary_statistics/`. Each run replaces the
previous output, so nothing stale is left behind.

### What you get

Every figure is produced for **two samples × four groups**:

| Sample folder | Who |
|---|---|
| `alle_befragten/` | every expert in `survey_main`, including break-offs |
| `completer/` | submitted questionnaires only (`is_complete`); no drop-out figures |

| Group folder | Who |
|---|---|
| `alle_berufsgruppen/` | everyone in the sample |
| `shk_handwerk/`, `schornsteinfeger/`, `energieberater/` | one Berufsgruppe, see below |

Inside each group folder the figures are sorted by questionnaire section:

| Folder | Contents |
|---|---|
| `01_stichprobe/` | Berufsgruppe, completion, vignettes answered, drop-out |
| `02_bearbeitungszeit/` | time for the whole questionnaire and per section |
| `03_karten/` | experts per federal state and per postcode |
| `04_unternehmen/` | Q0a–Q6a |
| `05_vignetten/` | recommendations (overall, by position, by each vignette attribute, by Berufsgruppe), response time per vignette, expected cost by technology — always per Fernwärme arm |
| `06_energiepreise/` | Q15–Q18b |
| `07_heiztechnologien/` | Q11b–Q11c (SHK firms only) |
| `08_praeferenzen/` | time and risk preferences: the first choice as asked, and the score |
| `09_dienstleistungen_markt/` | Q8, Q10a–b, Q19a–c, Q7, Q28 |
| `10_person/` | Q20–Q24 |

Continuous variables are histograms showing the mean (solid line) and median
(dashed); categorical variables are bar charts of shares. Every caption states n,
how many outliers were removed, and which sample and group the figure shows, so
a figure copied into a slide still says what it is.

Ranking questions (Q7, Q28) get two figures. The first shows the share of
experts naming each option, stacked by rank. The second (`…_gewichte.png`)
turns each expert's ranking into weights that sum to 1: of k options named,
rank r gets (k + 1 − r) / (k(k + 1)/2) — 3/6, 2/6, 1/6 for three options,
2/3, 1/3 for two, 1 for one — and options not named get 0. A bar is the mean
weight over the experts who ranked at least one option, so the bars sum to
100 % and an expert who names fewer options counts as much as one who names
all. The rule is `ranking_weights()` in `R/summary_statistics/01_plot_functions.R`.

**To look through everything at once, open `bericht.html` or `bericht.pdf`** in
a sample folder: every figure of that sample on one page, one chapter per group,
sorted by questionnaire section, with links to jump between them and the rules
below at the top. The HTML shows the PNG files next to it, so it only works
inside its folder; the PDF is self-contained and is the one to send around. The
PDF is printed by a headless Chrome or Edge (the paths in `REPORT_BROWSERS`,
`00_settings.R`). Where neither is installed, only the HTML is written — open it
and print it to PDF from the browser.

`tabellen.xlsx` in each sample folder holds the numbers behind the figures, all
groups in one table (column `gruppe`): mean, sd, quartiles and `n_entfernt` for
continuous variables, counts and shares for categorical ones, the mean ranking
weights (`rangfolge_gewichte`), recommendation,
cost and response-time statistics per arm, and a sheet `hinweise` stating the
rules below.

A question shown to only some experts produces no figure for a group that never
saw it (fewer than `MIN_N` = 10 answers): Q11b was only shown to SHK firms, Q8
and Q10 only to Energieberatung and Schornsteinfeger firms.

### Berufsgruppe

Q2 allows several ticks, so each expert is assigned to **one** group, by priority:

1. **SHK-Handwerk** — ticked *Sanitär-, Heizungs-, Klimatechnik*
2. **Schornsteinfeger** — ticked *Schornsteinfeger*, but not SHK
3. **Energieberater** — ticked *Energieberatungsbüro* **or** *Architektur- /
   Bauingenieurbüro / sonstiges Ingenieurbüro*, **or** *Sonstiges*, but neither trade

The trades take priority over energy advice: an SHK firm that also offers energy
advice counts as SHK-Handwerk, a chimney sweep who also advises as
Schornsteinfeger. Architecture and engineering offices count as Energieberater.
Experts who ticked only *Sonstiges* also count as Energieberater, those who skipped Q2
are `NA`, and appear only in `alle_berufsgruppen/`.

The rule is `add_berufsgruppe()` in `R/prepare_analysis_data.R`. Use that
function in every analysis rather than deriving the groups again.

### Outliers

Free numeric answers contain typos that stretch a histogram's axis until the
distribution is a single bar. Before a continuous variable is plotted **or**
summarised, two rules are applied, in this order:

1. **`VALID_RANGE`** — impossible values are dropped: birth years outside
   1920–2010 and training years outside 1930–2026 (the bounds the build report
   flags).
2. **`TRIM_QUANTILES`** — of the remaining values, those below the 0.5th and
   above the 99.5th percentile are dropped.

Both are at the top of `R/summary_statistics/00_settings.R`;
`TRIM_QUANTILES <- c(0, 1)` switches trimming off. Every histogram says in its
caption how many values were removed, the mean and median it shows are those of
the remaining values, and `tabellen.xlsx` applies the same rule. The data are
never changed. The preference scores are bounded by construction and are not
trimmed.

The rule catches typos, not unit errors: around 1 % of the energy prices look
like euro rather than cent per kWh (0.11 instead of 11), more than the 0.5 %
the quantile rule cuts. How to treat those is an analysis decision.

### Drop-out

LimeSurvey's `meta_lastpage` counts pages including those hidden by routing, so
the same page number stands for different questions for different experts. It
is plotted as recorded (`abbruch_lastpage.png`), but the other drop-out figures
use the response times instead: a question has a time exactly when the expert
submitted its page, so **the last question with a time is the last question
answered**. `add_last_question()` in `R/prepare_analysis_data.R` adds it as
`last_question`, based on the questionnaire order in `QUESTIONNAIRE`.

- `abbruch_nach_frage.png` — break-offs by the last question answered
- `verbleib_nach_abschnitt.png` — the share of all experts who got at least as
  far as each section

On the current data the two largest groups of break-offs answered Q6a last
(219 — they left when the vignettes began) and the cost question last (116 —
they left at the energy prices).

### Changing things

| To change | Edit |
|---|---|
| which questions are plotted, axis labels, bin widths | `HISTOGRAMS`, `BARS`, `MULTIPLE_CHOICE`, `RANKINGS` in `00_settings.R` |
| titles, subtitles, shortened answer labels | `TITLES`, `SUBTITLES`, `SHORT_LABELS` in `00_settings.R` |
| outlier rule, minimum n | `VALID_RANGE`, `TRIM_QUANTILES`, `MIN_N` in `00_settings.R` |
| samples or groups | `SAMPLES` in `00_settings.R`; the Berufsgruppe rule in `R/prepare_analysis_data.R` |
| colours, figure size | the end of `00_settings.R` |
| how one kind of figure looks | its function in `01_plot_functions.R` — all figures of that kind follow |
| the report page: section headings, browser used for the PDF, layout | `SECTION_LABELS`, `REPORT_BROWSERS` in `00_settings.R`; `report.css` |

### Using the prepared data in an analysis

```r
source("R/prepare_analysis_data.R")                 # also loads config.R
dat <- load_analysis_data(completers_only = TRUE)
dat$main        # one row per expert, plus berufsgruppe, last_question, tp_* and risk_* scores
dat$vignettes   # one row per expert x vignette, plus berufsgruppe
```

The preference z-scores are standardised on the sample that is loaded.

## Descriptives for talks

`R/run_descriptives.R` writes the descriptive tables and figures of a talk, in
English and sized for the 16:9 slides of the economics-latex layout: who was
invited and who answered, who the experts are, the characteristics that enter
the mixed logit models, the topics of the policy brief by group, the
recommendations with and without the district-heating option, and the response
times. Unlike the summary statistics, it writes LaTeX tables as well as
figures, and only the exhibits a talk needs.

```r
source("R/first_policy_brief/02_kl_divergence_prices.R")   # the tests it reports
source("R/run_descriptives.R")                             # a minute
```

It reads the cleaned data and the sampling frame (both secure) and writes only
aggregates, to `output_dropbox/descriptives/`. Each run empties `tables/`,
`figures/` and `excel/` there and nothing else, so slides can sit next to them.

### Sample and cleaning

- **Who:** the completed questionnaires of the three Berufsgruppen, in both
  arms: the experts of the mixed logit models, and those over whom
  `R/analysis/01_run_specifications.R` takes the reference prices. Only the
  response rates count everyone invited and everyone who completed, as in
  `R/ruecklauf/` (persons: an e-mail address once, an invitation once).
- **Energy prices (Q15):** in cent per kWh of the energy carrier, as Q15
  asked (district heating per kWh of heat), not converted to heat. By the
  rules of the models, `clean_reported_prices()` in
  `R/prepare_analysis_data.R`: euro read as cent, oil per litre and pellets
  per kg converted at the questionnaire's rules of thumb (1 litre of heating
  oil about 10 kWh, 1 kg of pellets about 5 kWh), answers more than three
  times above or below the median not usable. The notes of the tables say
  so; `tab_desc_price_cleaning.tex` counts every step.
- **Other continuous answers** (year of birth, year of training, employees,
  households advised): the outlier rule of the summary statistics
  (`VALID_RANGE`, then the 0.5% lowest and highest), applied **once to the
  pooled sample**, so that an answer is kept or removed in every column alike
  (`outliers_to_na()`; `tabellen.xlsx` of the summary statistics trims within
  each group instead). `tab_desc_other_cleaning.tex` counts what was removed.
- **Shares of the technologies** among the recommendations (Q10b) and the
  installations (Q11b): an expert gave shares if they sum to more than 0; a
  technology left blank then counts as 0 (Max, 2026-10-29). Used as reported,
  also where they do not sum to 100, and not trimmed (they lie between 0 and
  100; the script stops if one does not); `tab_desc_other_cleaning.tex`
  counts the experts whose shares do not sum to 100. The means are over the
  firms, not shares of all recommendations or installations. The
  questionnaire does not ask how many systems a firm installed.
- **Response times** are shown as medians and quartiles, not trimmed.
- **Small cells:** no statistic of fewer than `MIN_N` = 10 answers, medians
  and quartiles included (shown as --); a state of the map needs 30 invited and
  10 completed. Counts are shown at any size: N, the sample flow and the
  cleaning steps.

### What you get

| Part | Figures (`figures/`) | Tables (`tables/`) |
|---|---|---|
| Recruitment | `fig_desc_response_rate_map`, `fig_desc_composition` | `tab_desc_recruitment` (invited, started, completed by group), `tab_desc_sample_flow` (from the invitation to the estimation sample) |
| Who answered | | `tab_desc_experts` (the expert and the firm): N, mean, SD, quartiles; shares for answer options; `tab_desc_advice` (households advised, heat pumps among the recommendations and the installations), `tab_desc_technologies` (every technology among them) |
| Characteristics in the models | | `tab_desc_prices_heating` (prices per kWh of the energy carrier and degree days; below them district heating in the service area and heating at home side by side; the expected rises are in `tab_desc_price_expectations`); the same means and shares with a column per group: `tab_desc_prices_groups` (prices, degree days) and `tab_desc_heating_groups` (district heating, heating at home); `tab_desc_price_cleaning`, `tab_desc_other_cleaning` |
| Policy brief | `fig_desc_price_expectations`, `fig_desc_barriers_renewable`, `fig_desc_customer_criteria` | `tab_desc_price_expectations`, `tab_desc_barriers_renewable`, `tab_desc_customer_criteria` |
| Vignettes | `fig_desc_recommendations` | `tab_desc_design` (attributes and levels), `tab_desc_recommendations` |
| Appendix | `fig_desc_barriers_district_heating`, `fig_desc_barriers_conventional`, `fig_desc_vignette_times` | the four summary tables per group (`tab_desc_<table>_<group>`), `tab_desc_price_tests`, `tab_desc_price_pairs`, `tab_desc_barriers_district_heating`, `tab_desc_barriers_conventional`, `tab_desc_customer_weights`, `tab_desc_arms` (who is in each arm), `tab_desc_times_sections`, `tab_desc_times_vignettes` |

`excel/descriptives.xlsx` has every number unrounded, and leaves out what the
tables leave out (empty cells). `tables/numbers_desc.tex`
defines macros for the text of the slides (`\descExperts`, `\descResponseRate`,
...) and a note for every figure (`\descNoteResponseMap`, ...). In a deck:

```latex
\input{\tablepath/numbers_desc.tex}            % once, in the preamble
\begin{frame}{Response rate by state}
  \centering\includegraphics{fig_desc_response_rate_map.pdf}
  \slidenote{\descNoteResponseMap}
\end{frame}
\begin{frame}{Who answered}
  \input{\tablepath/tab_desc_experts.tex}
\end{frame}
```

The tables are complete floats with caption and label, as those of the mixed
logit models (they use the same `table_float()`), and need `booktabs` and
`threeparttable`. The figures have no titles inside: the frame title says what
they show. Every table and figure was checked to fit a 16:9 frame with its
note. Groups, technologies and the arms have the names of the regression
output; the figures use the ZEW chart colours.

**All of them in one deck:** `slides/261029_descriptives/slides.tex` in
`descriptives/` (Dropbox), next to `tables/` and `figures/`, shows every table
and figure in the order above, the per-group tables, tests and response times
as backup slides. After a rerun of `R/run_descriptives.R`, compile it with
`pdflatex slides` twice in that folder. Its frame titles say in words what the
run of 29 October 2026 shows; check them when the data change.

### Checks

The script stops if the price rules, the reference prices or the estimation
sample differ from those saved with any model of the deck (`MODELS_IN_DECK` in
the runs of `MODEL_RUNS`, `00_settings.R`): the descriptives and the models
would then rest on different data. A model of the list without saved results
(not estimated yet) gives a warning. It also stops if the tests of the price
expectations rest on other answers (rerun `02_kl_divergence_prices.R`), if an
answer has no English label or a table shows an answer option that is not in
the data, and if a state of the data is no polygon of the map. The workbook
leaves out what the tables and figures leave out.

### Things to keep in mind

- **The response rate of a group is approximate.** The invited are grouped by
  the list they were drawn from, the respondents by Q2, and nothing links the
  two (see `R/ruecklauf/02_link.R`).
- **Repeat completions.** An invitation (token) that completed twice counts
  once among the persons but twice among the completed questionnaires, and so
  twice in the models. `tab_desc_sample_flow.tex` counts them.
- **The arms are not random.** Q6a sets the arm, so the recommendations with
  and without the district-heating option differ in the experts too
  (`tab_desc_arms.tex`).
- **Q10 and Q11b were routed** by Q2: Q10 to firms offering energy advice or
  chimney sweeping, Q11b to HVAC firms. A group's numbers rest on those of its
  experts who were asked; the tables of the energy advisers and the chimney
  sweeps have no installations.

### Changing things

The sample, the rows of the tables, the English labels, colours and sizes are
all in `R/descriptives/00_settings.R`; the rules shared with other analyses
(Berufsgruppe, prices, `PEXP_RISE`, `HDD_YEAR`) in `R/prepare_analysis_data.R`,
the outlier rule and `MIN_N` in `R/summary_statistics/00_settings.R`.

## First policy brief

`R/first_policy_brief/` holds the scripts of the first policy brief, and
`output_dropbox/first_policy_brief/` what they write: a report with the figures
the brief uses, [tests of the price expectations](#price-expectations-by-berufsgruppe)
and [logit models of the recommendations](#logit-models-of-the-recommendations).
The first script collects the figures the brief uses into one report:

```r
source("R/first_policy_brief/01_figure_report.R")   # seconds; run R/run_summary_statistics.R first
```

Most figures are copied from `output_dropbox/summary_statistics/`, so they are
exactly the figures of the summary statistics. Re-run the script whenever those
change. The few the brief needs in another form are drawn afresh on every run by
`00_figures.R`, with the same plot functions and rules, and go into
`abbildungen/` like all the others:

| Figure | What is different from the summary statistics |
|---|---|
| `Q10b_SQ001_adv_share_hp`, `Q11b_WP_mix25_hp` | bins of 10 instead of 5 percentage points. Most experts answered in multiples of 10, so with bins of 5 every other bar is nearly empty |
| `empfehlungen_nach_baujahr_ohne_1970_unsaniert` | without the vignettes of a 1970 house that was never renovated (200 kWh/m²). The design has no unrenovated 2000 house, so otherwise the two years differ in renovation as well as in age |

| File | What it is |
|---|---|
| `bericht_abbildungen.html`, `.pdf` | the selected figures, one chapter per section of the brief, with the rules behind them at the top |
| `abbildungen/` | the same figures as PNG and SVG, named by their number in the report: `abb_4_07_….png` and `abb_4_07_….svg` are Abbildung 4.7 |

The SVGs keep their text as text, so it can be edited in a layout program. They
are written next to every PNG by `save_figure()`, in the summary statistics as in
`00_figures.R`. Summary statistics from before that have none, and the script
then stops and asks for a new run of `R/run_summary_statistics.R`.

**To add or remove a figure**, edit `FIGURES` at the top of the script. Each line
is one figure: its section and its path inside `summary_statistics/` (printed
under every figure of a `bericht.html` there), or `eigene/<group>/<file>` for a
figure `00_figures.R` draws. Delete a line, or put a `#` in front of it, to drop
that figure. The section headings and their order are in
`SECTIONS`, just above. If a path does not exist, the script stops before writing
anything and names the closest file in that folder.

Figures are numbered in the order of the list, so adding or removing one renumbers
the figures after it. The file name ends with the Berufsgruppe; a figure of all
experts only gets `_alle_berufsgruppen` when the list also has that question for
a Berufsgruppe, as for Q19a.

Each run empties `abbildungen/` and nothing else, so other files for the brief
can sit next to it in `first_policy_brief/`.

### Price expectations by Berufsgruppe

`02_kl_divergence_prices.R` asks whether SHK-Handwerk, Schornsteinfeger and
Energieberater expect different price developments for the five fuels of Q17
(gas, heat-pump electricity, district heating, heating oil, pellets), answered
on a five-point scale from *mehr als 10% niedriger* to *mehr als 10% höher*.

```r
source("R/first_policy_brief/02_kl_divergence_prices.R")   # seconds
```

The sample is the completers, without experts who have no Berufsgruppe; an
expert who skipped a fuel drops out for that fuel only. Each fuel gets three
tests, from the broadest question to the most specific:

| Test | Question | Effect size | Holm correction across |
|---|---|---|---|
| G-test of independence | are the full answer distributions different? Ignores the order of the answers | Cramér's V | the 5 fuels |
| Kruskal–Wallis | does at least one group answer systematically higher or lower on the ordered scale? | ε² = H / (n − 1) | the 5 fuels |
| pairwise Wilcoxon rank-sum | which pairs of groups differ, and which of the two answers higher? | `prob_1_higher` = P(group 1 higher) + ½ P(same answer) | the 15 comparisons (5 fuels × 3 pairs) |

The G statistic is 2N times the mutual information between group and answer —
the KL divergence of the joint distribution from independence, hence the name
of the script. Some expected counts are below 5, so besides the chi-squared
p-value the script simulates 10,000 tables with the same margins (`r2dtable`)
and the Holm correction uses that simulated p-value; the seed is fixed, so it is
reproducible.

The output is `first_policy_brief/preiserwartungen_tests.xlsx`:

| Sheet | Contents |
|---|---|
| `readme` | the three tests, their effect sizes and the Holm correction, explained |
| `anteile` | share of each answer, per fuel and group |
| `gruppen` | n, mean and median score, share expecting a price increase |
| `g_test`, `kruskal_wallis`, `paarvergleiche` | one sheet per test; `significant_holm` marks a Holm-adjusted p-value below 0.05, `direction` says which group of a pair tends to answer higher |

Read the tests alongside `anteile`: with samples this large, small differences
are significant. The same experts answer all five fuels, so compare groups
within a fuel rather than p-values across fuels. The mean score treats the
answers as equally spaced and is descriptive only; the tests do not assume it.
The fuels, answer labels, pairs, significance level and number of simulated
tables are set at the top of the script.

### Logit models of the recommendations

`03_rc_logit_with_ame.R` models which technology an expert recommends for a
vignette (`vig_rec`) as a function of the seven vignette attributes: the
couple's age, income, current heating, how soon it must be replaced, year of
construction, heat demand and heat distribution. Every attribute gets one
coefficient per technology, relative to a reference outcome.

The sample is the completers of one Fernwärme arm (`SELECTED_FW_ARM`): in `fw`
the outcomes are the six technologies and *Keine Empfehlung*, in `nofw` the same
without Fernwärme. Each expert makes up to six choices. Three models are
estimated with `mlogit`:

| Model | What it is |
|---|---|
| `mnl` | multinomial logit |
| `mixed` | mixed logit: the constants of the technologies are normally distributed across experts, independently of each other; an expert keeps the same draw for all six vignettes. 1,000 Halton draws per expert |
| `mixed_corr` | the same, with correlated constants (full covariance matrix) |

What the script reports:

- **Standard errors** clustered by expert (sandwich estimator, with the
  small-sample factor G/(G − 1)). The mixed logits need a numerical Hessian,
  which is most of the runtime. Standard deviations and correlations of the
  constants get delta-method standard errors; a standard deviation gets no test
  against zero, because zero lies on the boundary of the parameter space.
- **Average marginal effects** at a reference vignette — every attribute at its
  first level in `ATTRIBUTE_LEVELS`: a couple in their mid-40s, 65,000 € gross a
  year, a gas heating that must be replaced in the next years, built 1970,
  120 kWh/m², radiators. Each AME is the change in the probability of a
  recommendation when one attribute is switched, averaged over 1,000 draws of
  the random constants. Standard errors and percentile intervals by
  Krinsky–Robb (1,000 draws of the coefficient vector); a delta-method
  comparison is printed as a check.
- **Fit**: McFadden R², a likelihood-ratio test against a model with constants
  only, and for the mixed logits one against the multinomial logit.

Along the way the script checks itself and stops if a check fails: its
covariance matrix of the constants and its multinomial-logit probabilities must
reproduce those of `mlogit`, the AMEs must sum to zero across the outcomes, and
the numerical Hessian must be positive definite.

```r
source("R/first_policy_brief/03_rc_logit_with_ame.R")        # one run, with the arm and reference set at its top
source("R/first_policy_brief/03_run_all_specifications.R")   # all four runs; slow
```

`03_run_all_specifications.R` runs the main script four times, for the arms
`fw` and `nofw` with *Keine Empfehlung* and *Wärmepumpe* as the reference
outcome. Each run gets its own environment and passes its settings in
`RUN_SETTINGS`; a run that fails is recorded in the printed table of runs, and
the loop moves on to the next.

Each run writes to its own folder, `first_policy_brief/rc_logit_<arm>_ref_<outcome>/`,
e.g. `rc_logit_fw_ref_keine_empfehlung/`, so no run overwrites another. All
labels are in German.

| Folder | Contents |
|---|---|
| `tabellen/` | `tab_rcl_mnl.tex`, `tab_rcl_mixed.tex`, `tab_rcl_mixed_corr.tex`: one LaTeX table per model. The document needs `\usepackage[T1]{fontenc}`, `\usepackage[ngerman]{babel}`, `booktabs` and `threeparttable` |
| `abbildungen/` | `abb_ame_<outcome>.svg`: the AMEs of the three models with 95 % Krinsky–Robb intervals, one figure per outcome |
| `excel/` | `rc_logit_ergebnisse.xlsx`: coefficients, marginal effects and fit, one sheet each |
| `modelle/` | the three fitted models and their cluster-robust covariance matrices as `.rds`, so they can be reloaded without estimating again |

These figures are SVG only and drawn by the script itself, not with the plot
functions of the summary statistics. `01_figure_report.R` empties only the
top-level `abbildungen/`, so it leaves these folders alone.

The number of draws, the arm, the reference outcome and the attribute levels
are set at the top of `03_rc_logit_with_ame.R`. `mlogit` is attached in
`config.R`, so every script that sources it needs the package installed —
`renv::restore()` takes care of that.

## Heterogeneity of the recommendations

`R/analysis/` takes the logit models of the policy brief further: how do the
recommendations differ across experts, and in particular across Berufsgruppen?
Every model is a mixed logit of the recommended technology (`vig_rec`) on the
vignette attributes whose constants (alternative-specific constants) vary
across experts, normal and correlated. It is estimated by simulated maximum
likelihood with `mlogit` (Train 2009, chapters 6 and 10). All outputs are in
English.

```r
source("R/analysis/01_run_specifications.R")   # estimates the models; slow
source("R/analysis/01_run_term_effects.R")     # effects of all terms; slow
source("R/analysis/02_output.R")               # tables, figures, tests; a few minutes
```

| Script | What it does |
|---|---|
| `00_functions_estimation.R` | the functions: the variables of the models (shared by both `01` scripts), estimation (with its starting values), covariance matrix clustered by expert, probabilities and AMEs with Krinsky–Robb standard errors, fit |
| `01_run_specifications.R` | settings, data and the models; estimates each, saves the model to the secure drive and its aggregate results to Dropbox |
| `01_run_term_effects.R` | the effects of the prices, the expert characteristics and the interactions of the attributes on the probabilities, from the saved results (see [Effects of all terms](#effects-of-all-terms-on-the-probabilities)) |
| `00_functions_output.R` | the functions of the output: the quantities every table shows the same way, the LR tests, the LaTeX tables and the figures |
| `02_output.R` | settings and labels; the models compared, then each model: LaTeX tables, PDF figures, an Excel workbook and numbers for the slides, from the saved results (see [Tables and figures](#tables-and-figures)) |

### The models

| | Name | Constants | Further regressors | Prices |
|---|---|---|---|---|
| (2) | `m2` | one set for all experts, full covariance matrix | — | — |
| (4) | `m4` | one set for all experts, full covariance matrix | Berufsgruppe | — |
| (4i) | `m4i` | as (4) | Berufsgruppe, Berufsgruppe × attributes | — |
| (6i) | `m6i` | one set per Berufsgruppe, a covariance matrix within each | Berufsgruppe × attributes | — |
| (4p) | `m4p` | as (4) | Berufsgruppe | yes |
| (6p) | `m6p` | as (6i) | — | yes |
| (4ip) | `m4ip` | as (4) | Berufsgruppe, Berufsgruppe × attributes | yes |
| (4pa) | `m4pa` | as (4) | Berufsgruppe, attribute × attribute | yes |
| (4pd) | `m4pd` | as (4) | Berufsgruppe | yes, and price × heat demand |
| (4pl) | `m4pl` | as (4) | Berufsgruppe | log prices |
| (4pe) | `m4pe` | as (4) | Berufsgruppe, heating degree days, heat pump at home | yes, and expected price rise |
| (6pe) | `m6pe` | as (6i) | heating degree days, heat pump at home | as (4pe) |

(4pe) is the main specification, and (6pe) its robustness check (Max, 29
October 2026); (4) is the benchmark without the expert's prices and
characteristics. (2), (4i), (6i), (4p), (6p), (4ip) and the other variants of
(4p) are kept as alternatives. (6), (4) with one set of constants per
Berufsgruppe, was dropped (Max, 29 October 2026). The numbers are those of the
earlier version, whose other specifications (uncorrelated constants, (1), (3)
and (5), and prices with one coefficient per Berufsgruppe) are in commit
`93010be`.

All have the eight attribute dummies as regressors, with one fixed coefficient
per technology, as in the policy brief. In (4) the Berufsgruppe shifts the
means of the constants (reference category: Energieberater, October 2026; the
earlier version and the policy brief used the SHK-Handwerk), with one covariance
matrix for all. In (6i), (6p) and (6pe) every Berufsgruppe has its own means
and covariance matrix. Every expert belongs to one Berufsgruppe, so the covariances between
two groups' constants never enter the likelihood: they are set to zero as a
normalisation, not as an assumption (the Cholesky elements that link two
groups are fixed through `constPar`).

(4i) and (6i) let the effect of every attribute differ by Berufsgruppe: 16
interactions (the SHK-Handwerk and the Schornsteinfeger times the eight
attribute dummies), each with one coefficient per technology, the difference
to the Energieberater; the attribute coefficients themselves are then the
Energieberater's. To use another reference group, list the other two groups
in `GROUP_SHIFTS` and `GROUP_X_ATTRIBUTES` (section 3 of
`01_run_specifications.R`). In (6i) the Berufsgruppen share no parameter, so it is a
separate model per Berufsgruppe, estimated together. A Berufsgruppe's
interactions rest on its experts alone, so those of a small Berufsgruppe are
estimated imprecisely (the script prints the experts per Berufsgruppe).

(4p), (6p) and (4ip) add the price per kWh of each technology, from the energy
prices the expert reported (Q15). Answers in euro, per litre (oil) or per kg
(pellets) are converted, and answers more than three times above or below the
median of their fuel count as not reported (the rules and bounds are in
section 1 of `01_run_specifications.R`). Gas, oil and pellets are priced per
kWh of fuel, district heating per kWh of heat. The heat pump's price is the
electricity price divided by the COP, a price per kWh of heat; the hybrid
heating's is the weighted average of the gas price (weight `HYBRID_GAS_SHARE`)
and the heat pump's. Prices are centred at the mean over the experts; a price
not reported is set to the mean and gets a dummy. Each technology's price
enters its own utility (also the reference outcome's), with one coefficient
for all technologies, so the prices come before the `|` of `mlogit`'s formula;
the dummies have one coefficient per technology. The AMEs are for an expert
who reported the mean prices.

(4pa), (4pd), (4pl) and (4pe) are variants of (4p) (October 2026), each
changing one thing:

- **(4pa)** adds interactions of the attributes, each a product of two
  attribute dummies with one coefficient per technology: heat demand × build
  year, heat demand × underfloor heating, underfloor heating × build year. No
  vignette has a house built in 2000 with 200 kWh/m², so heat demand × build
  year exists for 70 kWh/m² only. The AMEs change one attribute at the
  reference vignette (1970, 120 kWh/m², radiators), where every interaction is
  0: the interactions show only in the coefficients.
- **(4pd)** adds the price times the dummies of 70 and 200 kWh/m². The price
  coefficient is then the effect of the price at 120 kWh/m², and the two
  others the differences at 70 and 200. If the price mattered through the
  running cost, price × heat demand, they would be −0.42 and +0.67 times the
  price coefficient (70/120 − 1 and 200/120 − 1). Estimated from `mlogit`'s
  default starting values on 28 October 2026, it ended below (4p); it now
  starts from (4p), as do (4pa) and (4pe) (see
  [Starting values](#starting-values)).
- **(4pl)** has the log price instead of the price: the log of the price minus
  the log of the mean price, so that, as in (4p), an expert who reported the
  mean prices has 0 and a price not reported counts as the mean. For the heat
  pump the COP cancels in logs; the hybrid's log price is the weighted
  average of those of gas and the heat pump (the log of a weighted geometric
  mean). (4pl) is not nested in (4p): it has as many parameters, so the
  log-likelihoods compare directly.
- **(4pe)** adds three expert characteristics. Two shift the mean constants
  like the Berufsgruppe, with one coefficient per technology: the heating
  degree days of the expert's NUTS3 region (Eurostat `nrg_chddr2_a`, 2025, in
  1,000 degree days; an expert without `nuts3` gets the mean, without a
  dummy, as they are few, Max, October 2026), and whether a heat pump heats
  the expert's own home (Q20, `heat_pump_at_home`; "Sonstiges" and no answer
  count as no heat pump). Each coefficient of the heat pump at home is the
  difference it makes to a technology's utility relative to the heat pump,
  also for "Keine Empfehlung", as for the degree days. The third has one
  coefficient, like the price: whether the expert expects a technology's
  price to rise in the next five years (Q17, "bis zu 10 % höher" or "mehr als
  10% höher"; `PEXP_RISE`), in the utility of every technology. The hybrid's
  expected rise is the weighted average of those of gas and electricity. A
  blank in Q17 is set to the mean and gets a dummy per technology, as a price
  not reported. Until 1 November 2026 the heating at home entered as whether
  the technology heats the expert's own home, with one coefficient for all
  six technologies (`own_heating_match`). That mixed different situations, a
  heat pump an expert chose and a gas heating that came with the building, so
  (4pe) and (6pe) now have the heat pump at home only, with one coefficient
  per technology; how to treat the other heatings at home is open (Max, 1
  November 2026; for a few hours the same day, one coefficient in the
  utility of the heat pump, which forced the same shift on every other
  technology).

**(6pe)** is (4pe) with one set of constants per Berufsgruppe, each with its
own means and covariance matrix, as (6p) is to (4p): the robustness check of
the main specification (Max, 29 October 2026). It starts from (4pe).

**Why the 0/1 variables are centred.** The heat pump at home and the expected
rise enter as the dummy minus its share among the experts (for Q17, among
those who answered), e.g. 1 − 0.3 = 0.7 and 0 − 0.3 = −0.3 if 30% of the
experts have a heat pump at home. The mean of a 0/1 variable is its
share of ones, so this centres it like the prices. It does not change its
coefficients or the log-likelihood: the share times a coefficient is the same
for every expert who answered, and the constant of the technology takes it up
(for the heat pump at home, each technology's constant the share times its
own coefficient). A blank in Q17 is 0, not minus the share, and its dummy
takes up the difference. It changes only which expert the constants, baseline
probabilities and AMEs describe. `01_run_specifications.R` computes them with
every term of the `prices` element, the degree days and the heat pump at home
at 0, an expert with average characteristics, as in (4p).
That expert exists only on paper (e.g. 0.95 of "expects higher gas prices"),
and the probabilities at average characteristics are not the average of the
experts' probabilities; the gap has not been measured. **What the outputs
report for (4pe) and (6pe)** is another expert: the mean prices and degree
days, no expected price rise and no heat pump at home (Max, 30 October 2026;
the heat pump since 1 November 2026, before that none of the six technologies
at home).
`01_run_term_effects.R` computes the AMEs, baseline probabilities and mean
constants at that expert from the saved estimates and draws, without
re-estimating (the model and its log-likelihood do not depend on the
centring), and `02_output.R` uses them in place of those of
`01_run_specifications.R` in every table, figure and Excel sheet (see "Effects
of all terms" below). The degree days are centred at their
mean for the same reason; all shares and means are taken over the experts of
the three Berufsgruppen in both arms, as for the prices, and saved with the
results (`expert_reference`).

### Writing a model

Each model is one entry of `MODELS` in section 3 of
`01_run_specifications.R`, e.g. (6pe):

```r
m6pe = list(
  label = "Random constants by occupational group, correlated, with prices and expert characteristics",
  constants = GROUP_CONSTANTS,
  regressors = c(ATTRIBUTES, EXPERT_CHARACTERISTICS),
  prices = c(PRICES, EXPERT_TERMS),
  start_from = "m4pe"
)
```

| Element | Values |
|---|---|
| `label` | name in tables and figures |
| `constants` | `"(Intercept)"`: one constant per technology for all experts; `GROUP_CONSTANTS` (`is_shk`, `is_schornsteinfeger`, `is_energieberater`): one per technology and Berufsgruppe |
| `regressors` | variables of the decisions, each with one coefficient per technology |
| `prices` | the terms that differ between the technologies, each with one coefficient for all of them: `PRICES` (each technology's price, and the dummies for a price not reported), `PRICES_X_DEMAND`, `PRICES_LOG` (instead of `PRICES`), `EXPERT_TERMS`, or `character(0)` |
| `start_from` | the model whose estimates are the starting values: for constants by Berufsgruppe, the same model with one set of constants; otherwise a model with the same constants that this one nests (see [Starting values](#starting-values)) |

The building blocks `ATTRIBUTES`, `GROUP_SHIFTS`, `GROUP_CONSTANTS`,
`GROUP_X_ATTRIBUTES`, `ATTRIBUTE_INTERACTIONS` and `EXPERT_CHARACTERISTICS` are
written out above the list, so a regressor is added or removed by adding or
deleting a line. Most regressors are 0/1 variables that
`add_regressors()` (`00_functions_estimation.R`) builds from the attributes and the
Berufsgruppe: one dummy per attribute level besides the reference level
(`REFERENCE_LEVELS` and `ATTRIBUTE_DUMMIES` in section 1), the group dummies,
the interactions `is_<group>_x_<dummy>` and the interactions of two attributes
`<dummy>_x_<dummy>`; the degree days `hdd_c` are an expert characteristic
(section 2.1). Before the first estimation, the
script checks every model: its columns exist and are linearly independent, its
prices are price terms, and the model it starts from has one set of constants
and, if this one has too, is nested in it.
To run only some models, list them in `MODELS_TO_RUN`; the others keep their
saved results.

### Adding a regressor

How depends on what kind of variable it is. In every case, its name then goes
into the `regressors` of a model, or into a building block.

**A variable built from the vignette attributes or the Berufsgruppe**, e.g. an
interaction of low income and urgent replacement. Create it in
`add_regressors()` (`00_functions_estimation.R`), below the attribute dummies:

```r
data$income_low_x_urgent <- data$income_low * data$replacement_urgent
```

and add `"income_low_x_urgent"` to the `regressors` of a model. Nothing else is
needed: the vignettes of the AMEs and of `01_run_term_effects.R` go through the
same function, so they get the variable too.

**An expert characteristic**, e.g. from `survey_main` (a preference score, the
firm size) or regional data joined by `nuts3`:

1. Section 2.1: add the column to `experts`, which is joined to the decisions:
   ```r
   experts <- experts_all %>%
     select(resp_uid, all_of(price_columns), expert_var)
   ```
   It must be numeric. A categorical variable becomes one 0/1 dummy per
   category besides a reference category, created in the same step.
2. Section 2.4 (a): add it to `model_variables`, so that a missing value stops
   the script with a clear message (otherwise the rank check in section 3 stops
   with an unclear one). Whether to drop experts with a missing value is a
   decision for the sample restrictions in section 2.2.
3. Section 3: add it to the `regressors` of the models, e.g. as a building
   block `EXPERT_CHARACTERISTICS <- c("expert_var")`. Like the Berufsgruppe
   shifts, it gets one coefficient per technology.
4. Section 4.4: give it a value in the vignettes of the AMEs, which are
   evaluated for an expert with that value:
   ```r
   vignettes_at_mean <- ame_vignettes %>%
     mutate(hdd_c = 0, heat_pump_at_home = 0, expert_var = 0)
   ```
   For a continuous variable centred in step 1, 0 is the sample mean. Which
   value to use is a substantive choice. Without it the script stops with
   "Not in the data: expert_var". `01_run_term_effects.R` needs the same value
   in `reference_state()`, and a change of its own if its effect should be
   computed (`term_changes()`, with a label in `term_effect_labels()` of
   `00_functions_output.R`).

**A variable that differs between the technologies within one vignette**, like
the price: a column in the long data (section 2.5) that holds each
technology's value in its rows, as `long_terms()` (`00_functions_estimation.R`)
does for the price with `PRICE_VARIABLES`. List it in the model's `prices`
element, which takes any such term, each with one coefficient for all
technologies.

Things to keep in mind:

- (6i), (6p) and (6pe) start from (4i), (4p) and (4pe). A regressor added to a
  model by Berufsgruppe needs the same regressor in the model it starts from;
  otherwise the script stops with "No starting value for ...". The other way
  round is fine: extra parameters of the common model are ignored.
- (4pa), (4pd) and (4pe) start from (4p), so they must keep all of its
  regressors and prices: a model with one set of constants only adds terms to
  the model it starts from (checked before the first estimation).
- The checks before the first estimation (columns exist and are not collinear,
  e.g. with the Berufsgruppe dummies) and after each estimation (every
  coefficient is used in the AMEs) cover new regressors as well.
- To estimate only the new or changed models, list them in `MODELS_TO_RUN`,
  with a model's `start_from` model before it.
- The output script needs a label for each new regressor and term that
  differs between the technologies (`02_output.R` stops and names any without
  one), and a table for it if none of its tables shows it yet.

### Starting values

`mlogit` starts the means at multinomial-logit estimates and every element of
the Cholesky factor L of the covariance matrix at 0.1, also those off the
diagonal. In every Berufsgruppe's block of (6p), that is a set of constants
with small variances and correlations close to one, far from the estimates.
(6i), (6p) and (6pe) therefore start from (4i), (4p) and (4pe): each
Berufsgruppe gets the constants of the common model plus its shift, and the
common model's Cholesky factor as its block of L; the attributes,
interactions and prices keep their values (`start_from_common()`). At these
values (6p) predicts exactly what (4p) predicts, for every expert, so it
starts at the maximum of (4p) and can only improve on it.

A model with one set of constants that adds terms to another, e.g. (4pd) to
(4p), starts from that model's estimates, with the added parameters at 0
(`start_from_nested()`): at these values the two are the same model, so the
estimation starts at the log-likelihood of (4p) and cannot end below it.
Without them, (3pg) on 22 October and (4pd) on 28 October 2026 stopped below
the model they nest, although `mlogit` reported convergence (code 1 or 2).

In both cases the script prints the log-likelihood at the starting values and
that of the model it starts from, and stops if they differ, e.g. because the
saved model comes from an earlier run on other data or with other draws; that
model must then be estimated again, before the other in `MODELS_TO_RUN`. If
the estimation stops with code 3 (no better point along the search direction),
it restarts from the last iterate, at most three times. On simulated data (340
experts, 100 draws per expert), the model by Berufsgruppe without prices (then
(6)) with `mlogit`'s default starting values took 95 iterations and stopped
1.1 log points below the maximum that the new starting values reached in 53.

### Sample

All models are estimated on the same sample, so that their log-likelihoods can
be compared: the completers of one Fernwärme arm (`SELECTED_FW_ARM`) with one
of the three Berufsgruppen. This drops "Keine Zuordnung" and the experts who
did not answer Q2, which the policy brief kept. Vignettes without a
recommendation are dropped too, visibly. The script prints the experts and
vignettes in the arm, those dropped for lack of a Berufsgruppe, the number of
vignettes without a recommendation, and the estimation sample by Berufsgruppe.
A missing value in a variable of the models stops the script instead of
shrinking the sample: restrict the sample visibly in section 2.2.

### Halton draws

`mlogit` gives every random coefficient its own Halton prime, in the order of
the coefficients: 2, 3, 5, ... With one set of six constants per Berufsgruppe,
the last group would get the primes 41 to 61, whose Halton sequences cover the
unit cube unevenly unless very many draws are used (Train 2009, section
9.3.3). An expert's utility contains only the constants of their own
Berufsgruppe, so the primes start again at 2 for each Berufsgruppe. Every
expert then gets the same draws in (4p) and (6p), which is what makes the
starting values above reproduce the log-likelihood of (4p).

The order of the random constants follows the order of the outcomes. The data
in `mlogit`'s format list the outcomes alphabetically, as `dfidx()` does with
the wide data of the policy brief and of the earlier version of the script,
with the reference outcome moved to the front. Another order would give each
constant another prime and change the Cholesky parameters, so estimates would
differ from earlier runs by simulation error.

### What the script saves

For each model, `results/<model>.rds` in
`output_dropbox/rc_logit_heterogeneity/rc_logit_<arm>_ref_<outcome>/`
(aggregates only):

- **Coefficients** with standard errors clustered by expert (sandwich with
  G/(G − 1); the Hessian by numerical differences of `mlogit`'s gradient), z
  and p; the covariance matrix and the inverse Hessian.
- **Krinsky–Robb draws**: `n_draws_kr` draws of the parameters from N(θ̂, V).
  The same draws give the standard errors of everything that is a nonlinear
  function of the parameters.
- **Standard deviations and correlations of the constants**, with
  Krinsky–Robb standard errors and percentile intervals, like every other
  quantity that is a nonlinear function of the parameters (decided October
  2026; the earlier version used the delta method). Unlike delta-method
  intervals, the percentile intervals stay within the bounds (a standard
  deviation above 0, a correlation between −1 and 1) and show skewness, which
  matters most in the smaller Berufsgruppen. Whether the constants vary at all
  (standard deviation 0) is a question for a likelihood-ratio test, not for a
  standard error: 0 lies on the boundary of the parameter space.
- **AMEs** at the reference vignette, with Krinsky–Robb standard errors, normal
  and percentile intervals, and the probabilities at the reference vignette. A
  model with Berufsgruppe has them for each Berufsgruppe, (2) for all experts.
  In (4i) and (6i) they include the interactions of the profile's Berufsgruppe.
  The probabilities at every Krinsky–Robb draw are saved too, so that
  differences between Berufsgruppen can be tested without rerunning.
- **Fit**: log-likelihood, McFadden R², AIC and BIC (with the number of experts
  as N).
- **Convergence** (since 29 October 2026): the code, the iterations, the
  largest element of the gradient, the model it started from and the
  log-likelihood there.
- **Reference values**: the mean prices and the rules for the reported prices
  (`price_reference`, `price_settings`), and the means and shares at which the
  expert characteristics of (4pe) and (6pe) are centred (`expert_reference`).

Every run, also one that estimates no model, saves `results/data_reference.rds`:
the same reference values, the 25th and 75th percentiles of the reported
prices and of the degree days, and how the terms of section 2.5 are built, for
`01_run_term_effects.R`.

The fitted model contains the estimation data, one row per expert and vignette,
so it goes to the secure drive instead,
`rc_logit_models/rc_logit_<arm>_ref_<outcome>/<model>.rds` next to the cleaned
data. The formula is saved in the results as text, because a formula object
carries the environment it was made in.

### Checks

The script stops before the first estimation if an attribute has a value that
is neither its reference level nor one of its dummies (a different spelling
would otherwise count as the reference level), a variable of the models has a
missing value, a recommendation is never chosen, in a run with the heat pump
at home a recommendation is never chosen by the experts with one or never by
those without (it prints these counts), a price dummy is the same for every
expert, an answer of Q17 or Q20 is not one of the known answers, a usable
price is at or below 0 (its log), a NUTS3 region has more than one row of
degree days or an expert's region has none, a model's columns or its price
terms are linearly dependent, a model with one set of constants does not nest
the model it starts from, or
`MODELS_TO_RUN` lists a model before the model it starts from. A dummy for a
blank in Q17 that is the same for every expert is left out of the models, with
a message. For each model it stops,
records the error and moves on to the next if the starting values do not
reproduce the log-likelihood of the model it starts from, the model has not converged or
does not use every choice situation, its parameters are not one per column of
the design matrix and technology plus the prices and the elements of L (the
AMEs would ignore a coefficient), the Cholesky factor that the AMEs use does
not reproduce `mlogit`'s covariance matrix of the constants, a re-evaluated
gradient differs from the stored one, the numerical Hessian is not positive
definite, or the probabilities do not sum to one. It prints the formula, the
number of parameters, the convergence code, the largest element of the
gradient and the log-likelihood. Convergence code 2 (successive
log-likelihoods within tolerance) counts as converged; read the largest
gradient element for (6i), (6p) and (6pe). Neither code proves a maximum:
`02_output.R` marks an LR statistic below 0, a larger model that ended below
the smaller one.

### Runtime

The draws are set at the top of `01_run_specifications.R`: 1,000 Halton draws
per expert, 100,000 draws for the AMEs and 5,000 Krinsky–Robb draws, so that
each 2.5% tail of a percentile interval rests on 125 draws. The estimation and
the numerical Hessian grow with the number of parameters and with the number
of experts times the draws per expert; the Hessian of a model with K
parameters needs 2K evaluations of the gradient. The Krinsky–Robb step grows
with the number of profiles and with both kinds of draws: one evaluation of
the AMEs took about 0.4 seconds with 100,000 draws, about 1.5 hours for a
model with three Berufsgruppen. Since 29 October 2026, the probabilities of
all vignettes come from two matrix products per draw, about six times faster
on simulated data, with the same results (largest difference 3e-15). Each
variant of (4p) takes about as long as (4p); (4pa) a little longer, as its
Hessian has 24 more parameters. For a first look, lower
`n_draws_ame` and `n_draws_kr`, and list only some models in `MODELS_TO_RUN`.

### Effects of all terms on the probabilities

The coefficients are in utility units relative to the heat pump, per unit of
their term (a cent per kWh, 1,000 degree days, a yes instead of a no), and
their scale differs between models, so they do not compare across terms. The
AMEs of `01_run_specifications.R` change one attribute of the reference
vignette. `01_run_term_effects.R` changes the other terms, one at a time, and
gives the change in the probability of each recommendation, in percentage
points, with Krinsky–Robb standard errors and percentile intervals from the
saved draws: comparable across the terms of a model, and across models with
the same base state (below).

Every change starts from a **base state** (`TERM_EFFECTS_BASE`): the reference
vignette and an expert of the profile's Berufsgruppe with the mean prices and
degree days. With `"neutral"`, the setting, the expert of (4pe) and (6pe)
expects no price to rise and has no heat pump at home (`neutral_state()`), so
that each of these changes switches on one answer (Max, 30 October 2026; the
heat pump since 1 November 2026). With `"average"`, the expected rises and
the heat pump at home stay at their shares (e.g. 0.95 for "expects higher gas
prices"), the reference state of `01_run_specifications.R`. In the other
models the two settings are the same.

All figures and tables of a model must describe the same expert, or the
baseline probabilities of the term-effect figures differ from those of the
AME and comparison figures (they did on 30 October 2026: chimney sweeps 36%
against 42% for the heat pump in (4pe)). So at the neutral base state the
script also saves, from the same draws:
- the AMEs and the probabilities at every Krinsky–Robb draw at that expert,
  laid out as `ame_table` and `probability_draws` of `01_run_specifications.R`
  (`base_ame_table`, `base_probability_draws`);
- what the expected rises and the heat pump at home add to each technology's
  utility there (`base_constant_shift`; for the heat pump at home, minus its
  share times the technology's coefficient), which `02_output.R` adds to the
  mean constants; the script checks that the constants with it reproduce the
  probabilities at the base state.

`02_output.R` puts these in place of the AMEs and probabilities of
`01_run_specifications.R`, so the baseline probabilities, AMEs, group
differences, mean constants, comparison figures and term-effect figures of
(4pe) and (6pe) all describe the neutral expert; it checks that the term
effects start from the baseline probabilities of the other figures, records
the expert of each model in the Excel sheet `models` (`expert_state`), and
leaves out a neutral file without these (from before the evening of 30 October
2026). The coefficients do not depend on the base state, only their
translation into percentage points. On simulated data (1 November 2026), the
term effects changed between the two settings by up to 1.6 points (median about
0.1; the heat pump at home by up to 0.8), the AMEs by up to 1.9 (median 0.2), the
baseline probabilities by up to 4.6, and the dummies for a blank in Q17 (Excel
only) by up to 4.2 points.

| Change | From | To |
|---|---|---|
| price of a fuel | its 25th percentile among the experts | its 75th percentile (Max, 29 October 2026) |
| price of a fuel, in (4pd) | as above, in a house of 70 or 200 kWh/m² | |
| price not reported | reported, at the mean | not reported |
| expected rise of a fuel (Q17) | no rise expected for this fuel, the other fuels as in the base state | a rise expected for this fuel |
| no expectation for a fuel | the base state | blank (coded as in `01_run_specifications.R`: expected rise 0, dummy 1) |
| heat pump at home (Q20), (4pe) and (6pe) | no heat pump at home | a heat pump at home |
| heating degree days | the 25th percentile among the experts with a region | the 75th |
| interaction of two attributes (4pa) | one attribute at its other level | both |

The heat pump at home moves the utility of every technology relative to the
heat pump, each with its own coefficient, like the degree days. The script
also handles a model with the match of every heating at home
(`own_heating_match`, (4pe) and (6pe) until 1 November 2026; none at the
moment): there each technology at home switches on from none of the six
technologies at home, the others as in the base state, and the heat pump
switches from a gas heating at home to a heat pump instead
(`OWN_HEATING_BEFORE`), which moves the heat pump's and the gas heating's
utilities, so its `p_before` in the sheet `term_effects` is the probability
with a gas heating at home.

A fuel's change moves every technology that uses it, as in the models:
electricity the heat pump and the hybrid heating, gas the gas and the hybrid
heating. Everything else stays at the base state. The script builds its vignettes
with the same functions as `01_run_specifications.R` (section 2 of
`00_functions_estimation.R`: `add_regressors()`, `add_alternative_prices()`,
`long_terms()`, `design_by_profile()`), reads the percentiles from
`data_reference.rds`, and checks that the reference values
there are those of each model's results (same data) and that it reproduces
the probabilities and AMEs of `01_run_specifications.R` at the estimates (same
coding and draws). It saves `results/<model>_term_effects.rds` for the models
of `MODELS_TO_COMPUTE`, with the coefficients they rest on: `02_output.R`
leaves out effects of an earlier estimation of the model.

A model with 22 changes, such as (4pe), takes up to two hours with 100,000
draws and 5,000 Krinsky–Robb draws (on the real data, (6pe) took about 1.7
hours on 30 October 2026, with 27 changes before the heat pump at home
replaced the six technologies at home); models can run in separate R sessions, each with its own
`MODELS_TO_COMPUTE`.

Price equivalents (a coefficient divided by the price coefficient) would be
free of the scale too, but the price is the price the expert reported, not a
designed attribute: errors in the reports pull its coefficient towards zero
and inflate every equivalent.

### Tables and figures

`02_output.R` reads the saved results of one arm and reference outcome
(`SELECTED_FW_ARM`, `REFERENCE_OUTCOME`), aggregates only, never the fitted
models, and writes into the same run folder. Its outputs follow the story of a
talk: first the models compared (which model, and do the results depend on
it), then each model.

| Folder | File | What |
|---|---|---|
| `tables/` | `tab_rcl_fit.tex` | the models (what differs by occupational group, prices, expert characteristics) and their fit: parameters, log-likelihood, AIC, BIC, McFadden R²; without the variants of (4p) but the main specification |
| | `tab_rcl_lr_tests.tex` | likelihood-ratio tests between nested models, classical and robust; without the variants of (4p) |
| | `tab_rcl_variants.tex` | (4p) and its variants: the change, the fit, and the LR test against (4p) |
| | `tab_rcl_coefficients_<technology>.tex` | the attribute effects on one technology across the models whose attribute effects are the same for all groups |
| | `tab_rcl_prices.tex` | the price coefficients of the models with prices: the price, the log price (4pl), the price × heat demand (4pd) |
| | `tab_rcl_prices_not_reported.tex` | the dummies for a price not reported, of the same models |
| | `tab_rcl_<model>_constants.tex` | mean (A) and standard deviation (B) of the constants, by group |
| | `tab_rcl_<model>_coefficients[_<group>].tex` | the regression table: effects of the attributes, and in the models with prices the price coefficient; one table per group in the models with interactions |
| | `tab_rcl_<model>_baseline.tex` | baseline probabilities (at the reference vignette), by group |
| | `tab_rcl_<model>_price_by_demand.tex` | (4pd): the price coefficient at each heat demand, against a price effect proportional to the heat demand, with a Wald test |
| | `tab_rcl_<model>_interactions.tex` | (4pa): the interactions of the attributes |
| | `tab_rcl_<model>_expert.tex` | (4pe): the expert characteristics (degree days, heat pump at home, expected price rise) |
| | `numbers_rcl.tex` | macros for the text of the slides: sample, draws, reference vignette, mean prices |
| | `tab_rcl_<benchmark>_constants_slide.tex` | for the main slides: the constants of `BENCHMARK_MODEL` ((4)), one row per technology, the means of the groups and the standard deviation, narrow enough to sit beside a figure |
| | `tab_rcl_spread_slide.tex` | for the main slides: the standard deviations of the constants across the models of `GAP_MODELS` with one covariance matrix for all groups |
| `figures/` | `fig_rcl_<model>_baseline.pdf` | baseline probabilities, by group |
| | `fig_rcl_<model>_ame_<outcome>.pdf` | AMEs by group, one figure per outcome |
| | `fig_rcl_<model>_correlations[_<group>].pdf` | correlations of the constants, one heat map per covariance matrix |
| | `fig_rcl_comparison_baseline.pdf` | probabilities across the models of `COMPARISON_MODELS`, a panel per group |
| | `fig_rcl_comparison_ame_<outcome>_<group>.pdf` | AMEs across the same models |
| | `fig_rcl_<model>_term_effects_<outcome>.pdf` | the effects of all terms (`01_run_term_effects.R`), by group, for the models of `TERM_FIGURE_MODELS`, with the baseline at the base state in the subtitle and the values of each change in the labels (e.g. "Price of electricity: 24 → 30 ct/kWh"), next to the attribute AMEs at the same expert; the slide version of `MAIN_MODEL` on the heat pump shows the changes of the terms only |
| | `fig_rcl_<model>_ame_heat_pump_slide.pdf`, `fig_rcl_<benchmark>_baseline_slide.pdf`, `fig_rcl_comparison_baseline_slide.pdf` | for the main slides, narrow (3.2 × 2.8 in), with room for notes or a table beside them: the heat-pump AMEs of `MAIN_MODEL` and of `BENCHMARK_MODEL`, the baseline probabilities of `BENCHMARK_MODEL`, and the heat-pump probability by group across the models of `BASELINE_SLIDE_MODELS` ((4) and (4pe)) |
| | `fig_rcl_<model>_term_effects_heat_pump_slide.pdf` | for the main slides, short (5.5 × 2.3 in), with room for notes below: the effects of all terms of `MAIN_MODEL` on the heat pump |
| `excel/` | `rc_logit_results.xlsx` | all of it unrounded, every estimated parameter as `mlogit` names it, the tests of group differences, the convergence of each model and the expert its probabilities, AMEs and mean constants describe (sheet `models`, `expert_state`: `average` or `neutral`), and the effects of all terms (sheet `term_effects`, every change and outcome; sheet `term_effects_base`, the probabilities at each model's base state) |

**One meaning per row.** A row of a table, or a series of a figure, always
shows a group's own value, never its difference to the reference group:

- *mean constant* of a group: the mean utility of a technology relative to
  the reference outcome at the reference vignette (and, with prices, for an
  expert who reported the mean prices). In (4) the constant plus the group's
  shift, in (6i), (6p) and (6pe) the group's own constant, in (2) one for all
  experts.
- *attribute effect* of a group: in the models with interactions the
  attribute's coefficient plus the group's interaction, otherwise one for
  all experts.
- *standard deviation and correlation* of the constants: per covariance
  matrix, so for all experts in (2) and (4), per group in (6i), (6p) and
  (6pe). The heat maps have 1 on the diagonal.

Whether the groups differ is a question for the LR tests, and for the
probabilities and AMEs for the Krinsky–Robb draws: the probabilities of all
groups are saved at every draw of the parameters, so the difference between
two groups at each draw gives a standard error and a percentile interval of
the difference (`group_differences()`), for all pairs, in Excel. The figures
do not mark them. Where the groups share the attribute coefficients, as in
(4), their AMEs come from the same coefficient draws and are almost perfectly
correlated (0.95 on simulated data). A small difference that comes only from
the groups' different baseline probabilities is then significant although the
two intervals in the figure overlap almost completely, which is easy to
misread on a slide. The tests are cell by cell, without adjustment for the
many cells of a figure.

**Standard errors.** Tables have standard errors in parentheses: clustered by
expert for the coefficients and their sums (a group's mean constant or
attribute effect is a sum, so its standard error is exact), Krinsky–Robb for
the standard deviations and the probabilities. Stars only where a test of zero
makes sense, so not for the standard deviations (0 is on the boundary) or the
probabilities. Figures show 95% Krinsky–Robb percentile intervals.

**LR tests** (`LR_TESTS` in section 1): do the mean constants differ by group
((2) vs (4)); do the covariances ((4i) vs (6i), (4p) vs (6p), (4pe) vs
(6pe)); do the attribute effects ((4) vs (4i), (4p) vs (4ip)); do the prices
matter ((4) vs (4p), (4i) vs (4ip)); and for the variants of
(4p): do the attributes interact ((4p) vs (4pa)), does the price effect differ
by heat demand ((4p) vs (4pd)), do the expert characteristics matter ((4p) vs
(4pe)), in `tab_rcl_variants.tex` next to their fit, as the table of the
models and the table of the LR tests would no longer fit a frame with them.
(4pl) is not nested in (4p); with as many parameters, their log-likelihoods
compare directly. Whether the price effect is proportional to the heat demand
is a Wald test in (4pd) alone (`tab_rcl_m4pd_price_by_demand.tex`). Both models
of an LR test have the same sample and draws. A negative statistic, a larger
model below the smaller one, gets a dagger and a note in the tables, and a
message: that model stopped short of its maximum and must be estimated again
from the smaller one (`start_from`). The classical p-value assumes the larger model is
correctly specified; the robust one (Kent 1982; White 1982) compares LR with a
weighted sum of χ²(1) variables, with weights from the inverse Hessian and the
cluster-robust covariance matrix. The test of the covariances restricts the
covariance matrices, not their Cholesky factors, which are not unique.

**On slides.** Everything is sized for the 16:9 slides of the economics-latex
layout: figures 5.5 × 2.8 in (heat maps 3.2 × 2.8 in), included at natural
size with `\includegraphics{fig_rcl_m4_baseline.pdf}`; tables in
`\scriptsize`, each a complete float with caption and label, included with
`\input{\tablepath/tab_rcl_m4_constants.tex}` and checked to fit a frame. To
fit, the technology columns have one-word headers (as the heat maps), the cell
above the row labels says that the utilities are relative to the reference
outcome, the single price coefficient has its standard error beside it in a
cell across the columns, and the notes are one to three lines; the dummies
for a price not reported are in `tab_rcl_prices_not_reported.tex`. The sample and the
draws are in the notes of
`tab_rcl_fit.tex` and in the macros of `numbers_rcl.tex` (`\rclExperts`,
`\rclReferenceVignette`, `\rclPriceGas`, ...), which the slides `\input` once.
The macros have the same names in every run, so a deck inputs the
`numbers_rcl.tex` of one run only.
Groups have the same colour and symbol in every figure, models in every
comparison. The colours are the ZEW chart colours, as in the descriptives
(Max, 29 October 2026): energy advisers lime, HVAC installers gray, chimney
sweeps ice, all experts slate, and the comparison models in that order; the
symbols are filled with them, and symbols and intervals are drawn in dark
grey so that the light colours stand out on white. The heat maps run from
lime (−1) to slate (1). The tables need `booktabs` and `threeparttable`.

**Changing things.** The models and their order (`MODELS`, `MAIN_MODEL`),
the models of the comparison figures (`COMPARISON_MODELS`, at most four, one
per colour) and of the comparison tables (`COEFFICIENT_COMPARISON_MODELS`),
the models with figures of the effects of all terms (`TERM_FIGURE_MODELS`),
the tests (`LR_TESTS`), the labels, colours and sizes are all in section 1, and so
is the change of each variant of (4p) in its table (`VARIANT_CHANGES`). A new regressor of `01_run_specifications.R` that is an
attribute dummy needs a label in `ATTRIBUTE_LABELS`, any other regressor or
term one in `INTERACTION_LABELS`, `EXPERT_LABELS`, `PRICE_LABELS` or
`PRICE_NA_LABELS` (the dummies for a blank in Q17, `RISE_NA_TERMS`, are in
Excel only); the script stops and names any value without a label.

**Reading the results.**

- The constants, and so their correlations, are relative to the reference
  outcome. An expert who dislikes heat pumps has higher constants for every
  other technology, which makes those constants positively correlated; the
  correlations change with `REFERENCE_OUTCOME`.
- Coefficients are relative to the scale of the unexplained utility, which can
  differ between models; compare the AMEs across models, not the coefficients.
  An exception is the price coefficient of (4p) and (4pe): what (4pe) adds is
  the same in all of an expert's vignettes, so when it is left out the random
  constants take it up, not the unexplained utility, and the scale stays about
  the same. The two coefficients compare directly: if they differ, (4p)'s price
  coefficient mixed in the heat pump at home, the expected rises or the region,
  which can make it larger or smaller. The reported prices are beliefs, not
  randomised like the vignettes. This does not hold for (4pa) and (4pd), which
  add terms that differ between an expert's vignettes.
- The price enters linearly, in cent per kWh, centred at the mean over the
  experts of both arms (`price_reference` in the results). Its coefficient is the change in utility from one cent more per kWh, so a
  coefficient divided by minus the price coefficient is in cent per kWh: the
  price change that has the same effect on that technology's utility. It is
  identified only by differences between the prices the experts reported, not
  by the vignettes, and "No recommendation" has no price. In (4pl) that ratio
  is in log points instead: 0.1 is a price about 10% higher.
