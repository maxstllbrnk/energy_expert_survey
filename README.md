# Energieberater vignette survey — data cleaning

Turns the raw LimeSurvey exports of the Energieberater expert survey into three
clean data sets. Twelve mailing waves, including the two soft-launch waves.

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

DROPBOX (DROPBOX_DIR)                  …/Heating Transition/analysis_survey
  public_data_dropbox/                   inputs that are safe to share
    LimeSurvey_Versanduebersicht_010726.xlsx    batch overview
    vignetten_kombinationen.xlsx                vignette design matrix
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

PROJECT (this folder, on GitHub)
  config.R                    the three roots and the paths derived from them
  config.local.R              optional per-machine roots  (git-ignored)
  R/run_cleaning.R            entry point for the cleaning pipeline
  R/cleaning/
    00_functions.R              parsing, naming, shared helpers
    01_codebook.R               DATA SET 1
    02_read_stack.R             read and merge all twelve waves
    03_recode.R                 postcode repair, bundesland, "Sonstiges", flags
    04_vignettes.R              DATA SET 2
    05_main.R                   DATA SET 3 + build report
  R/add_preferences.R         the GPS time- and risk-preference scores
  R/geodata/
    build_plz_bundesland.R      run ONCE: downloads and builds the postcode lookup
  tests/selftest.R            checks on the cleaned output
  renv.lock                   the exact package versions
```

Analysis scripts go in `R/analysis/`, with their own runner (`R/run_analysis.R`)
alongside `R/run_cleaning.R`. Keep cleaning and analysis separate: the cleaning
pipeline should not change when an analysis does. Their tables and figures
belong under `output_dropbox/`, each in their own sub-folder next to
`reports_cleaning/`.

## What you get

| File | Written to | Grain | What it is |
|---|---|---|---|
| `survey_main.*` | secure (`CLEAN_DATA_DIR`) | one row per expert | everything outside the vignette module |
| `vignettes_long.*` | secure (`CLEAN_DATA_DIR`) | one row per expert × vignette | recommendation, branch, response times, the nine design attributes, `vignette_id` |
| `preferences.*` | secure (`CLEAN_DATA_DIR`) | one row per expert | time- and risk-preference scores — from `R/add_preferences.R`, not from the pipeline |
| `codebook.xlsx` | Dropbox (`REPORT_DIR`) | one row per variable | question text · LimeSurvey question id · analysis variable name |
| `build_report.xlsx` | Dropbox (`REPORT_DIR`) | — | run diagnostics — **read this after every run** |

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

- **`renv.lock`** — the manifest: R 4.6.1 plus 102 packages with exact versions.
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

`sf` is not in `renv.lock` — nothing in the pipeline needs it. Install it and
`renv::snapshot()` when you start on the maps.

### Rebuilding

Only when you want a newer vintage. It overwrites the lookup, so re-run the
cleaning pipeline afterwards.

```bash
Rscript R/geodata/build_plz_bundesland.R refresh
```
