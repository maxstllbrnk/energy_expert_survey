# Notes on the data

What was found while building the cleaning, for whoever has to rely on it. The rules
themselves are in the comments of `R/cleaning/`; the columns are in `docs/data_contract.md`.

## How waves are merged

Columns are matched to the raw exports **by question code**, never by position. The CSV headers
carry the code (`"Q24[SQ005]. Haben Sie …"`), so a wave with a different number of columns still
lines up.

This matters: the two soft-launch waves have 415 and 416 columns where the main waves have 417.
`Q24a` does not exist in either, wave 334335 has an extra sixth rank on `Q7`, and the numeric
`groupTime<id>` values differ in every wave, so group times are matched on their German label, which
is identical across all twelve.

The column separator is read off each file's header rather than assumed. The LimeSurvey export dialog
decides it, and it has already changed once between export batches (semicolon to comma); assuming the
wrong one reads a whole file as a single column and fails much later, looking like a naming bug.

The self-test proves the alignment: all nine design attributes must jointly resolve to a row of
`vignetten_kombinationen.xlsx`, which cannot happen by chance if columns are misaligned.

## Time and risk preferences

`R/cleaning/00_preferences.R` turns the two staircases (`tp_*`, `risk_*`) into scores: the
quantitative component of the Global Preference Survey (Falk, Becker, Dohmen, Enke, Huffman and Sunde
2018, QJE, section II.B). Each expert makes up to five binary choices, each setting the terms of the
next, closing in on their point of indifference. Five choices sort them into one of 32 ordered cells.

The published GPS measure combines the staircase with a Likert self-assessment, weighted 71/29 for
patience and 47/53 for risk taking. This questionnaire has no such item, so these scores are the
staircase alone: a valid ordering of experts, not comparable to published GPS country means.

- **Not everyone gets five levels.** LimeSurvey never generated `Time5ABBA` or `Time5ABBB`, so an
  expert whose first three time answers were A, B, B stops after four. Others broke off. A shorter path
  gives a wider cell, and its midpoint is exactly the average of the two cells it contains, so nobody is
  pushed in either direction. Use `*_levels` to drop shallow paths; at depth 1 the score is only 0.25 or
  0.75.
- **A z-score depends on who is in the sample.** Between everyone who answered and completers only, the
  scores move by up to 0.07 SD. So `experts.rds` holds only the scores, and a script that wants z-scores
  computes them on its sample.

## Bundesland

`bundesland` and `bundesland_source` are derived from the postcode `plz`; the questionnaire never asks
for the state. The lookup is a plain CSV in Dropbox, built **once** by
`R/geodata/build_plz_bundesland.R`. Nothing in the pipeline downloads anything.

| Source | For | Licence |
|---|---|---|
| [BKG VG2500](https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/), the federal mapping agency | the official state keys and names, and the polygons for maps | dl-de/by-2-0 |
| [GeoNames postal codes DE](https://download.geonames.org/export/zip/DE.zip) | the postcodes, with the official AGS district key | CC-BY 4.0 |

The first two digits of the AGS are the official state key, so the state is read straight off the
postcode's district, with no geometry at all. Checked against a point-in-polygon join on the BKG
polygons, the two agree on 99.80% of GeoNames' 23,297 rows, and the exceptions are border cases.
GeoNames' own state column is not used: it holds 23 spellings for 16 states and agrees with the
AGS-derived state only 90.3% of the time.

Postcodes are Deutsche Post routing areas, not administrative units, so `bundesland_source` says how
each one was reached:

| | |
|---|---|
| `plz` | in the lookup, and the postcode lies in one state only |
| `plz_border` | in the lookup, but it straddles a state line: assigned to the state holding most of it (32 of 10,812 postcodes) |
| `plz_prefix` | not in the lookup, but every postcode sharing its first three digits is in one state. Reproduces the right state 98.3% of the time (leave-one-out) |
| `unmatched` | five digits, but no such postcode exists in Germany: a typo, left as `NA` |
| `no_postcode` | Q0 was blank or unusable |

To rebuild with a newer vintage: `Rscript R/geodata/build_plz_bundesland.R refresh`, then rerun the
cleaning.

## NUTS3

`nuts3` and `plz_in_nuts_list` come from Eurostat's postcode to NUTS correspondence (GISCO; postcodes
2025, NUTS 2024). The list puts every postcode in exactly one NUTS3 region, so there is no border or
prefix rule: a postcode is either in the list or `nuts3` is `NA`.

The list holds only geographic postcodes. P.O.-box and large-customer postcodes are valid and keep `plz`
and `bundesland`, but get no NUTS3 region, so no degree days. Of the 10,812 postcodes in the GeoNames
lookup, 2,502 are not in Eurostat's list. The report gives counts only: a large-customer postcode can
identify a firm.

24 postcodes are in Eurostat's list but not in GeoNames. Several look miscoded, because Eurostat puts
them in a different state than their postcode area: 04728, 04730, 04731 and 04760 (Saxon postcodes) in
Nordrhein-Westfalen, 06280 (Sachsen-Anhalt) in Schleswig-Holstein, and 57320 and 57350 (Siegen area) in
the Saarland. An expert with one of these gets the Kreis, and so the degree days, of the other state.
`nuts3` and `bundesland` can disagree for those and for 22113, 26954, 64754 and four postcodes on a state
line. The cleaning report counts the disagreements.

## Working with renv

`renv` pins every package to an exact version, so that the pipeline gives the same result on any machine
and in a year's time. `renv.lock` is the manifest and is committed; the library is private to the
machine and is not. `.Rprofile` activates it whenever R starts in this folder.

| Command | When |
|---|---|
| `renv::status()` | is the lockfile in sync with what I'm using? |
| `renv::snapshot()` | I added a package: record it in the lockfile, and commit `renv.lock` |
| `renv::restore()` | new machine, or I just pulled: install what the lockfile says |
| `renv::update()` | deliberately move to newer versions, in a commit of its own |

`renv` finds dependencies by scanning `.R` files for `library()` calls, and records the R version but does
not install R.
