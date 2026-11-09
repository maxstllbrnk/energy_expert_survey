# Session C: the summary statistics of the questions and the vignettes

**Owns:** `R/summary_statistics/section_experts_firms.R`, `section_market.R`,
`section_prices_beliefs.R`, `section_barriers_criteria.R`, `section_preferences.R`,
`section_heating_context.R`, `section_vignettes.R`, `tests/test_sections.R` (new).

Session B writes the recruitment, completion and selection sections and the deck at the same time. Read
`R/summary_statistics/run_summary_statistics.R` and `R/functions/exhibits.R` (`exhibit_row()`,
`summary_table()`, `subset_group()`, `plot_state_map()`, `state_means()`), `labels.R` and
`R/functions/tables.R`, `figures.R` first.

Max's request: "a full set of summary statistics that can directly go into that paper... summary
statistics for all experts as well as within expert groups... group the summary statistics for questions
together in a way that makes the most sense and is easily understandable for a reader. Sometimes you may
just make a table with summary statistics for some variables that are not of a lot of interest that will
ultimately just go into the appendix. Make the decision on what format, i.e. table, graphic or map, suits
each variable the best. Tables should contain the number of answers, the mean, standard deviation, and all
quartiles. The graphics do no longer have to be in the ZEW colors. Rather go back to the color scheme we had
in the beginning. The graphics and maps should all be exported as pdfs. The tables should be exported as
latex tables."

Conventions: `tab_<id>_<slug>.tex`, `fig_<id>_<slug>.pdf`, `map_<id>_<slug>.pdf` with `group_slug()`.
**Every exhibit below comes in four versions, all experts and each group, unless a row says otherwise**: a
loop over `c(ALL_EXPERTS, EXPERT_GROUPS)` with `subset_group()`. Each returns `exhibit_row()`s; paths relative to
the summary statistics folder. `placement` as in the tables: the group versions of a main exhibit are
`appendix`. Tables through `summary_table()` (numbers: N, mean, SD, quartiles; answers: N and share) or
`table_custom()`. Figures through `save_figure()`, no titles inside; `theme_paper()` is set globally. English
labels only, from `labels.R`: a German answer in a table is a bug (`label_of()` stops on unknown values). A
variable that is missing from the sample is skipped; `summary_table()` does this by itself. A section with
nothing to show returns `tibble()`. Figures with too few answers (`MIN_N`) are skipped, and bars of fewer than
`MIN_N` answers left out. Questions asked of only some firms: say whom in the note.

Note the group comparison tables: for each main table of numbers, one all-groups table with
`by = "expert_group"`, `stats = c("n", "mean", "sd")`, `id` `<id>_groups`, placement `main`.

## Block D: `section_experts_firms(experts, dirs)`

| Exhibit | Content | Format | Placement |
|---|---|---|---|
| `experts` | Panels: the expert (`age`, `dem_training_year_clean`, owner, education levels, Q24 topics); the firm (`firm_employees_clean`, revenue levels, radius levels, Q2 categories) | table | main (all and groups comparison), appendix (per group) |
| `experts_state` | Experts per state (count, all experts only) | map | appendix |

## Block E: `section_market(experts, dirs)`

| Exhibit | Content | Format | Placement |
|---|---|---|---|
| `market` | Q8 services; `adv_households_clean`; Q7 customer groups (first and second rank) with the soft-launch caveat; Q11c (`brand_top1`, shares of the most named, with the rest as "Other"; below `MIN_N` goes into "Other") | table | appendix |
| `technology_shares` | Mean shares per technology in Q10b (recommendations) and Q11b (installations), `_filled` | table | main |
| `technology_shares` | The same, stacked bars of the mean share, Q10b and Q11b side by side | figure | main |

## Block F: `section_prices_beliefs(experts, dirs)`

| Exhibit | Content | Format | Placement |
|---|---|---|---|
| `prices` | `price_<fuel>_kwh`, one row per fuel | table | main |
| `price_distribution` | Distribution per fuel. Bins of fewer than `MIN_N` answers are not drawn | figure (`tall`) | appendix |
| `price_expectations` | Q17: share of each answer per fuel, colours `EXPECTATION_COLOURS` | stacked bars | main |
| `price_expectations` | The same in numbers | table | appendix |
| `price_tests` | Do the groups differ in Q17? G-test with Cramér's V per fuel, all experts only. Port from old `R/first_policy_brief/02_kl_divergence_prices.R` and the use in `R/descriptives/05_policy_brief.R`, computing it here | table | appendix |
| `carbon_price` | `co2_heard`; `co2_est_low_clean`, `co2_est_high_clean` | table | appendix |
| `price_state` | Mean reported gas price and heat-pump electricity price by state (`state_means()`), all experts only | map | appendix |

## Block G: `section_barriers_criteria(experts, dirs)`

| Exhibit | Content | Format | Placement |
|---|---|---|---|
| `barriers_<file>` | For each row of `BARRIER_QUESTIONS`: the share ticking each option. Q19a main, the others appendix | figure | main, appendix |
| `barriers` | The three questions side by side, one row per option, N per question | table | appendix |
| `customer_criteria` | Q28: mentions per criterion, stacked by rank (`RANK_COLOURS`) | figure | main |
| `customer_weights` | Q28: mean weight per criterion (rule in old `ranking_weights()`, `R/summary_statistics/01_plot_functions.R`) | table | appendix |

## Block H: `section_preferences(experts, dirs)`

`preferences`: `tp_score`, `risk_score`, and the share who went through the whole staircase, table,
appendix. `preference_distribution`: histogram of the two scores, figure, appendix.

## Block I: `section_heating_context(experts, dirs)`

| Exhibit | Content | Format | Placement |
|---|---|---|---|
| `own_heating` | Q20 (`own_heating`), bars in `TECHNOLOGY_COLOURS` | figure | main |
| `heating_context` | Q20 and Q6a (`firm_dh_in_area`) answers; `hdd` | table | main |
| `district_heating_state` | Share "available" in Q6a by state (`state_means()`), all experts only | map | main |
| `degree_days` | Degree days by NUTS3 region, public data: `KREIS_SHP` (its column `NUTS` matches `geo` of `HDD_FILE`), year `HDD_YEAR`, all experts only and no answers involved | map | appendix |

## Block J: `section_vignettes(experts, vignettes, dirs)`

| Exhibit | Content | Format | Placement |
|---|---|---|---|
| `design` | Attributes, levels, reference level, N vignettes per level, all experts only | table | main |
| `balance` | Is the design balanced? Share of each level by arm and group, chi-squared p-value per attribute, all experts only | table | appendix |
| `recommendations` | Shares by arm, side by side, `ARM_COLOURS` | figure | main |
| `recommendations` | Counts and shares by arm | table | appendix |
| `recommendations_groups` | Shares by group, one panel per arm, all experts only | figure | main |
| `recommendations_<attribute>` | Shares by each level of each attribute (not `att_image`; `att_heat_demand` stands for `att_renovation`), `TECHNOLOGY_COLOURS` | figure | appendix |
| `arms` | Who is in each arm: means and shares of group, prices, degree days, heating at home, in the two arms (the arm follows the answer to Q6a and is not randomised), all experts only | table | main |
| `within_expert` | Variation within experts: number of different technologies an expert recommends across six vignettes; share recommending one technology throughout. It matters for the random constants. | table | main |
| `position` | Shares of the recommendations by vignette position (fatigue), all experts only | figure | appendix |
| `investment_cost` | Q on the last vignette (`vig_cost`), `COST_COLOURS`, by the technology recommended | stacked bars | appendix |

Old code: `R/descriptives/04_experts.R`, `05_policy_brief.R`, `06_vignettes.R`, `01_functions.R`,
`00_settings.R`, and `R/summary_statistics/` for the original look (`01_plot_functions.R`, `04_vignettes.R`).

## Done when

`tests/test_sections.R` runs your sections on the synthetic full sample and on a sub-sample (completers of
one arm; and without some columns, e.g. no `mix25_*`), and every exhibit file exists and its row is in the
list. Compile a small deck of your own in the scratch folder to look at them, as `build_presentation()` is
being written by Session B: `\documentclass{article}` with `booktabs`, `threeparttable`, `makecell`, is enough
for tables, and `pdftoppm -r 60 -png` for figures. Look at them: readable, right colours, no German, no
overlapping labels, percent signs escaped. Figures are 5.5 x 2.3 in (`full`): keep legends in one row.
