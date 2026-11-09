# Session B: recruitment, completion, selection, and the deck

**Owns:** `R/summary_statistics/section_recruitment.R`, `section_completion.R`,
`section_selection.R`, `presentation.R`, `R/functions/frame.R` (new),
`R/run_summary_statistics.R` (new), `tests/test_deck.R` (new).

Session C writes the other sections at the same time, on the same helpers. Read
`R/summary_statistics/run_summary_statistics.R` and `R/functions/exhibits.R` (`exhibit_row()`,
`summary_table()`, `subset_group()`, `plot_state_map()`, `state_means()`), `labels.R`,
`questionnaire.R` and `R/functions/tables.R`, `figures.R` first.

Max's request: "response rates across states as well as between groups, summary statistics for all
experts as well as within expert groups, and also all the survey metadata, i.e. who did not complete
the survey, and when did they stop. Does the group with incomplete questionnaires have different
summary statistics from the one that they answered than the group with complete questionnaires."
"The tables should contain the number of answers, the mean, standard deviation, and all quartiles.
The graphics and maps should all be exported as pdfs. The tables should be exported as latex tables.
I want a presentation in the summary statistics folder that always automatically updates and displays
the current version of the graphics and tables and maps, so that one can easily check the results."

Conventions: `tab_<id>_<slug>.tex`, `fig_<id>_<slug>.pdf`, `map_<id>_<slug>.pdf` with
`group_slug()`. Every exhibit comes for all experts and for each of the three groups unless said
otherwise. Each section returns `exhibit_row()`s; paths relative to the summary statistics folder.
Tables through `summary_table()` or `table_custom()`, figures through `theme_paper()` (set globally),
`save_figure()`, no titles inside figures. English labels only, from `labels.R`. Colours from
`labels.R` (the original palette, not the ZEW colours). Each section checks that its columns exist
(`has_columns()`) and writes nothing if the block does not apply, e.g. no frame, or only completers.

## Block A. Recruitment: `section_recruitment(experts, vignettes, frame, dirs)`

Returns `tibble()` if `frame` is `NULL`. Port `load_frame()` to `R/functions/frame.R` (from old
`R/ruecklauf/01_frame.R`; `R/functions/frame.R` also gets `FRAME_TYPE_LABELS`, mapped to
`EXPERT_GROUPS`, and the t-online rule). Everyone counts as a person: an e-mail address of the lists
once, an invitation once. **There are no token lists (Max, 2026-11-09), so a response cannot be
matched to its list: by group, the denominator is the mailing list and the numerator the answer to Q2.
Every exhibit by group says so in its note.** The old `apply_token_bridge()` goes.

| Exhibit | Format | Placement |
|---|---|---|
| `sample_flow`: invited, started, reached the vignettes, completed, completed with a group, by arm with answered vignettes. Experts and vignettes. | table | main |
| `response_state`: response rate by state (completed per invited), minimum 30 invited and 10 completed | map (`size = "map"`) | main |
| `response_state`: the same for the started questionnaires; and per group | map | appendix |
| `response_state_table`: by state: invited, started, completed, rates; with all contacts and without t-online addresses | table | appendix |
| `response_group`: by group: invited, started, completed, rates, with and without t-online | table | main |
| `composition`: shares of the groups among invited and completed | figure | main |
| `response_wave`: by wave and target group, soft launch marked, with the cross-check against the dispatch log (`load_versand_total()` of the old code) | table | appendix |

Old code: `R/descriptives/03_recruitment.R` and `R/ruecklauf/` (`02_link.R`, `03_karten.R`,
`04_berufsgruppen.R`, `05_ueberblick.R`, `06_tabellen.R`).

## Block B. Survey metadata: `section_completion(experts, vignettes, dirs)`

| Exhibit | Format | Placement |
|---|---|---|
| `survival`: share of the persons who started still in the survey at each question (`last_question_position`), questions on the axis in questionnaire order, one line per group in the all-experts version | figure | main |
| `breakoff_section`: of those who never completed, the share stopping in each section | figure | main |
| `breakoff_question`: by question: N and share stopping there (a count of who stopped may be small) | table | appendix |
| `breakoff_state`: break-off rate by state, warm ramp (`MAP_BREAKOFF_LOW`, `MAP_BREAKOFF_HIGH`) | map | appendix |
| `duration`: `interview_minutes` of completers: N, mean, SD, quartiles; and by section (sum of question times, `question_times()`), with quartiles, not trimmed | table | appendix |
| `vignette_times`: median time per vignette position | figure | appendix |
| `item_response`: for each question: reached, answered, share answered among those who were asked (mind the routing in the data contract) | table | appendix |
| `meta`: repeat starts, further attempts, soft launch vs main, arm conflict, `arm_source` | table | appendix |

The `survival` version per group needs no other lines. Writes nothing if every expert completed, except
`duration`, `vignette_times`.

## Block C. Selection: `section_selection(experts, vignettes, dirs)`

Max's question: does the group with incomplete questionnaires differ, on what they did answer? Writes
nothing unless the data hold at least `MIN_N` completers and break-offs.

| Exhibit | Format | Placement |
|---|---|---|
| `selection`: every variable both groups could reach, in questionnaire order: for completers and break-offs each: N, mean, SD, quartiles; then the normalised difference (Imbens and Rubin) and a p-value (t test; chi-squared for shares). Categorical answers as shares per level. Only experts who reached the question count: compare with `last_question_position`. | table | main (all); appendix (per group) |
| `selection_vignettes`: the shares of the recommendations in the answered vignettes of completers and break-offs, by arm | table | main |

Variables: the firm block (`resp_is_owner`, Q2 categories, `firm_employees_clean`, `firm_revenue`,
`firm_radius_recoded`, `firm_dh_in_area`), expert group, arm, wave type, state; the prices (`price_<fuel>_kwh`),
expectations and whatever later blocks enough break-offs reached. A row below the minimum cell in either
group shows `--`. Wide table: use `table_custom()`, formatting with `fmt_num()`, `fmt_int()`. Note in the
table: p-values are not adjusted for multiple testing, and they are descriptive.

## The deck: `build_presentation(exhibits, output_dir, sample_label, experts, vignettes)`

Beamer, 16:9, with `econslides.sty` (a copy is next to this code; copy it to `output_dir`). It writes
`summary_statistics.tex` next to `tables/`, `figures/`, `maps/` and compiles it there twice with
`pdflatex`, from `output_dir` as the working directory (`setwd()` inside the function, restored with
`on.exit()`), so relative paths work under folders with spaces. If `pdflatex` is missing or fails: a
message with the log's path, no stop, and the `.tex` stays.

- First slide: the title, the sample label, the date, N experts and vignettes, and the commit
  (`git rev-parse --short HEAD`), so that a reader sees which run this is. Then one overview slide of
  the blocks, with hyperlinks.
- A section per block, in the order of the exhibits; within a block all experts first, then the groups.
  One frame per exhibit. Frame title: the caption, with the group and "main" or "appendix".
- Figures and maps at natural size, the note below in `\slidenote{}`. Tables go through
  `adjustbox` with `max width=\textwidth, max totalheight=0.82\textheight, center`, in this deck only.
  So nothing overflows however long a table is.
- A last section "Not shown", listing what the sample does not have: the blocks that wrote nothing.
- Tables are `threeparttable` fragments: check they compile inside `adjustbox`.
- `build_presentation()` can run alone on an existing folder: it needs only `exhibits.csv`.

`R/run_summary_statistics.R`: the entry point for the full sample: reads `experts.rds` and `vignettes.rds`,
`load_frame()`, calls `run_summary_statistics(experts, vignettes, file.path(OUTPUT_DIR, "full_sample",
"summary_statistics"), "Full sample", frame = frame)`.

## Done when

On synthetic data, `R/run_summary_statistics.R` writes all your exhibits and a deck that compiles, also
with `EES_SYNTHETIC_DIR` under a path with a space in it. Render the deck with `pdftoppm -r 60 -png` and
look at it: slides readable, no overflow, maps with labels, no German. `grep "Overfull"` in the log for
frames with figures. The other sections are stubs in your branch: `run_summary_statistics()` must run
anyway. Test a sub-sample too: completers of one arm, no frame.
