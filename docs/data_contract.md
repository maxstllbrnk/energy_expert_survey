# The cleaned data

`R/run_cleaning.R` writes three files into `CLEAN_DATA_DIR` (secure). Everything else
starts from them.

## `experts.rds`: one row per person, key `resp_uid`

All columns of `survey_main` (the answers as the respondents gave them, German levels),
plus the columns below. A person who started twice keeps the furthest attempt.

| Column | Type | Meaning |
|---|---|---|
| `person`, `n_attempts` | chr, int | token (or `resp_uid` without one); started questionnaires of this invitation |
| `is_complete` | lgl | submitted the questionnaire |
| `expert_group` | factor | `Energy advisers`, `HVAC installers`, `Chimney sweeps`; `NA` if none can be assigned |
| `expert_group_note` | chr | `assigned`, `Q2 not answered`, `no category ticked` |
| `arm` | factor | `fw` (with the district-heating option), `nofw`; `NA` if unknown or answered in both. Use this, not `vig_arm` in `experts`, which holds only the arm that was observed |
| `last_question`, `last_section` | chr | the last question answered and its section, in English; `No question answered` and `NA` if no page was submitted |
| `last_question_position` | int | its position in `QUESTIONNAIRE`, 0 if no page was submitted |
| `price_<fuel>_kwh` | dbl | reported price in cent per kWh after the unit rules; `NA` if blank or not usable. `<fuel>`: `gas`, `hpelec`, `dh`, `oil`, `pellet` |
| `price_<fuel>_treatment` | chr | `blank`, `as reported`, `euro`, `per litre or kg`, `euro and per litre or kg`, `not usable` |
| `pexp_<fuel>_rise` | dbl | 1 if the expert expects the price to rise, 0 if not, `NA` if blank |
| `own_heating` | chr | answer of Q20, `Keine Angabe` if missing, also for everyone who stopped before Q20 |
| `heat_pump_at_home` | dbl | 1 if a heat pump heats the expert's home, else 0, also for everyone who stopped before Q20 |
| `hdd` | dbl | heating degree days of the expert's NUTS3 region in `HDD_YEAR`; `NA` without `nuts3` |
| `tp_score`, `risk_score` | dbl | staircase score in (0, 1); also `_cell`, `_levels`, `_complete`. z-scores depend on the sample: compute them in the script |
| `dem_birthyear_clean`, `dem_training_year_clean`, `firm_employees_clean`, `adv_households_clean`, `co2_est_low_clean`, `co2_est_high_clean` | dbl | the answer after the outlier rule (valid range, then the 0.5% tails over everyone who answered) |
| `age` | dbl | `SURVEY_YEAR` minus `dem_birthyear_clean` |
| `interview_minutes` | dbl | LimeSurvey's total time; for a break-off, the time until the break-off. Not trimmed: use medians and quartiles |
| `adv_share_<tech>_filled`, `mix25_<tech>_filled` | dbl | share of Q10b and Q11b for an expert who gave shares, 0 where left blank; `NA` for the others. `<tech>`: `hp`, `pellet`, `dh`, `gas`, `oil`, `hybrid`, `other` |

The answers of `survey_main` by block: `firm_*` (Q0 to Q6a, with `plz`, `bundesland`,
`nuts3`, `firm_radius_recoded`), `resp_*`, `price_<fuel>`, `pexp_<fuel>`, `co2_*`, `mix25_*`,
`brand_top1`, `tp_*`, `risk_*`, `svc_*`, `adv_*`, `barr_<ren|dh|conv>_<option>`,
`cust_rank1..6`, `crit_rank1..8`, `dem_*` (with `dem_education_recoded`), `cpd_*`,
`fb_comment`, `time_*` (response times in seconds), `meta_*` (LimeSurvey), and the flags
`wave`, `target_group`, `versandrunde`, `is_softlaunch`, `reached_vignettes`,
`n_vignettes_answered`, `arm_conflict`.

Routing, and so `NA` by design: Q0b only if not the owner; Q8 and Q10 only for firms with
energy advice, engineering or chimney sweeping; Q11b and Q11c only for HVAC firms; Q18b only
if the carbon price is known; Q24a only with other training. Soft-launch caveats: Q6 had no
"bundesweit", Q7 was a different ranking task (`is_softlaunch`).

## `vignettes.rds`: one row per person x vignette, key `resp_uid` x `vig_num`

Six rows per person, also for those who broke off (the design is known either way). Left out:
the further attempts, and the one expert who answered in both arms.

| Column | Meaning |
|---|---|
| `vig_num` | 1 to 6, the position in the questionnaire |
| `vig_arm`, `arm_source` | the arm of this expert; `answered`, `routed from Q6a`, `unknown` |
| `vig_rec` | the recommendation, factor with seven levels in German; `NA`: not answered |
| `vig_rec_other` | free text of `Keine Empfehlung` |
| `vignette_id` | which of the 128 design combinations |
| `att_image`, `att_couple_age`, `att_income`, `att_current_heating`, `att_replacement_timing`, `att_build_year`, `att_heat_demand`, `att_heat_distribution`, `att_renovation` | the attributes, character, exactly as in the data (`DESIGN_LEVELS` in `labels.R`) |
| `vig_time`, `vig_group_time` | seconds |
| `vig_cost`, `vig_cost_source`, `vig_cost_time` | cost question, last vignette only |
| `expert_group`, `is_complete` | as in `experts.rds` |

## `reference_values.rds`: list, aggregates only

Over the completed questionnaires of the three groups in both arms, so that both estimation
scripts can share one reference expert:

- `prices`: one row per fuel: `n`, `median`, `lower`, `upper` (the bounds of the unit rule),
  `mean`, `q25`, `q75`
- `expectations`: one row per fuel: `answered`, `share_rise`
- `own_heating`: named vector, the share of each answer of Q20, `Keine Angabe` included
- `hdd`: list: `year`, `n`, `mean`, `q25`, `q75`
- `n_experts`, `date`

## The sampling frame: `load_frame()` in `R/functions/frame.R`

One row per contacted person: `frame_id`, `type`, `plz`, `expert_group`, `is_tonline`,
`bundesland`. The e-mail address identifies a contact and does not leave the function. Only
`run_summary_statistics()` needs the frame, for the response rates of the full sample.
`load_versand()` gives the dispatch log, one row per wave.

## The cleaning report: `OUTPUT_DIR/cleaning/cleaning_report.xlsx` and `.rds`

One sheet per topic: `overview`, `rules`, `settings`, `waves`, `attempts`, `arms`,
`expert_groups`, `prices`, `expectations`, `own_heating`, `outliers`, `shares`, `preferences`,
`geography`, `variables`. The raw stage adds `raw_checks`, `harmonisation`, `recodes` and
`data_quality`. `section_cleaning()` reads the `.rds` for the tables of the data appendix.

## Rules for outputs

Aggregates only. Every statistic rests on at least `MIN_N` = 10 answers, every response rate
also on at least `MIN_CONTACTS` = 30 contacted persons; no minimum or maximum of individual
values. Counts of what a cleaning step changed may be smaller.
