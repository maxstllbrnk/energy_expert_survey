# Open issues of the new pipeline

What the rebuild of November 2026 left for the researchers: what was decided, what is left for later, what to check on the first real run, and the issues to keep in mind for the paper. Numbers from "the synthetic run" are from simulated data with one seed: they show how the code behaves, not what the survey says.

## 1. Decided on 2026-11-09

Each is implemented where the last column says, with Max's name and the date in a comment there.

| Topic | Decision | Where |
|---|---|---|
| AME step of the model rule | An AME that only the larger model has counts as moving from 0, its value in the smaller model, if it is at least 1 pp and its 95% Krinsky-Robb interval excludes 0. In the synthetic run, where prices matter, the price effects reached 5.4 pp (z 3.5 to 6.1) while the shared effects moved 0.3 pp, and the old step kept (4) as the main model; the dummies for a missing answer (z 0.1 to 1.9) mostly do not count | `compare_models()` |
| Cross-validation | Every model, so that every question has the same parsimony gate. A quick run leaves it out | estimation scripts |
| Gaps in the rule | Kept as the code settles them: a gain that fails on fresh draws keeps the smaller model; a failed fold lets BIC decide; a term without variation in a fold keeps its full-sample estimate; cross-validation must win by one paired SE; the fresh-draw critical value is at 5%, not Holm's level; valid means a scaled gradient up to 0.1 and a condition number up to 1e8; a test with an invalid model counts as not rejected in the Holm family | `compare_models()` |
| Minimum cell | A share needs 10 answers in its denominator, a count printed on its own needs 10, cells of vignettes count distinct experts. A numerator that follows from N and a share, or a hidden cell that follows from a total by subtraction, is accepted. Counts of what the cleaning did may be small | profile, data contract |
| Heating at home | `own_heating` and `heat_pump_at_home` are `NA` for experts who stopped before Q20; anyone who answered it counts as having reached it | step 06 |
| Shares outside 0 to 100 | All shares of that question missing for that expert, and counted, instead of stopping the cleaning | step 06 |
| Heating oil in euro per litre | Answers up to 1.40 are read as euro per litre, like those up to 0.99 | step 06 |
| Price rule | A price more than 5 times above or below the median is not usable (was 3) | step 06 |
| Ties between attempts | Of two attempts that got equally far, the one started first stays | step 06 |
| Outlier rule | Years keep their valid range but are not trimmed; employees, households and carbon prices cannot be negative and keep their 0.5% tails trimmed; a carbon range whose lower end lies above its upper end loses both ends, before the tails are trimmed | step 06 |
| Mean of the prices as reported | Dropped from the report and the appendix: it mixes units, and one typo can dominate it | step 07 |
| Frame rows without an e-mail address | Not invited: they leave the denominator, and are counted | `load_frame()` |
| Free text in `EDUCATION_MAP`, the respondent ID in the git history, the seminar's numbers in comments | Fine as they are | |
| Standards | Six rules from the reviews go into the profile, among them that every new cleaning rule is tested on a case that sets it off, and that a test excuses an expected difference only by exact name; one, that two statistics whose difference rests on fewer than 10 answers count as one, is dropped, as recovery by subtraction is accepted | profile |

## 2. Left for later

- **For the output script of the models:** whether a coefficient counts as a statistic under the minimum cell (a dummy held by three experts has a coefficient), and whether `results$choice_counts` in `results.rds` is blanked below it. `results.xlsx` and
  `results.rds` stay on the secure drive and are not outputs for the paper, so this matters only once tables are made from them.
- **For the paper:** whether `selection_prices` belongs in the main text (`selection` is split into five tables to fit a page); the wording of "AME", a discrete change at one reference expert and vignette, not an average over the sample; the COP (3) and the gas
  share of the hybrid (0.5), still placeholders of 2026-10-19 and 2026-10-20, which the heat pump and hybrid price terms depend on.
- **For the economics-r skill, not this project:** list the common base functions (`setdiff()`, `match()`, `coalesce()` and the like) in the style vocabulary, so that they need no comment at their first use.

## 3. To check on the first real run

Best with `QUICK_RUN <- TRUE` in `config.R` first (README, "How to run").

- **The cleaning report:** the sheet `attempts` should show no repeat completions, as the seminar's sample flow did; if it shows any, the old log-likelihoods no longer apply. The sheets `outliers`, `shares` and `prices` count what the new rules do.
- **Q11c:** read the panel of manufacturers. `NOT_A_BRAND` in `section_market.R` cannot be complete: another word with at least 10 mentions would reach the table. `str_to_title()` also rewrites brand names, BWT to Bwt.
- **Stops that may fire, each naming what to do:** `fit_mixed_logit()` stops on a term with no finite estimate, for any two-valued term in one alternative. This is broader than the old code and may fire for (4pe) or (6pe) in the small arm, e.g. `oil_rise_na` if
  no expert with a blank recommends oil. `post_estimation()` stops on a Hessian that is not positive definite.
- **(6pe) in the small arm.** In the synthetic run the estimates for chimney sweeps and oil were not credible: constant -10.5 (SE 4.7), SD 6.3, on 32 experts. It passed every check, with a condition number of 1.1e5.
- **Rare dummies in the tests:** a mean weight far below 1 in the sheet `nested_tests`, which the reason of the decision now flags below 0.5, means that the robust test rests on few experts and rejects too easily; compare the classical p-value.

## 4. Statistical issues

**Model comparison**

- The robust test can over-reject in the group-covariance test. On the synthetic data with equal covariances, the mean weight is 0.83 and the robust p below the classical p. The seminar's (4pe) against (6pe) shows the same pattern (p 0.005, robust p below 0.001), so that gap may be small-sample bias in the clustered variance rather than misspecification. This rests on one draw.
- The robust test can reject almost anything if a coefficient rests on few experts: with a dummy held by 3 of 300 experts, the mean weight was 0.06 and the robust critical value 0.2, against 3.84 for chi-squared. A clustered variance is known to collapse with few
  treated clusters. The fresh-draw gate does not help here, as it uses the same weights; cross-validation does. The flag at a mean weight below 0.5 rests on that one case and only warns; in the small arm of the synthetic run, (4pe) against (6pe) had 0.69, not flagged.
- A nested test whose restrictions are not independent at the estimates is not compared: its classical LR is reported but plays no part, and in the Holm family it counts as not rejected.
- The LR on the estimation's draws is inflated with many constants and few draws: 61.2 against 39.9 on fresh draws with 6 constants and 100 draws. With 3 constants there is no inflation. The fresh-draw gate will bind less at 1,000 draws, so cross-validation will
  carry more of the decision. The mean fresh LR is biased downward, as the parameters were tuned on other draws.
- The gate's critical value is lower than the bar the test cleared: it is not adjusted for Holm, and weights below 1 lower it further (50.2 against 58.1 for chi-squared with 42 df).
- One paired SE is a low bar, and the SE is rather too small, as the folds share experts in training. Folds start from the full-sample estimates, so held-out experts influence which optimum a fold reaches; training on 4/5 of the sample works the other way, against larger models.
- Vuong's normal limit is for strictly non-nested models; sets that share terms overlap, and no variance pre-test is run.
- Holm depends on the tests declared and runs per call, so per arm. (4p) against (6p), robust p 0.034, no longer rejects once a second nested test is declared and its p-value is the smaller or larger (0.068).
- The AME step uses the largest of many rows against a fixed 1 pp. For the effects both models have, their SE plays no part, as no SE of the difference between two models is at hand. An added effect also needs a 95% interval that excludes 0, so that noisy ones, such
  as most dummies for a missing answer or an effect resting on few experts in the small arm, do not count. Still, a test that adds terms of real size will nearly always pass the step.
- Absolute fit has little power: RMSE by attribute level was 0.19 pp or less in all models, including wrong ones, as a model fits the cells of its own dummies by construction.
- TIC can mislead: it favoured (6p) where the truth says no, as tr(H^-1 B) falls below K. It is not in the rule, but its verdict column could misdirect a reader. The condition number depends on the units of the regressors.
- Proposals are per test. A chain of tests can conflict, and the rule does not reconcile it. A Monte Carlo of the rule's error rates has not been run.

**Estimation and AMEs**

- Fixed prediction draws mean the Krinsky-Robb spread leaves out simulation error in the probabilities; simulated-ML error in theta is not separated either.
- Krinsky-Robb on the Cholesky parameters skews SDs and correlations (oil's SD of 0.5 near the low end of 0.35 to 1.23). Prefer the percentile intervals there.
- Group covariances in (6pe) are weakly identified in small groups, and clustered SEs rest on few experts there. Correlations of the constants were not recovered in the small arm of the synthetic run (RMSE 0.32).
- Prices are mean-imputed with a dummy and degree days without one, which is biased unless blanks are random. Term shares are centred on both arms.
- The effects are many (630 rows in (4pe)), so multiple testing applies, and they sum to zero across outcomes, so they are not independent.
- The correlated mixed logit is not invariant to the reference outcome (a 4.5 log-point gap in October). All tests are at one reference outcome.
- Cells with one or two choices pass the stop of `fit_mixed_logit()` and give very large SEs. Synthetic runs use 100 estimation and 200 Krinsky-Robb draws, so coverage there says little about a real run.

**Cleaning**

- With a factor of 5, the price rule keeps answers from a fifth to five times the median (oil in the synthetic run: 2.2 to 55 cent per kWh), so some answers far from any real price stay. They enter the means of the reference values, the quartiles that set the price
  changes of the AMEs, and the linear price terms of the models.
- Two pools stand side by side in the appendix: the price median uses completers with a group, the outlier rule everyone who answered. Shares are used as reported where they do not sum to 100.

**Summary statistics**

- Rates: the numerator counts an invitation once and the denominator an address once, so someone who answers in two waves counts twice and state and group rates can overstate. Wave rates are per invitation and do not compare with them. Rates assume delivery: bounces
  are unknown, 2 batches were only partly sent, t-online blocking is unmeasured, and the rates without t-online are upper bounds. Those who leave before Q0 or Q2 have no state or group, while the frame keeps everyone, so these rates are low by a different amount in each
  cell. An address on two lists sits on the first by row order, so that group's denominator is arbitrary.
- Selection: about 150 unadjusted tests per version; Pearson per answer level, rough for rare answers; Welch on skewed counts with small N; `hdd` and state are clustered by region, which the tests ignore. `selection_vignettes` pools experts with one to six answers, and
  break-offs' vignettes come from early positions only. Reached-conditional comparisons compare completers with survivors among break-offs, and N falls with position.
- Questions: barrier and Q8 denominators include experts who submitted the page with no tick, so shares are too low if the question was not mandatory. N differs by question: Q20 rests on experts near the end, Q6a on nearly everyone. Q10b and Q11b means are per firm,
  unweighted. A blank in Q15 is not non-response: it can mean no client uses the fuel. Durations are untrimmed, and section sums do not add up to the total time.
- Vignettes: balance p-values are seven tests with no correction, treating vignettes as independent, on answered vignettes only. Pooled shares weight experts by vignettes answered. The G-test ignores the order of the answers, fuels are correlated within experts, and those answering on district heating are selected. The "No recommendation" cost bar has an unclear meaning.
- Figures: Q11c spelling variants can split a manufacturer and push it below 10, and the question refers to a different technology for each expert. Hidden bins leave gaps in small groups that look like real features; depth 1 to 3 score paths fall on bin edges. Quartiles of the Q28 weights are mostly 0, so those columns say little. State map means rest on as few as 10 answers, and the colour ramp stretches small differences.

## 5. Identification issues

- **Arm:** routed by Q6a, not randomised; "Weiß nicht" goes to the arm without the option. Comparisons between the two scripts are descriptive, the rule can pick different models per arm, and comparing across arms mixes model and sample. Reference values pool both arms.
- **Only the vignette attributes are randomised.** Prices, expected rises, a heat pump at home and degree days are between-expert and possibly tied to the random constants, so their "effects" are associations. Prices and expectations are reported after the vignettes, so they may be shaped by them, and blanks are selective. Levels against logs is confounded with measurement error and the unit rules.
- **Design:** no house built in 2000 is unrenovated or has oil heating. Oil is identified only from 1970 houses, and "built in 2000" only from gas houses at 70 or 120 kWh. An AME that moves to those cells extrapolates, and `at` or a change can do so without a message. Raw shares by heating, year and demand mix these attributes.
- **Sample:** completers only, so selected on persistence; "completed first" too. Block C compares only on what break-offs had answered, among those who got that far, and says nothing about non-starters. A break-off at a vignette may depend on the attributes shown, which
  would threaten internal validity. This is not tested here, but the design of unanswered vignettes is in the data, so it could be. Cross-validation generalises only to the selected sample of those who answered.
- **Attempts:** the arm depends on which attempt went furthest for a person with attempts in both; the kept attempt can be a second exposure; a token is per survey, so someone invited in two waves counts as two people.
- **Expert group:** self-reported in Q2 with a priority order, so a group estimate mixes functions and group differences are not causal. Free text counts as advisers; the eight completers without a group leave the reference values. By-group rates mix response with classification and cannot be separated without token lists. Group versions condition on having reached Q2.
- **Covariances by group** may reflect response scale or consistency rather than preference heterogeneity, as the logit scale is 1 in every group. Only the constants are random, so unmodelled heterogeneity in attribute effects can show up as differences in the constants. Experts with one technology throughout have no within variation.
- **Other:** `hdd` is at the firm's postcode for one year, not at the vignette household, and service areas can be nationwide. State rates may reflect list composition, and state maps the group mix. Position mixes fatigue and selection in samples with break-offs. Cost by technology compares different houses, as the expert chose the technology for that house. The soft launch differs in Q6 and Q7 and is pooled.

## 6. Alternative specifications, not run

- **Models:** from the old section 3, not in this first version: (2), (4i), (6i), (6p), (4ip), (4pa), (4pd), (4pl). Uncorrelated constants; another reference outcome; dropping `Keine Empfehlung`; a scale-heterogeneity model by group as a competitor to covariances by group; (4p) with `start_from = m4` (it starts from scratch, as in the old code).
- **Terms:** expected rises as ordered categories or only "mehr als 10% höher"; the match of every heating at home; time and risk scores or age as covariates; a different COP by heat distribution, another hybrid share; a dummy for missing degree days, or a mean over several years.
- **Effects:** averaged over the sample's states instead of at one reference state; other reference states and profiles.
- **Model comparison:** re-estimate on a few fresh sets, or more draws; a critical value at Holm's level, a small-sample correction of the clustered variance, or a parametric bootstrap of the LR for (4pe) against (6pe); the exact tail with Imhof (`CompQuadForm` is in `renv.lock`); Shi's (2015) non-degenerate Vuong test, Clarke's sign test, a variance pre-test; repeated folds, a two-SE bar; an AME rule based on the median, the share of rows moved or the SE, or restricted to policy-relevant effects; absolute fit on cells a model does not target, such as how often an expert repeats a technology; a model confidence set (Hansen et al. 2011).
- **Cleaning:** attempts: first completed instead of furthest, drop persons with several attempts or attempts in both arms. Outlier  rule: the old pool, within groups, an upper bound for the counts. Prices: `PRICE_VALID_FACTOR` of 3. Groups: advice first, free text as its own group, several indicators instead of one group. Shares rescaled to 100. Reference values by arm, or without the soft launch.
- **Summary statistics:** one joint test per question, Fisher's exact test for rare answers, Holm or Benjamini-Hochberg, one model of completion on all firm variables; break-offs by stage, or a hazard per question next to the share of all break-offs; non-response weights to the frame by list and state; exact rates by list type, if `Firmen` is the SHK list and `Personen` DENA plus chimney sweeps; recommendation shares at a fixed position; a balance test clustered by expert, and over all drawn vignettes; Q7 in main waves only; Q6a without "Don't know"; barriers without experts who ticked nothing; recommendations for experts with all six vignettes; Q10b
  weighted by households advised; Q11c from a fixed list of manufacturers; coarser score bins in groups; state maps adjusted for group composition; Q17 with Kruskal-Wallis and Wilcoxon, as in the old table.

## 7. Rules that differ from the old code

Each can be reversed. `tests/test_cleaning_equivalence.R` lists by name the comparisons that the decisions of section 1 change on purpose, checks that everything else still equals the old code, and tests the new rules on data that sets them off.

- `experts.rds` has one row per person, and repeat completions count once. The outlier rule pools everyone who answered, and `age` follows it. The decisions of section 1 on prices, outliers, shares, Q20 and ties.
- `data_quality` loses its column of examples, which held individual answers. The limits of the trimmed tails are not reported. Q2 combinations below 10 persons are pooled within their group. A minimum-cell stop guards the expected rises.
- A count of answers is hidden below 10 answers or 30 contacted. The break-off map also needs 10 completed. Group and arm columns below the minimum cell are dropped. Vignette exhibits count distinct experts for the minimum cell.
- Q20 shares are over experts who got as far as Q20, so "No answer" no longer includes break-offs. Completers count as having reached every question.
- Score bins are 1/16, not 1/32, as shorter paths' midpoints fall on the 1/32 edges. Q11c shows the top 10 with at least 10 mentions (old: top 15). Q17 keeps only the G-test. Q7 pools all waves, with a caveat in the note.
- Not ported, as not asked for: the funnel figure, the denominator map, the state by group tiles, the t-online comparison figure, the `lastpage` plot; the mean constants at the neutral state and the column `is_affected` of the term effects.

## 8. Known limits of the code

- `mlogit:::halton` and `make.random.nb` are internal to mlogit, so an update could break them. A model with a single random constant (two outcomes) would fail in `post_estimation()`; it cannot occur here.
- A fold of (6pe) at 1,000 draws needs an estimated 3 to 4 GB, unmeasured, and the cross-validation runs four at once. A session that dies stops `compare_models()`, but the folds that ended are saved, and the next run resumes with them. The Hessian sends about 250 MB
  to each worker, by an estimate.
- `model$model$probabilities` has its values in a different order from the rows, as mlogit writes the reference outcome first: use the matrix `model$probabilities`. A model evaluated again at code 2 keeps `iterlim = 0` in its saved call.
- A new `label` needs `post_estimation(force = TRUE)`, and a name of effects no longer used stays in the results until then. If a dummy is deleted from a formula, its `rise_na_<fuel>` change must go too.
- A workbook open in Excel cannot be written; the next run writes it again. A rerun keeps a filled `final_decision` in a dated copy and does not merge it back. An old model folder on the same data can flag a newer model as stopped short.
- `att_*` combinations outside the design pass `make_regressors()` if each level is known.
- `compare_models.R` has about 1,550 lines. Moving its step 6 into a function of its own would make the main function clearer rather than shorter.
- The deck's height limit is a rule of thumb (120 characters per note line, 105 per title line), and a far longer note could overflow. A LaTeX error in one table reports the whole deck as failed. `kableExtra` writes an invisible `\vphantom{1}` into repeated rows of `tab_clean_prices.tex`. Workbook widths and tab colours were read with `readxl` and from the files' XML, never seen in Excel.
- The simulation has no repeat completions, engineering-only offices or euro-per-litre answers, and only two `bundesland_source` values; `test_cleaning_equivalence.R` adds the first three in a stressed copy. One part of `test_estimation_equivalence.R` retypes old
  section 2.1 rather than pasting it, so that part cannot detect a difference.
