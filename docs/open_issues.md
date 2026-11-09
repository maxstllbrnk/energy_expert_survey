# Open issues of the new pipeline

What the rebuild of November 2026 left open, for the researchers to decide or to keep in mind. Nothing here
has been acted on unless it says so. Numbers from "the synthetic run" are from simulated data with one seed:
they show how the code behaves, not what the survey says.

## 1. Decisions, with what the code does until they are made

### Model comparison

1. **Does the 1 percentage point step count the effects a larger model adds?** It counts only effects that both
   models define. So in (4) against (4p) the price effects themselves do not count, and the test can end in "keep
   smaller as main" although the new terms move probabilities by far more than 1 pp. In the synthetic run, where
   prices do matter, this is what happened: largest shared change 0.29 pp. A natural alternative is to treat an added
   effect as moving from 0, or to report that count next to the current one.
2. **Should all questions use one parsimony gate?** Only (4pe) against (6pe) is cross-validated. The other two
   questions fall back on BIC with experts as N, which is very strict: in the synthetic run it rejected (4pe) against
   (4p), +81, where the truth has those terms. Cross-validating (4) and (4p) too takes ten more estimations per script.
3. **Gaps in the rule that the code fills, to confirm:**
   - a test that says yes while the gain fails on fresh draws gives "keep smaller: gain does not survive fresh draws";
   - if a fold failed or did not converge, BIC decides, as when cross-validation was not run;
   - a term without variation in a fold, such as a rare dummy among the few experts of a small arm, stays at its full-sample
     estimate, a small leak from the held-out experts if they hold it. Without this, 2 of 5 folds failed in the small arm of the
     synthetic run;
   - cross-validation favours a model only if its gain exceeds one paired SE;
   - the fresh-draw critical value is at `alpha`, not at Holm's level;
   - validity limits: scaled gradient at most 0.1, condition number at most 1e8;
   - AME changes are also reported relative to the SE in the smaller model, and compared only at an identical
     reference state and for effects both models define alike;
   - tests with an invalid model count as not rejected and stay in the Holm family;
   - `cross_validation.rds` holds one log-likelihood per expert, next to `model.rds` on the secure drive.

### The minimum cell

4. **Does the minimum cell cover the numerator of a share?** At present it covers the denominator: 2.2% of 277 vignettes,
   6 vignettes, stands in a table, and N and share together give the numerator. Covering numerators would blank many rows.
5. **Is a hidden cell that can be recovered by subtraction acceptable?** A single hidden state or wave follows from the
   total minus the visible rows; a hidden arm from the other arm; break-offs from started minus completed; a small t-online
   count from invited minus invited without t-online; not answered from asked minus answered. Nothing prevents this now.
6. **Which bare counts may be small?** The rule of 2026-10-29 says "counts may be smaller, e.g. how many answers a cleaning step
   changed". The code reads this strictly: counts of what the cleaning did stay (further attempts, answered in both arms, no
   usable postcode, without a group), and other bare counts are hidden below 10 (who stopped where, who was asked, the rows of
   `meta`). `invited` is always shown, as it counts contacts rather than answers. Relaxing any of these takes one line.
7. **Does a coefficient count as a statistic?** A dummy held by three experts has a coefficient, and nothing checks that.
   `results$choice_counts` in `results.rds` is also not blanked below the minimum cell; only the sheet is.
8. **`mean_before` of a price** is blank if the rules changed fewer than 10 answers, but one typo can still dominate it: in a test
   a typed 1,000,000 gave a mean before of 1,014, from which the typo can be roughly recovered. Dropping the column takes one line.

### The cleaning

9. **`heat_pump_at_home` is 0, and `own_heating` "Keine Angabe", for everyone who stopped before Q20.** The old code covered
   completers only. The estimation scripts use completers, so they are not affected, but a sample with break-offs would treat these
   as "no heat pump". Recommended: `NA` where Q20 was not reached. The sections of the summary statistics already work around it.
10. **A share outside 0 to 100 stops the cleaning,** now over everyone rather than completers of the three groups. One such answer
    from a break-off stops the first real run at step 06, with a clear message. The alternative is to set it to missing and count it.
11. **Oil in euro per litre.** Answers from 1.00 to 1.40 are not caught by the euro rule and become "not usable", while 0.40 to 0.99 are
    converted. Such prices are plausible in January 2026. `PRICE_VALID_FACTOR` is still "first choice, to be confirmed", and the appendix
    now prints it as the rule.
12. **Tied repeat completions:** the first `resp_uid` as text stays, so `_100` comes before `_99`, which is not chronological. The old function
    did the same but only counted; now it decides which answers a person has. A tie-break by `meta_startdate` is the alternative. The seminar's
    sample flow shows no repeat completions, so this matters only if new data bring some.

### The summary statistics

13. **`selection` is split into five tables,** because all rows in one do not fit a page: `selection` and `selection_prices` (main, all experts),
    `selection_state`, `selection_later`, `selection_barriers` (appendix). Does `selection_prices` belong in the main text?
14. **Wording for the paper:** an "AME" here is a discrete change at one reference expert and vignette, not an average over the sample.

## 2. To check on the first real run

- **Q11c:** read the panel of manufacturers. `NOT_A_BRAND` in `section_market.R` cannot be complete: another word with at least 10 mentions would
  reach the table. `str_to_title()` also rewrites brand names, BWT to Bwt.
- **The sample.** A person who completed twice now counts once, while the old models counted both. The seminar's sample flow shows 0 repeat completions, so
  the estimation sample and the reference values should not change. The sheet `attempts` of the cleaning report confirms this: if it shows any, the old log-likelihoods no longer apply.
- **Stops that may fire, each naming what to do:** `fit_mixed_logit()` stops on a term with no finite estimate, for any two-valued term in one alternative.
  This is broader than the old code and may fire for (4pe) or (6pe) in the small arm, e.g. `oil_rise_na` if no expert with a blank recommends oil. `post_estimation()`
  stops on a Hessian that is not positive definite. `tests/selftest.R` keeps the old check "one answer per level at most", which fails if the real data hold abandoned
  branches, not because of a bad merge; its branches for the real data have never run.
- **(6pe) in the small arm.** In the synthetic run the estimates for chimney sweeps and oil were not credible: constant -10.5 (SE 4.7), SD 6.3, on 32 experts. It passed every
  check, with a condition number of 1.1e5.
- **Frame rows without an address** collapse into one person, and `load_frame()` reports how many. Should they count as invited?

## 3. Statistical issues

**Model comparison**

- The robust test can over-reject in the group-covariance test. On the synthetic data with equal covariances, the mean weight is 0.83 and the robust p below the classical p. The seminar's (4pe) against (6pe)
  shows the same pattern (p 0.005, robust p below 0.001), so that gap may be small-sample bias in the clustered variance rather than misspecification. This rests on one draw.
- **The robust test can reject almost anything if a coefficient rests on few experts.** In a test with a dummy held by 3 of 300 experts, the mean weight is 0.06 and the robust critical value 0.2, against 3.84 for chi-squared: LR 5.4, classical p 0.020, robust p below 0.001. (Those 3 experts were picked for having chosen the technology, so the LR itself says nothing; the weight is the point.) A clustered variance is known to collapse with few treated clusters. So check the rare dummies, such as the blanks in Q17 in the small arm: `mean_weight` far below 1 in the sheet `nested_tests` is the sign. The fresh-draw gate does not help here, as it uses the same weights.
- The LR on the estimation's draws is inflated with many constants and few draws: 61.2 against 39.9 on fresh draws with 6 constants and 100 draws. With 3 constants there is no inflation.
- The fresh-draw gate will bind less at 1,000 draws, so cross-validation will carry more of the decision. The mean fresh LR is biased downward, as the parameters were tuned on other draws.
- The gate's critical value is lower than the bar the test cleared: it is not adjusted for Holm, and weights below 1 lower it further (50.2 against 58.1 for chi-squared with 42 df).
- One paired SE is a low bar, and the SE is rather too small, as the folds share experts in training. Folds start from the full-sample estimates, so held-out experts influence which optimum a fold
  reaches; training on 4/5 of the sample works the other way, against larger models.
- Without usable cross-validation, Vuong effectively decides alone, as z and the BIC difference share their sign by construction. Vuong's normal limit is for strictly non-nested models, sets that
  share terms overlap, and no variance pre-test is run.
- Holm depends on the tests declared and runs per call, so per arm. (4p) against (6p), robust p 0.034, no longer rejects once a second nested test is declared and its p-value is the smaller or larger (0.068).
- The AME rule uses the largest of many rows against a fixed 1 pp. With hundreds of rows and noisy small groups, "keep smaller as main" may almost never fire among shared effects.
- Absolute fit has little power: RMSE by attribute level was 0.19 pp or less in all models, including wrong ones, as a model fits the cells of its own dummies by construction.
- TIC can mislead: it favoured (6p) where the truth says no, as tr(H^-1 B) falls below K. It is not in the rule, but its verdict column could misdirect a reader. The condition number depends on the units of the regressors.
- Proposals are per test. A chain of tests can conflict, and the rule does not reconcile it. A Monte Carlo of the rule's error rates has not been run.

**Estimation and AMEs**

- Fixed prediction draws mean the Krinsky-Robb spread leaves out simulation error in the probabilities; simulated-ML error in theta is not separated either.
- Krinsky-Robb on the Cholesky parameters skews SDs and correlations (oil's SD of 0.5 near the low end of 0.35 to 1.23). Prefer the percentile intervals there.
- Group covariances in (6pe) are weakly identified in small groups, and clustered SEs rest on few experts there. Correlations of the constants were not recovered in the small arm of the synthetic run (RMSE 0.32).
- Prices are mean-imputed with a dummy and degree days without one, which is biased unless blanks are random. Term shares are centred on both arms.
- The effects are many (630 rows in (4pe)), so multiple testing applies, and they sum to zero across outcomes, so they are not independent.
- COP 3 and a hybrid gas share of 0.5 are placeholders. The heat pump and hybrid price terms, and the fuel effects that move them, depend on them.
- The correlated mixed logit is not invariant to the reference outcome (a 4.5 log-point gap in October). All tests are at one reference outcome.
- Cells with one or two choices pass the new stop and give very large SEs. Synthetic runs use 100 estimation and 200 Krinsky-Robb draws, so coverage there says little about a real run.

**Cleaning**

- The outlier rule also trims valid years, which removes genuine oldest and youngest experts. Employees, households and carbon prices have no valid range, so zero or negative answers stay unless they fall in a tail.
  `co2_est_low` and `co2_est_high` are trimmed separately, so a person can keep one bound, and low above high is not looked at.
- Two pools stand side by side in the appendix: the price median uses completers with a group, the outlier rule everyone who answered. Shares are used as reported where they do not sum to 100.

**Summary statistics**

- Rates: the numerator counts an invitation once and the denominator an address once, so someone who answers in two waves counts twice and state and group rates can overstate. Wave rates are per invitation and do not compare with them. Rates assume delivery:
  bounces are unknown, 2 batches were only partly sent, t-online blocking is unmeasured, and the rates without t-online are upper bounds. Those who leave before Q0 or Q2 have no state or group, while the frame keeps everyone, so these rates are low by a different amount in each cell.
  An address on two lists sits on the first by row order, so that group's denominator is arbitrary.
- Selection: about 150 unadjusted tests per version; Pearson per answer level, rough for rare answers; Welch on skewed counts with small N; `hdd` and state are clustered by region, which the tests ignore. `selection_vignettes` pools experts with one to six answers,
  and break-offs' vignettes come from early positions only. Reached-conditional comparisons compare completers with survivors among break-offs, and N falls with position.
- Questions: barrier and Q8 denominators include experts who submitted the page with no tick, so shares are too low if the question was not mandatory. N differs by question: Q20 rests on experts near the end, Q6a on nearly everyone. Q10b and Q11b means are per firm, unweighted.
  A blank in Q15 is not non-response: it can mean no client uses the fuel. Durations are untrimmed, and section sums do not add up to the total time.
- Vignettes: balance p-values are seven tests with no correction, treating vignettes as independent, on answered vignettes only. Pooled shares weight experts by vignettes answered. The G-test ignores the order of the answers, fuels are correlated within experts, and those answering on
  district heating are selected. The "No recommendation" cost bar has an unclear meaning.
- Figures: Q11c spelling variants can split a manufacturer and push it below 10, and the question refers to a different technology for each expert. Hidden bins leave gaps in small groups that look like real features; depth 1 to 3 score paths fall on bin edges. Quartiles of the Q28 weights
  are mostly 0, so those columns say little. State map means rest on as few as 10 answers, and the colour ramp stretches small differences.

## 4. Identification issues

- **Arm:** routed by Q6a, not randomised; "Weiß nicht" goes to the arm without the option. Comparisons between the two scripts are descriptive, the rule can pick different models per arm, and comparing across arms mixes model and sample. Reference values pool both arms.
- **Only the vignette attributes are randomised.** Prices, expected rises, a heat pump at home and degree days are between-expert and possibly tied to the random constants, so their "effects" are associations. Prices and expectations are reported after the vignettes, so they may be shaped by them,
  and blanks are selective. Levels against logs is confounded with measurement error and the unit rules.
- **Design:** no house built in 2000 is unrenovated or has oil heating. Oil is identified only from 1970 houses, and "built in 2000" only from gas houses at 70 or 120 kWh. An AME that moves to those cells extrapolates, and `at` or a change can do so without a message. Raw shares by heating, year and demand mix these attributes.
- **Sample:** completers only, so selected on persistence; "completed first" too. Block C compares only on what break-offs had answered, among those who got that far, and says nothing about non-starters. A break-off at a vignette may depend on the attributes shown, which would threaten internal validity. This is
  not tested here, but the design of unanswered vignettes is in the data, so it could be. Cross-validation generalises only to the selected sample of those who answered.
- **Attempts:** the arm depends on which attempt went furthest for a person with attempts in both; the kept attempt can be a second exposure; a token is per survey, so someone invited in two waves counts as two people.
- **Expert group:** self-reported in Q2 with a priority order, so a group estimate mixes functions and group differences are not causal. Free text counts as advisers; the eight completers without a group leave the reference values. By-group rates mix response with classification and cannot be separated without token lists.
  Group versions condition on having reached Q2.
- **Covariances by group** may reflect response scale or consistency rather than preference heterogeneity, as the logit scale is 1 in every group. Only the constants are random, so unmodelled heterogeneity in attribute effects can show up as differences in the constants. Experts with one technology throughout have no within variation.
- **Other:** `hdd` is at the firm's postcode for one year, not at the vignette household, and service areas can be nationwide. State rates may reflect list composition, and state maps the group mix. Position mixes fatigue and selection in samples with break-offs. Cost by technology compares different houses, as the expert chose the
  technology for that house. The soft launch differs in Q6 and Q7 and is pooled.

## 5. Alternative specifications, not run

- **Models:** from the old section 3, not in this first version: (2), (4i), (6i), (6p), (4ip), (4pa), (4pd), (4pl). Uncorrelated constants; another reference outcome; dropping `Keine Empfehlung`; a scale-heterogeneity model by group as a competitor to covariances by group; (4p) with `start_from = m4` (it starts from scratch, as in the old code).
- **Terms:** expected rises as ordered categories or only "mehr als 10% höher"; the match of every heating at home; time and risk scores or age as covariates; a different COP by heat distribution, another hybrid share; a dummy for missing degree days, or a mean over several years.
- **Effects:** averaged over the sample's states instead of at one reference state; other reference states and profiles.
- **Model comparison:** re-estimate on a few fresh sets, or more draws for the shortlist; a critical value at Holm's level, a small-sample correction of the clustered variance, or a parametric bootstrap of the LR for (4pe) against (6pe); the exact tail with Imhof (`CompQuadForm` is in `renv.lock`); Shi's (2015) non-degenerate Vuong test, Clarke's sign test, a variance pre-test; repeated folds, a two-SE bar; an AME rule based on the median, the share of rows moved or the SE, or restricted to policy-relevant effects; absolute fit on cells a model does not target, such as how often an expert repeats a technology; a model confidence set (Hansen et al. 2011).
- **Cleaning:** attempts: first completed instead of furthest, drop persons with several attempts or attempts in both arms. Outlier rule: the old pool, within groups, only the valid range, valid ranges for employees, households and carbon prices. Prices: `PRICE_VALID_FACTOR` of 2 or 5, a euro band for oil up to 1.40. Groups: advice first, free text as its own group, several indicators instead of one group. Shares rescaled to 100. Reference values by arm, or without the soft launch.
- **Summary statistics:** one joint test per question, Fisher's exact test for rare answers, Holm or Benjamini-Hochberg, one model of completion on all firm variables; break-offs by stage, or a hazard per question next to the share of all break-offs; non-response weights to the frame by list and state; exact rates by list type, if `Firmen` is the SHK list and `Personen` DENA plus chimney sweeps; recommendation shares at a fixed position; a balance test clustered by expert, and over all drawn vignettes; Q7 in main waves only; Q6a without "Don't know"; barriers without experts who ticked nothing; recommendations for experts with all six vignettes; Q10b weighted by households advised; Q11c from a fixed list of manufacturers; coarser score bins in groups; state maps adjusted for group composition; Q17 with Kruskal-Wallis and Wilcoxon, as in the old table.

## 6. Rules that differ from the old code

Each was decided while building, and each can be reversed.

- `experts.rds` has one row per person, and repeat completions count once. The outlier rule pools everyone who answered, and `age` follows it.
- `data_quality` loses its column of examples, which held individual answers. The limits of the trimmed tails are not reported. Q2 combinations below 10 persons are pooled within their group. A new minimum-cell stop guards the expected rises.
- A count of answers is hidden below 10 answers or 30 contacted. The break-off map also needs 10 completed. Group and arm columns below the minimum cell are dropped. Vignette exhibits count distinct experts for the minimum cell.
- Q20 shares are over experts who got as far as Q20, so "No answer" no longer includes break-offs. Completers count as having reached every question.
- Score bins are 1/16, not 1/32, as shorter paths' midpoints fall on the 1/32 edges. Q11c shows the top 10 with at least 10 mentions (old: top 15). Q17 keeps only the G-test. Q7 pools all waves, with a caveat in the note.
- Not ported, as not asked for: the funnel figure, the denominator map, the state by group tiles, the t-online comparison figure, the `lastpage` plot; the mean constants at the neutral state and the column `is_affected` of the term effects.

## 7. Known limits of the code

- `mlogit:::halton` and `make.random.nb` are internal to mlogit, so an update could break them. A model with a single random constant (two outcomes) would fail in `post_estimation()`; it cannot occur here.
- A session that dies, e.g. out of memory, stops `compare_models()` and loses the folds of that call; only errors inside a fold are caught. A fold of (6pe) at 1,000 draws needs an estimated 3 to 4 GB, unmeasured. The Hessian sends about 250 MB to each worker, by an estimate.
- `model$model$probabilities` has its values in a different order from the rows, as mlogit writes the reference outcome first: use the matrix `model$probabilities`. A model evaluated again at code 2 keeps `iterlim = 0` in its saved call.
- A new `label` needs `post_estimation(force = TRUE)`, and a name of effects no longer used stays in the results until then. If a dummy is deleted from a formula, its `rise_na_<fuel>` change must go too.
- A workbook open in Excel cannot be written; the next run writes it again. `model_comparison.xlsx` sets no column widths, so widen `reason` by hand. A rerun keeps a filled `final_decision` in a dated copy and does not merge it back. An old model folder on the same data can flag a newer model as stopped short.
- If an element of L sits near 0, the rank check on the restrictions can stop `compare_models()` at the start. `att_*` combinations outside the design pass `make_regressors()` if each level is known.
- `compare_models.R` has about 1,450 lines. The reviewer found only a few dozen removable. Moving its step 6 into a function of its own would make the main function clearer rather than shorter.
- The deck's height limit is a rule of thumb (120 characters per note line, 105 per title line), and a far longer note could overflow. A LaTeX error in one table reports the whole deck as failed. `kableExtra` writes an invisible `\vphantom{1}` into repeated rows of `tab_clean_prices.tex`. Workbook widths and tab colours were read with `readxl` only, never seen in Excel.
- The simulation has no repeat completions, engineering-only offices or euro-per-litre answers, and only two `bundesland_source` values; `test_cleaning_equivalence.R` adds the first three in a stressed copy. One part of `test_estimation_equivalence.R` retypes old section 2.1 rather than pasting it, so that part cannot detect a difference.

## 8. Proposed additions to the standards, for approval

From the reviews of this rebuild; none is in the skill or the profile yet.

- Exceptions to the minimum cell are listed in the profile, with name and date; a comment or a table note cannot create one.
- Two statistics whose difference rests on fewer than the minimum cell count as one statistic on those answers.
- A fingerprint that skips work covers every input, settings and public files too, and is saved only after the output was written without error.
- When a rule or a stop moves to a wider sample, say so and list what it can now hit.
- A share needs 10 answers in its denominator; a count printed on its own needs 10; vignette-based cells count distinct experts.
- A finiteness check covers the reference outcome too. A term leaving a model also leaves its `changes`. `system(intern = TRUE)` signals failure by zero length.
- List the common base functions in the style vocabulary.
