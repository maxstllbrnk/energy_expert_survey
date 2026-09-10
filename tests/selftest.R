# =============================================================================
# tests/selftest.R  -  check the cleaned data sets after a run
# =============================================================================
# Run AFTER R/run_cleaning.R:   Rscript tests/selftest.R
#
# These are the checks that would fail if the soft-launch waves were merged
# incorrectly - the failure mode the old pipeline had. The strongest one is the
# vignette_id match: it only resolves if all nine design attributes landed in
# the right columns, which cannot happen by chance if columns are misaligned.
# =============================================================================

source("config.R")
source("R/cleaning/00_functions.R")

vl <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes_long.rds"))
sm <- readRDS(file.path(CLEAN_DATA_DIR, "survey_main.rds"))
versand <- load_versand()

failed <- 0L
ok <- function(label, cond, extra = "") {
  passed <- isTRUE(cond)
  cat(sprintf("%-58s %s%s\n", label, if (passed) "PASS" else "*** FAIL ***",
              if (nzchar(extra)) str_c("  ", extra) else ""))
  if (!passed) failed <<- failed + 1L
}

cat("\n===== waves =====\n")
ok("all 12 waves present", n_distinct(sm$wave) == 12, str_c(n_distinct(sm$wave), " waves"))
ok("both soft-launch waves present", sum(versand$is_softlaunch) == 2)
ok("every wave has respondents", all(table(sm$wave) > 0))
ok("soft launch flagged from Versandrunde, not hardcoded",
   all(sm$is_softlaunch == (sm$versandrunde == "Soft Launch")))

cat("\n===== keys =====\n")
ok("resp_uid unique in survey_main", !anyDuplicated(sm$resp_uid))
ok("every vignette row joins to survey_main", all(vl$resp_uid %in% sm$resp_uid))
ok("every respondent appears in vignettes_long", all(sm$resp_uid %in% vl$resp_uid))
ok("6 vignette rows per respondent (except arm conflicts)",
   all(table(vl$resp_uid[!vl$arm_conflict]) == 6))

cat("\n===== soft-launch alignment (the old pipeline's failure mode) =====\n")
by_group <- vl |> summarise(matched = mean(!is.na(vignette_id)), .by = is_softlaunch)
ok("every vignette row matches a design combination (main)",
   isTRUE(all.equal(by_group$matched[!by_group$is_softlaunch], 1)))
ok("every vignette row matches a design combination (soft launch)",
   isTRUE(all.equal(by_group$matched[by_group$is_softlaunch], 1)))
ok("all 128 design combinations appear", n_distinct(vl$vignette_id) == 128)

lv <- function(x) levels(droplevels(x))
for (v in c("dem_own_heating", "dem_education", "co2_heard", "resp_is_owner")) {
  ok(str_c("same answer options in both wave groups: ", v),
     setequal(lv(sm[[v]][sm$is_softlaunch]), lv(sm[[v]][!sm$is_softlaunch])) ||
       all(lv(sm[[v]][sm$is_softlaunch]) %in% lv(sm[[v]][!sm$is_softlaunch])))
}
ok("design attributes have the same range in both groups",
   identical(range(as.numeric(vl$att_build_year[vl$is_softlaunch]), na.rm = TRUE),
             range(as.numeric(vl$att_build_year[!vl$is_softlaunch]), na.rm = TRUE)))

cat("\n===== soft-launch harmonisation =====\n")
ok("cpd_other_text (Q24a) is NA in the soft launch", all(is.na(sm$cpd_other_text[sm$is_softlaunch])))
ok("cpd_other_text is present in the main waves", any(!is.na(sm$cpd_other_text[!sm$is_softlaunch])))
ok("cust_rank6 exists only in wave 334335",
   "cust_rank6" %in% names(sm) && all(is.na(sm$cust_rank6[sm$wave != "334335"])))
ok("'weiss nicht' removed from every cust_rank*",
   !any(map_lgl(str_subset(names(sm), "^cust_rank"),
                ~ any(as.character(sm[[.x]]) == "weiß nicht", na.rm = TRUE))))

cat("\n===== vignette module =====\n")
ok("'Keine Empfehlung' is a level of vig_rec", "Keine Empfehlung" %in% levels(vl$vig_rec))
ok("'Keine Empfehlung' occurs", sum(vl$vig_rec == "Keine Empfehlung", na.rm = TRUE) > 0)
ok("Fernwaerme never recommended in the nofw arm",
   !any(vl$vig_rec[vl$vig_arm == "nofw"] == "Fernwärme", na.rm = TRUE))
ok("Fernwaerme does occur in the fw arm",
   any(vl$vig_rec[vl$vig_arm == "fw"] == "Fernwärme", na.rm = TRUE))
ok("investment cost only on the last vignette",
   all(is.na(vl$vig_cost[vl$vig_num != N_VIGNETTES])) &&
     any(!is.na(vl$vig_cost[vl$vig_num == N_VIGNETTES])))
ok("arm is constant within a respondent (bar conflicts)",
   all(vl |> filter(!arm_conflict) |> summarise(k = n_distinct(vig_arm), .by = resp_uid) |> pull(k) == 1))

cat("\n===== response times =====\n")
ok("no colliding time_NA column", !"time_NA" %in% names(sm))
ok("no duplicate column names", !anyDuplicated(names(sm)) && !anyDuplicated(names(vl)))
ok("array-question timings kept separate",
   all(c("time_barr_ren", "time_barr_dh", "time_barr_conv", "time_price",
         "time_cust_rank") %in% names(sm)))
ok("time_interview present and numeric",
   "time_interview" %in% names(sm) && is.numeric(sm$time_interview))
ok("vig_time is numeric and non-negative",
   is.numeric(vl$vig_time) && all(vl$vig_time >= 0, na.rm = TRUE))
ok("no per-vignette timings left in survey_main",
   !any(str_detect(names(sm), "^(time_vig[0-9]|grouptime_vig[0-9])")))

cat("\n===== separation of the two data sets =====\n")
ok("no vignette or attribute columns in survey_main",
   !any(str_detect(names(sm), "^(vig[0-9]_|att_|vig_intro$)")))
# Count rows by parsing the exports, not by counting lines: free-text answers
# contain embedded newlines, so a line count overstates the number of rows.
raw_rows <- list.files(RAW_DIR, "^survey_[0-9]+_R_data_file\\.csv$",
                       recursive = TRUE, full.names = TRUE) |>
  map_int(~ nrow(read_raw_wave(.x)$data)) |>
  sum()
ok("survey_main keeps every respondent, none dropped",
   nrow(sm) == raw_rows, str_c(nrow(sm), " cleaned vs ", raw_rows, " raw"))

cat("\n===== recodes =====\n")
ok("plz is 5 characters where valid",
   all(nchar(sm$plz[!is.na(sm$plz)]) == 5))
ok("plz recovers leading zeros", any(str_starts(na.omit(sm$plz), "0")))
ok("firm_radius_recoded never loses an original answer",
   all(!is.na(sm$firm_radius_recoded[!is.na(sm$firm_radius)])))
ok("recode source is recorded for every respondent",
   all(!is.na(sm$firm_radius_recode_source)))

cat("\n===== bundesland =====\n")
if (!file.exists(PLZ_LOOKUP_FILE)) {
  cat("skipped - build the lookup first: Rscript R/geodata/build_plz_bundesland.R\n")
} else {
  ok("bundesland has all 16 federal states as levels", nlevels(sm$bundesland) == 16)
  ok("a source is recorded for every respondent", all(!is.na(sm$bundesland_source)))
  ok("bundesland is set exactly when a source found one",
     all(is.na(sm$bundesland) == sm$bundesland_source %in% c("no_postcode", "unmatched")))
  ok("no respondent with a valid plz is left unexplained",
     !any(is.na(sm$plz) & sm$bundesland_source != "no_postcode"))
  # The lookup is only useful if it covers the postcodes people actually gave.
  cov <- mean(!is.na(sm$bundesland[!is.na(sm$plz)]))
  ok("at least 99% of valid postcodes resolve to a state", cov >= 0.99,
     str_c(sprintf("%.2f%%", 100 * cov), " of ", sum(!is.na(sm$plz))))
  # Deutsche Post assigns postcodes geographically, so the leading digit tracks
  # the state closely. A rebuild that shifted the state names or the AGS
  # prefixes would break this long before anyone noticed it in a table.
  region_ok <- sm |>
    filter(!is.na(bundesland), bundesland_source %in% c("plz", "plz_border")) |>
    mutate(plz_area = str_sub(plz, 1, 2)) |>
    summarise(top = max(table(droplevels(bundesland))) / n(), .by = plz_area) |>
    pull(top)
  ok("each 2-digit postcode area is dominated by one state",
     mean(region_ok) > 0.9, str_c("mean share ", sprintf("%.3f", mean(region_ok))))
}

cat("\n===== preference staircases =====\n")
# The scores are built outside the pipeline, but the RAW answers live here and
# the two properties everything downstream rests on can be checked on them.
for (p in c("tp_", "risk_")) {
  cols <- str_subset(names(sm), str_c("^", p, "[0-9]"))
  lvl  <- as.integer(str_sub(str_remove(cols, p), 1, 1))
  pth  <- str_to_upper(str_sub(str_remove(cols, p), 2))
  ok(str_c(p, "* columns are all two-level factors"),
     all(map_lgl(cols, ~ is.factor(sm[[.x]]) && nlevels(sm[[.x]]) == 2)))
  # Option A must stay the first level: the score reads the choice off the level
  # index, so a reversed export would invert the whole measure silently.
  first_lab <- map_chr(cols, ~ levels(sm[[.x]])[1])
  ok(str_c(p, "* keep option A as the first level"),
     if (p == "tp_") all(str_detect(first_lab, "heute"))
     else all(str_detect(first_lab, "^Verlosung")))
  # The staircase only orders respondents if the amount offered rises
  # monotonically with the number of "A" answers so far, at every level.
  amt <- map_dbl(cols, ~ as.numeric(str_extract(levels(sm[[.x]])[2], "[0-9]+")))
  mono <- map_lgl(sort(unique(lvl[lvl > 1])), function(k) {
    i <- lvl == k
    code <- map_int(pth[i], ~ Reduce(function(a, b) a * 2L + as.integer(b == "A"),
                                     str_split_1(.x, ""), 0L))
    isTRUE(all.equal(cor(code, amt[i], method = "spearman"), 1))
  })
  ok(str_c(p, "* offers rise monotonically along the branch"), all(mono))
  # Nobody may answer two branches of the same level: the path would be undefined.
  per_level <- map(sort(unique(lvl)), ~ rowSums(!is.na(sm[, cols[lvl == .x], drop = FALSE])))
  ok(str_c(p, "* one answer per level at most"),
     all(map_lgl(per_level, ~ all(.x <= 1))))
}
ok("the time staircase is missing exactly its two known level-5 arms",
   setdiff(str_c("tp_5", c("aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb",
                           "baaa","baab","baba","babb","bbaa","bbab","bbba","bbbb")),
           names(sm)) |> identical(c("tp_5abba", "tp_5abbb")))
ok("the risk staircase has all 16 level-5 arms",
   sum(str_detect(names(sm), "^risk_5")) == 16)

cat(str_c("\n================= ",
          if (failed == 0) "ALL CHECKS PASSED" else str_c(failed, " CHECK(S) FAILED"),
          " =================\n"))
if (!interactive()) quit(status = if (failed == 0) 0 else 1)
