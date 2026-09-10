# =============================================================================
# 04_vignettes.R  -  DATA SET 2: one row per expert x vignette
# =============================================================================
# Six rows per respondent. The two Fernwaerme branches are collapsed into one
# `vig_rec` column plus a `vig_arm` label, because the branch is between-subject:
# Q6a routes each respondent into exactly one of them (AO01/AO02 -> mit FW,
# AO03/AO04 -> ohne FW), which holds for 4,175 of the 4,176 respondents.
#
# Every respondent keeps six rows, including those who broke off before the
# vignettes - the design attributes are known for them either way, and
# `vig_rec` is simply NA. Filter on !is.na(vig_rec) for the choice sample.
# The one respondent who answered in BOTH branches keeps a row per branch and
# carries arm_conflict = TRUE.
#
# Output: CLEAN_DATA_DIR/vignettes_long.{rds,csv,xlsx}
# =============================================================================

# --- which branch was each respondent in? ------------------------------------
# Observed from the answers where possible; otherwise reconstructed from the
# Q6a routing, so that respondents who broke off still get the right branch.
arm_info <- survey_wide |>
  transmute(
    resp_uid,
    arm_conflict,
    arm_observed = vig_arm,
    arm_routed   = case_when(as.integer(firm_dh_in_area) %in% 1:2 ~ "fw",
                             as.integer(firm_dh_in_area) %in% 3:4 ~ "nofw",
                             TRUE ~ NA_character_),
    arm_final    = coalesce(arm_observed, arm_routed),
    arm_source   = case_when(!is.na(arm_observed) ~ "answered",
                             !is.na(arm_routed)   ~ "routed from Q6a",
                             TRUE                 ~ "unknown"),
    # for row selection only: respondents with no branch at all keep one row set
    arm_keep     = coalesce(arm_final, "nofw")
  )

# --- answers, per respondent x vignette x branch (12 rows) -------------------
answers <- survey_wide |>
  select(resp_uid, matches("^vig[1-6]_rec_(fw|nofw)(_other)?$")) |>
  mutate(across(-resp_uid, as.character)) |>
  rename_with(~ str_c(.x, "_answer"), matches("^vig[1-6]_rec_(fw|nofw)$")) |>
  pivot_longer(
    -resp_uid,
    names_to      = c("vig_num", "vig_arm", ".value"),
    names_pattern = "^vig([1-6])_rec_(fw|nofw)_(answer|other)$"
  ) |>
  transmute(resp_uid, vig_num = as.integer(vig_num), vig_arm,
            vig_rec = answer, vig_rec_other = na_if(str_squish(other), ""))

# --- response time and page time, same grain ---------------------------------
times <- survey_wide |>
  select(resp_uid, matches("^time_vig[1-6]_rec_(fw|nofw)$")) |>
  pivot_longer(-resp_uid, names_to = c("vig_num", "vig_arm"),
               names_pattern = "^time_vig([1-6])_rec_(fw|nofw)$", values_to = "vig_time") |>
  mutate(vig_num = as.integer(vig_num))

group_times <- survey_wide |>
  select(resp_uid, matches("^grouptime_vig[1-6]_(fw|nofw)$")) |>
  pivot_longer(-resp_uid, names_to = c("vig_num", "vig_arm"),
               names_pattern = "^grouptime_vig([1-6])_(fw|nofw)$", values_to = "vig_group_time") |>
  mutate(vig_num = as.integer(vig_num))

# --- the nine design attributes ----------------------------------------------
# attribute_1..54 are 6 blocks of 9; block v belongs to vignette v.
attributes_long <- survey_wide |>
  select(resp_uid, matches("^att_v[1-6]_")) |>
  mutate(across(-resp_uid, as.character)) |>
  pivot_longer(-resp_uid, names_to = c("vig_num", ".value"),
               names_pattern = "^att_v([1-6])_(.*)$") |>
  mutate(vig_num = as.integer(vig_num)) |>
  rename_with(~ str_c("att_", .x), all_of(ATTRIBUTE_NAMES))

# --- investment-cost follow-up (last vignette only) --------------------------
# Three routes: the ohne-FW branch; the mit-FW branch when another technology
# was recommended; and the mit-FW branch when Fernwaerme itself was recommended.
both_cost <- sum(!is.na(survey_wide$vig6_cost_fw) & !is.na(survey_wide$vig6_cost_fw_dhrec))
if (both_cost > 0)
  warning(both_cost, " respondent(s) answered both mit-FW cost questions; ",
          "vig_cost_source records which value was kept.", call. = FALSE, immediate. = TRUE)

costs <- survey_wide |>
  transmute(
    resp_uid, vig_num = N_VIGNETTES,
    cost_nofw = as.character(vig6_cost_nofw),
    cost_fw   = as.character(vig6_cost_fw),
    cost_dh   = as.character(vig6_cost_fw_dhrec),
    vig_arm   = if_else(!is.na(cost_nofw), "nofw", "fw"),
    vig_cost  = coalesce(cost_nofw, cost_dh, cost_fw),
    vig_cost_source = case_when(!is.na(cost_nofw) ~ "nofw",
                                !is.na(cost_dh)   ~ "fw_dhrec",
                                !is.na(cost_fw)   ~ "fw",
                                TRUE              ~ NA_character_),
    vig_cost_time = case_when(!is.na(cost_nofw) ~ time_vig6_cost_nofw,
                              !is.na(cost_dh)   ~ time_vig6_cost_fw_dhrec,
                              !is.na(cost_fw)   ~ time_vig6_cost_fw,
                              TRUE              ~ NA_real_),
    vig_group_cost_time = grouptime_vig6_cost
  ) |>
  filter(!is.na(vig_cost)) |>
  select(resp_uid, vig_num, vig_arm, vig_cost, vig_cost_source, vig_cost_time,
         vig_group_cost_time)

# --- assemble ----------------------------------------------------------------
vignettes_long <- answers |>
  left_join(times,           by = c("resp_uid", "vig_num", "vig_arm")) |>
  left_join(group_times,     by = c("resp_uid", "vig_num", "vig_arm")) |>
  left_join(attributes_long, by = c("resp_uid", "vig_num")) |>
  left_join(costs,           by = c("resp_uid", "vig_num", "vig_arm")) |>
  left_join(arm_info,        by = "resp_uid") |>
  # keep the branch the respondent was actually in; the other is all-NA.
  # a respondent who answered in both keeps whichever rows carry an answer.
  filter(if_else(arm_conflict, !is.na(vig_rec), vig_arm == arm_keep)) |>
  mutate(vig_arm = if_else(arm_conflict, vig_arm, arm_final)) |>
  select(-arm_observed, -arm_routed, -arm_final, -arm_keep)

# --- the vignette design id --------------------------------------------------
# vignetten_kombinationen.xlsx lists the 128 combinations; joining on all nine
# attributes recovers which one each respondent was shown.
vignette_design <- read_excel(VIGNETTE_FILE) |>
  mutate(across(everything(), as.character)) |>
  rename(!!!set_names(unname(ATTRIBUTE_SOURCE), str_c("att_", names(ATTRIBUTE_SOURCE)))) |>
  mutate(vignette_id = as.integer(vignette_id))

join_cols <- str_c("att_", ATTRIBUTE_NAMES)
vignettes_long <- vignettes_long |> left_join(vignette_design, by = join_cols)

unmatched <- sum(is.na(vignettes_long$vignette_id) & !is.na(vignettes_long$att_image))
if (unmatched > 0)
  warning(unmatched, " vignette row(s) did not match a combination in ",
          basename(VIGNETTE_FILE), ".", call. = FALSE, immediate. = TRUE)

# --- final shape -------------------------------------------------------------
rec_levels <- unique(c(levels(survey_wide$vig1_rec_fw), VIG_OTHER_LABEL))

vignettes_long <- vignettes_long |>
  left_join(survey_wide |> select(resp_uid, wave, meta_id, target_group,
                                  versandrunde, is_softlaunch),
            by = "resp_uid") |>
  mutate(vig_rec  = factor(vig_rec,  levels = rec_levels),
         vig_arm  = factor(vig_arm,  levels = c("nofw", "fw")),
         vig_cost = factor(vig_cost, levels = levels(survey_wide$vig6_cost_fw))) |>
  select(resp_uid, wave, meta_id, target_group, versandrunde, is_softlaunch,
         vig_num, vig_arm, arm_source, arm_conflict, vignette_id,
         vig_rec, vig_rec_other, vig_time, vig_group_time,
         vig_cost, vig_cost_source, vig_cost_time, vig_group_cost_time,
         all_of(join_cols)) |>
  arrange(resp_uid, vig_num, vig_arm)

message("vignettes_long: ", nrow(vignettes_long), " rows, ",
        n_distinct(vignettes_long$resp_uid), " respondents, ",
        sum(!is.na(vignettes_long$vig_rec)), " recommendations")
