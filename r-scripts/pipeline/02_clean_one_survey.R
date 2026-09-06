# =============================================================================
# 02_clean_one_survey.R
# -----------------------------------------------------------------------------
# Reshapes ONE labelled survey batch (the output of label_raw_csv()) into a
# long, per-vignette data set: one row per respondent x vignette x Fernwaerme
# branch, with the vignette answer, question/group timings, vignette-6 cost
# information, and the joined vignette combinations.
#
# This is the reshaping logic from Test.R (steps 0-9 + the vignette-combination
# join), unchanged, wrapped into a function. The input data.frame is the `data`
# argument; the function returns the long data set `data_long_full`.
#
# The helper get_single_column() and the file path VIGNETTE_FILE both come from
# config.R (sourced before this function is used).
#
# Usage:
#   long_data <- clean_one_survey(labelled)
# =============================================================================

clean_one_survey <- function(data) {

# Work on the labelled data directly. The respondent id `id` is the row key
# (it is unique per respondent), so no extra row identifier is needed.
dat <- data


# =========================================================
# 0. Identify timing columns
# =========================================================


cost_time_ohne_col <- get_single_column(
  dat,
  "^KostenVignetteohneFWTime",
  "Timing variable for KostenVignetteohneFW"
)

cost_time_mit_col <- get_single_column(
  dat,
  "^KostenVignettemitFWTime",
  "Timing variable for KostenVignettemitFW"
)

cost_time_fw_col <- get_single_column(
  dat,
  "^KostenVignetteFWTime",
  "Timing variable for KostenVignetteFW"
)

cost_group_time_col <- get_single_column(
  dat,
  "^groupTime10855",
  "Group timing variable for the cost question"
)


# =========================================================
# 1. Reshape the 54 attributes into six vignette rows
# =========================================================

attributes_long <- dat %>%
  select(
    id,
    matches("^attribute_[0-9]+$")
  ) %>%
  mutate(
    across(matches("^attribute_[0-9]+$"), as.character)
  ) %>%
  pivot_longer(
    cols = matches("^attribute_[0-9]+$"),
    names_to = "attribute_number",
    names_pattern = "^attribute_([0-9]+)$",
    values_to = "attribute_value"
  ) %>%
  mutate(
    attribute_number = as.integer(attribute_number),
    
    # Attributes 1–9 = vignette 1,
    # attributes 10–18 = vignette 2, etc.
    vignette = ((attribute_number - 1L) %/% 9L) + 1L,
    
    # Position within the vignette
    attribute_position = ((attribute_number - 1L) %% 9L) + 1L
  ) %>%
  select(
    id,
    vignette,
    attribute_position,
    attribute_value
  ) %>%
  pivot_wider(
    names_from = attribute_position,
    values_from = attribute_value,
    names_prefix = "attribute_"
  )


# =========================================================
# 2. Reshape the vignette answers
# =========================================================

vignette_responses <- dat %>%
  select(
    id,
    matches("^Vignette[1-6](ohne|mit)FW(_other)?$")
  ) %>%
  
  # Convert factors to character before combining the two
  # treatment branches, because their factor levels differ
  mutate(
    across(
      matches("^Vignette[1-6](ohne|mit)FW(_other)?$"),
      as.character
    )
  ) %>%
  
  # Add _answer to the main response variables so that
  # answer and other can be created by pivot_longer()
  rename_with(
    .fn = ~ paste0(.x, "_answer"),
    .cols = matches("^Vignette[1-6](ohne|mit)FW$")
  ) %>%
  pivot_longer(
    cols = -c(id),
    names_to = c("vignette", "FW", ".value"),
    names_pattern =
      "^Vignette([1-6])(ohne|mit)FW_(answer|other)$"
  ) %>%
  mutate(
    vignette = as.integer(vignette),
    FW = recode(
      FW,
      "ohne" = "ohne FW",
      "mit" = "mit FW"
    ),
    answer = na_if(trimws(answer), ""),
    other = na_if(trimws(other), "")
  )


# =========================================================
# 3. Reshape the vignette question times
# =========================================================

vignette_question_times <- dat %>%
  select(
    id,
    matches("^Vignette[1-6](ohne|mit)FWTime")
  ) %>%
  pivot_longer(
    cols = -c(id),
    names_to = c("vignette", "FW"),
    names_pattern =
      "^Vignette([1-6])(ohne|mit)FWTime.*$",
    values_to = "vignette_question_time"
  ) %>%
  mutate(
    vignette = as.integer(vignette),
    FW = recode(
      FW,
      "ohne" = "ohne FW",
      "mit" = "mit FW"
    )
  )


# =========================================================
# 4. Reshape the vignette group times
# =========================================================

# The group-time IDs do not directly contain the vignette
# number, so define their correspondence explicitly.
vignette_group_time_lookup <- tibble(
  group_time_prefix = c(
    paste0("groupTime108", 41:46),
    paste0("groupTime108", 47:52)
  ),
  vignette = rep(1:6, 2),
  FW = rep(
    c("ohne FW", "mit FW"),
    each = 6
  )
)

vignette_group_times <- dat %>%
  select(
    id,
    matches("^groupTime108(4[1-9]|5[0-2])")
  ) %>%
  pivot_longer(
    cols = -c(id),
    names_to = "source_variable",
    values_to = "vignette_group_time"
  ) %>%
  mutate(
    group_time_prefix = str_extract(
      source_variable,
      "^groupTime[0-9]+"
    )
  ) %>%
  left_join(
    vignette_group_time_lookup,
    by = "group_time_prefix"
  ) %>%
  select(
    id,
    vignette,
    FW,
    vignette_group_time
  )


# =========================================================
# 5. Combine answer and timing information
# =========================================================

vignette_information <- vignette_responses %>%
  full_join(
    vignette_question_times,
    by = c("id", "vignette", "FW")
  ) %>%
  full_join(
    vignette_group_times,
    by = c("id", "vignette", "FW")
  ) 

# =========================================================
# 6. Check the vignette-6 cost answer variables
# =========================================================

# KostenVignetteFW and KostenVignettemitFW are two routes
# within the mit-FW condition and should be mutually exclusive.
cost_answer_conflicts <- dat %>%
  filter(
    !is.na(KostenVignettemitFW) &
      !is.na(KostenVignetteFW)
  ) %>%
  select(
    id,
    Vignette6mitFW,
    KostenVignettemitFW,
    KostenVignetteFW
  )

if (nrow(cost_answer_conflicts) > 0L) {
  warning(
    nrow(cost_answer_conflicts),
    " respondents have values in both `KostenVignettemitFW` ",
    "and `KostenVignetteFW`. These variables should be mutually ",
    "exclusive. Inspect `cost_answer_conflicts`.",
    call. = FALSE
  )
}

# =========================================================
# 7. Create vignette-6 cost information by FW branch
# =========================================================

cost_information_v6_ohne <- dat %>%
  transmute(
    id,
    vignette = 6L,
    FW = "ohne FW",
    
    investment_cost = case_when(
      !is.na(KostenVignetteohneFW) ~
        as.character(KostenVignetteohneFW),
      TRUE ~ NA_character_
    ),
    
    investment_cost_source = case_when(
      !is.na(KostenVignetteohneFW) ~
        "ohne FW",
      TRUE ~ NA_character_
    ),
    
    investment_cost_time = case_when(
      !is.na(KostenVignetteohneFW) ~
        as.numeric(.data[[cost_time_ohne_col]]),
      TRUE ~ NA_real_
    ),
    
    # Only retain the group time when the corresponding
    # cost answer was observed
    investment_cost_group_time = case_when(
      !is.na(KostenVignetteohneFW) ~
        as.numeric(.data[[cost_group_time_col]]),
      TRUE ~ NA_real_
    )
  )


cost_information_v6_mit <- dat %>%
  transmute(
    id,
    vignette = 6L,
    FW = "mit FW",
    
    # KostenVignetteFW is used when Fernwärme was selected.
    # KostenVignettemitFW is used for the other technologies.
    investment_cost = case_when(
      !is.na(KostenVignetteFW) ~
        as.character(KostenVignetteFW),
      
      !is.na(KostenVignettemitFW) ~
        as.character(KostenVignettemitFW),
      
      TRUE ~ NA_character_
    ),
    
    investment_cost_source = case_when(
      !is.na(KostenVignetteFW) ~
        "mit FW: Fernwärme gewählt",
      
      !is.na(KostenVignettemitFW) ~
        "mit FW: andere Heiztechnologie",
      
      TRUE ~ NA_character_
    ),
    
    # Select the time from the question whose answer is observed
    investment_cost_time = case_when(
      !is.na(KostenVignetteFW) ~
        as.numeric(.data[[cost_time_fw_col]]),
      
      !is.na(KostenVignettemitFW) ~
        as.numeric(.data[[cost_time_mit_col]]),
      
      TRUE ~ NA_real_
    ),
    
    investment_cost_group_time = case_when(
      !is.na(KostenVignetteFW) |
        !is.na(KostenVignettemitFW) ~
        as.numeric(.data[[cost_group_time_col]]),
      
      TRUE ~ NA_real_
    )
  )


cost_information_v6 <- bind_rows(
  cost_information_v6_ohne,
  cost_information_v6_mit
)

# =========================================================
# 8. Combine attributes, answers, timing, and costs
# =========================================================

data_long <- attributes_long %>%
  left_join(
    vignette_information,
    by = c(
      "id",
      "vignette"
    ),
    relationship = "one-to-many"
  ) %>%
  left_join(
    cost_information_v6,
    by = c(
      "id",
      "vignette",
      "FW"
    ),
    relationship = "many-to-one"
  ) %>%
  mutate(
    answer = factor(
      answer,
      levels = c(
        "Wärmepumpe",
        "Pelletheizung",
        "Hybridheizung",
        "Gasheizung",
        "Ölheizung",
        "Fernwärme"
      )
    )
  ) %>%
  relocate(
    id,
    vignette,
    FW,
    answer,
    other,
    vignette_question_time,
    vignette_group_time,
    investment_cost,
    investment_cost_source,
    investment_cost_time,
    investment_cost_group_time
  ) %>%
  arrange(
    vignette,
    FW
  )

# =========================================================
# 9. Identify all variables now stored at vignette level
# =========================================================

vignette_specific_variables <- names(dat)[
  str_detect(
    names(dat),
    paste0(
      "^attribute_[0-9]+$|",
      "^Vignette[1-6](ohne|mit)FW(_other)?$|",
      "^Vignette[1-6](ohne|mit)FWTime|",
      "^groupTime108(4[1-9]|5[0-2])|",
      "^KostenVignette(ohneFW|mitFW|FW)$|",
      "^KostenVignette(ohneFW|mitFW|FW)Time|",
      "^groupTime10855"
    )
  )
]

respondent_variables <- dat %>%
  select(
    -all_of(vignette_specific_variables)
  )

data_long_full <- data_long %>%
  left_join(
    respondent_variables,
    by = c("id")
  ) %>%
  rename(vignette_position = vignette) 

vignette_combinations <- read_xlsx(VIGNETTE_FILE)  %>%
  mutate(
    across(everything(), as.character))

data_long_full <- data_long_full %>%
  left_join(
    vignette_combinations,
    by = c(
      "attribute_1" = "bild",
      "attribute_2" = "ehepaar_alter",
      "attribute_3" = "einkommen",
      "attribute_4" = "bestehende_heizung",
      "attribute_5" = "ersatzzeitpunkt",
      "attribute_6" = "baujahr",
      "attribute_7" = "waermebedarf_kwh_m2",
      "attribute_8" = "heizsystem",
      "attribute_9" = "sanierungsstand_waermebedarf"
    )
  ) %>% 
  rename(
    attr_1_bild = attribute_1,
    attr_2_ehepaar_alter = attribute_2,
    attr_3_einkommen = attribute_3,
    attr_4_bestehende_heizung = attribute_4,
    attr_5_ersatzzeitpunkt = attribute_5,
    attr_6_baujahr = attribute_6,
    attr_7_waermebedarf_kwh_m2 = attribute_7,
    attr_8_heizsystem = attribute_8,
    attr_9_sanierungsstand_waermebedarf = attribute_9
  ) %>%
  select(
    id, vignette_position, vignette_id, everything()
  ) %>%
  arrange(id, vignette_position)

  # Hand back the long, per-vignette data set for this batch.
  return(data_long_full)
}
