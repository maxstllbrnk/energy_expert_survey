# =============================================================================
# 03_batch_metadata.R
# -----------------------------------------------------------------------------
# Attaches the batch information to a cleaned data set.
#
# The survey was sent out in batches. Each batch has:
#   - an Umfrage-ID (survey / batch id), and
#   - a Zielgruppe (target group, e.g. "Personen" or "Firmen").
# Both live in the batch-overview Excel file (VERSAND_FILE).
#
# This script provides two functions:
#   load_versand()          - reads the overview into a tidy table
#                             (one row per batch: survey_id + target_group)
#   add_batch_metadata(...)  - adds survey_id, target_group and a unique
#                             respondent id (<batch_id>_<id>) to a cleaned batch
# =============================================================================

# -----------------------------------------------------------------------------
# load_versand(): read the batch overview.
#
# The overview sheet has 7 header/intro rows before the real table, so we skip
# them. We keep only the two columns we need, de-duplicate (the overview lists
# one row per mailing "Paket", but many share the same Umfrage-ID), and give the
# columns clear English names.
# -----------------------------------------------------------------------------
load_versand <- function(path = VERSAND_FILE) {
  read_excel(
    path = path,
    sheet = "Versandübersicht",
    skip = 7,
    na = c("", "NA"),
    .name_repair = "unique"
  ) %>%
    select(
      `Umfrage-ID`, Zielgruppe
    ) %>%
    distinct() %>%
    rename(
      survey_id    = `Umfrage-ID`,
      target_group = Zielgruppe
    )
}

# -----------------------------------------------------------------------------
# add_batch_metadata(): stamp a cleaned batch with its identity.
#
#   clean_df   - the long, per-vignette data for ONE batch (from clean_one_survey)
#   survey_id  - the batch id (taken from the raw file name)
#   versand    - the overview table from load_versand()
#
# Adds three columns and moves them to the front:
#   survey_id     - the batch id
#   target_group  - looked up from the overview for this survey_id
#   unique_id     - <survey_id>_<id>, a respondent id that is unique across all
#                   batches (the per-batch `id` on its own may repeat between
#                   batches). In the long format this value repeats across the
#                   12 vignette rows belonging to the same respondent.
# -----------------------------------------------------------------------------
add_batch_metadata <- function(clean_df, survey_id, versand) {

  # Look up the target group for this batch.
  target_group <- versand$target_group[versand$survey_id == survey_id]

  # Defensive check: exactly one target group should exist for the batch.
  if (length(target_group) != 1L) {
    stop(
      "Expected exactly one target group for survey_id ", survey_id,
      ", but found ", length(target_group), "."
    )
  }

  clean_df %>%
    mutate(
      survey_id    = survey_id,
      target_group = target_group,
      unique_id    = paste0(survey_id, "_", id)
    ) %>%
    relocate(unique_id, survey_id, target_group)
}
