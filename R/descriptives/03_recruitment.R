################################################################################
# 03_recruitment.R — who was invited, who answered, and from where
# Inputs : the sampling frame (FRAME_FILE, secure) and everyone who opened the
#          questionnaire, through load_frame(), load_respondents() and
#          apply_token_bridge() of R/ruecklauf/; the state polygons of the BKG
#          (BUNDESLAND_SHP); completers, experts and arm_sample (02_data.R)
# Outputs: figures/fig_desc_response_rate_map.pdf, fig_desc_composition.pdf;
#          tables/tab_desc_recruitment.tex, tab_desc_sample_flow.tex
# Checks : every state of the data is a polygon of the map; a state is
#          coloured only with at least MIN_KONTAKTE invited and MIN_N
#          completed; every invited and every respondent is in one row
################################################################################
# Everyone counts as a person here, as in R/ruecklauf/: an e-mail address of
# the lists once, and an invitation (token) once, however often it was used
# to start the questionnaire. The response rate is the completed
# questionnaires per invited person. The response rates count everyone who
# completed, also those without a Berufsgruppe: they were invited too.

frame <- load_frame()
respondents <- load_respondents() %>%
  apply_token_bridge(frame)
# "q2": respondents are grouped by their answer to Q2; "token": by the list
# their invitation came from (see R/ruecklauf/02_link.R)
numerator_source <- attr(respondents, "numerator_source")
completed_persons <- respondents %>%
  filter(is_complete)
cat("Invited:", nrow(frame), "| started:", nrow(respondents),
    "| completed:", nrow(completed_persons), "\n")

# 1. Response rate by state (R1) -----------------------------------------------
# Invited and completed without a usable postcode carry no state and are left
# out on both sides of the fraction, as in R/ruecklauf/03_karten.R. A state is
# coloured only with at least MIN_KONTAKTE invited and MIN_N completed.
rate_by_state <- ruecklauf_by(
  frame %>% filter(!is.na(bundesland)),
  completed_persons %>% filter(!is.na(bundesland)),
  by = "bundesland"
) %>%
  transmute(state = as.character(bundesland), invited = n_kontakte,
            completed = n_antworten, rate = ruecklaufquote,
            is_shown = invited >= MIN_KONTAKTE & completed >= MIN_N)

# GF 9: the land area of the states, one polygon each
map_states <- st_read(BUNDESLAND_SHP, quiet = TRUE) %>%
  filter(GF == 9)
unknown_states <- setdiff(rate_by_state$state, map_states$GEN)
if (length(unknown_states) > 0) {
  stop("States of the data that are no polygon of the map: ",
       paste(unknown_states, collapse = ", "))
}

map_data <- map_states %>%
  left_join(rate_by_state, by = c("GEN" = "state")) %>%
  mutate(rate_shown = if_else(!is.na(is_shown) & is_shown, rate, NA_real_),
         label = if_else(is.na(rate_shown), "–",
                         percent_label(rate_shown, digits = 1)))

# Where the labels go: a point inside each state (st_point_on_surface(); its
# coordinates by st_coordinates(), in metres of the map's projection), moved
# by MAP_LABEL_NUDGES: those of the small states outside the map, with a line
# from the state to the label, Brandenburg's within Brandenburg, without one.
# st_bbox(): the corners of the map.
unknown_nudged <- setdiff(MAP_LABEL_NUDGES$state, map_states$GEN)
if (length(unknown_nudged) > 0) {
  stop("States of MAP_LABEL_NUDGES that are no polygon of the map: ",
       paste(unknown_nudged, collapse = ", "))
}
state_points <- st_coordinates(st_point_on_surface(st_geometry(map_data)))
map_labels <- tibble(state = map_data$GEN, label = map_data$label,
                     x = state_points[, "X"], y = state_points[, "Y"]) %>%
  left_join(MAP_LABEL_NUDGES, by = "state", relationship = "one-to-one") %>%
  mutate(is_nudged = !is.na(dx_km),
         label_x = if_else(is_nudged, x + 1000 * dx_km, x),
         label_y = if_else(is_nudged, y + 1000 * dy_km, y),
         # the line stops short of the label, so that it does not run into it
         line_x = x + 0.75 * (label_x - x),
         line_y = y + 0.75 * (label_y - y))
# The map shows the moved labels too, with room for their width (metres)
map_box <- st_bbox(map_data)
map_xlim <- range(c(map_box[["xmin"]], map_box[["xmax"]], map_labels$label_x)) +
  c(-50000, 50000)
map_ylim <- range(c(map_box[["ymin"]], map_box[["ymax"]], map_labels$label_y)) +
  c(-20000, 20000)

plot_map <- ggplot() +
  geom_sf(data = map_data, aes(fill = rate_shown), colour = "white",
          linewidth = 0.3) +
  geom_segment(data = map_labels %>% filter(is_nudged, with_line),
               aes(x = x, y = y, xend = line_x, yend = line_y),
               colour = ZEW_DARK, linewidth = 0.25) +
  geom_label(data = map_labels,
             aes(x = label_x, y = label_y, label = label), size = TEXT_SIZE,
             colour = ZEW_DARK, fill = "white", alpha = 0.85, linewidth = 0,
             label.padding = unit(0.1, "lines")) +
  scale_fill_gradient(low = RATE_MAP_LOW, high = RATE_MAP_HIGH,
                      labels = scales::label_percent(accuracy = 1),
                      na.value = RATE_MAP_NOT_SHOWN, name = "Response\nrate") +
  # datum = NA: no graticule. The x and y of the labels and the limits are
  # in the map's projection, as coord_sf() expects of layers that are not sf.
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE, datum = NA) +
  labs(x = NULL, y = NULL) +
  theme_rcl(FONT_SIZE) +
  theme(legend.position = "right",
        legend.title = element_text(size = FONT_SIZE - 1),
        panel.grid = element_blank(),
        axis.text = element_blank())
save_figure_pdf(plot_map,
                file.path(figures_dir, "fig_desc_response_rate_map.pdf"),
                FIGURE_SIZES$map)

n_states_shown <- sum(rate_by_state$is_shown)
# The workbook leaves out what the map leaves out
excel_sheets$response_rate_by_state <- rate_by_state %>%
  mutate(completed = if_else(is_shown, completed, NA_integer_),
         rate = if_else(is_shown, rate, NA_real_))
numbers[["descStatesShown"]] <- latex_number(n_states_shown, 0)
map_note <- paste0(
  "Completed questionnaires per invited expert, by the federal state of ",
  "the postcode: ", latex_number(sum(rate_by_state$completed), 0),
  " completed of ", latex_number(sum(rate_by_state$invited), 0), " invited ",
  "with a postcode, all occupational groups."
)
# Grey is explained only where a state is grey
if (any(is.na(map_data$rate_shown))) {
  map_note <- paste0(map_note, " Grey and \"–\": fewer than ", MIN_KONTAKTE,
                     " invited or ", MIN_N, " completed.")
}
numbers[["descNoteResponseMap"]] <- latex_text(map_note)

# 2. Invited and responses by group (R2) ---------------------------------------
# The invited are grouped by the list they were drawn from (FRAME_TYPE_LABELS,
# with the names of the Berufsgruppen), the respondents by numerator_source.
# A respondent without a Berufsgruppe is "none".
invited_by_group <- frame %>%
  mutate(berufsgruppe = as.character(berufsgruppe)) %>%
  count(berufsgruppe, name = "invited")
answered_by_group <- respondents %>%
  mutate(berufsgruppe = if_else(as.character(berufsgruppe) %in% BERUFSGRUPPEN,
                                as.character(berufsgruppe), "none")) %>%
  group_by(berufsgruppe) %>%
  summarise(started = n(), completed = sum(is_complete), .groups = "drop")

# coalesce(): the first value that is not NA, here 0 for a group without
# respondents
recruitment <- tibble(berufsgruppe = c(names(GROUP_LABELS), "none")) %>%
  left_join(invited_by_group, by = "berufsgruppe") %>%
  left_join(answered_by_group, by = "berufsgruppe") %>%
  mutate(started = coalesce(started, 0L),
         completed = coalesce(completed, 0L))
# Every invited person and every respondent is in one row
stopifnot(sum(recruitment$invited, na.rm = TRUE) == nrow(frame),
          sum(recruitment$started) == nrow(respondents),
          sum(recruitment$completed) == nrow(completed_persons))

# Shares among the three groups, on each side
is_group <- recruitment$berufsgruppe %in% names(GROUP_LABELS)
recruitment <- recruitment %>%
  mutate(
    group = if_else(is_group, unname(GROUP_LABELS[berufsgruppe]),
                    "No occupational group"),
    rate = completed / invited,
    share_invited = if_else(is_group,
                            invited / sum(invited[is_group]), NA_real_),
    share_completed = if_else(is_group,
                              completed / sum(completed[is_group]), NA_real_)
  )

share_tonline <- mean(frame$is_tonline)
rate_all <- nrow(completed_persons) / nrow(frame)
rate_without_tonline <- nrow(completed_persons) / sum(!frame$is_tonline)

recruitment_cells <- recruitment %>%
  transmute(panel = NA_character_,
            label = latex_text(group),
            invited = statistic_text(invited, 0),
            started = latex_number(started, 0),
            completed = latex_number(completed, 0),
            rate = statistic_text(100 * rate, 1),
            share_invited = statistic_text(100 * share_invited, 1),
            share_completed = statistic_text(100 * share_completed, 1))
# Respondents without a Berufsgruppe were invited too, but from a list that is
# not known: no invited, no rate and no shares in their row
recruitment_cells$invited[!is_group] <- ""
recruitment_cells$rate[!is_group] <- ""
recruitment_cells$share_invited[!is_group] <- ""
recruitment_cells$share_completed[!is_group] <- ""
# The shares are among the three groups, so the total, which counts the
# respondents without a group too, has none
total_cells <- tibble(panel = NA_character_, label = "Total",
                      invited = latex_number(nrow(frame), 0),
                      started = latex_number(nrow(respondents), 0),
                      completed = latex_number(nrow(completed_persons), 0),
                      rate = latex_number(100 * rate_all, 1),
                      share_invited = "", share_completed = "")

if (numerator_source == "token") {
  grouping_note <- paste0(
    "Respondents are grouped by the list of their invitation."
  )
} else {
  grouping_note <- paste0(
    "Respondents are grouped by their answer to Q2, the invited by the list ",
    "they were drawn from, so the response rate of a group is approximate."
  )
}
recruitment_tex <- table_float(
  column_spec = "lcccccc",
  header = c(
    paste0(" & & & & & \\multicolumn{2}{c}{Share of the three groups (\\%)}",
           " \\\\"),
    "\\cmidrule(lr){6-7}",
    paste0(" & Invited & Started & Completed & ",
           header_cell("Response\\\\rate (\\%)"), " & Invited & Completed \\\\")
  ),
  body = c(cell_rows(recruitment_cells %>% select(-panel)), "\\midrule",
           cell_rows(total_cells %>% select(-panel))),
  caption = "Invited experts and responses",
  label = "tab:desc_recruitment",
  notes = latex_text(paste0(
    "Invited: persons on the lists the invitations went to, by list: the ",
    "dena list of energy advisers, the register of HVAC firms and the ",
    "register of chimney sweeps; an e-mail address counts once. Started and ",
    "completed: persons who opened and who submitted the questionnaire; ",
    "repeated starts of one invitation count once. Response rate: completed ",
    "per invited. ", grouping_note, " ",
    percent_label(share_tonline, 1), " of the invited had a t-online ",
    "address, which Telekom may have blocked; without them in the ",
    "denominator the response rate would be at most ",
    percent_label(rate_without_tonline, 1), "."
  )),
  font_size = TABLE_FONT_SIZE
)
write_tex(recruitment_tex, file.path(tables_dir, "tab_desc_recruitment.tex"))

excel_sheets$recruitment <- recruitment %>%
  select(group, invited, started, completed, rate, share_invited,
         share_completed)
numbers[["descInvited"]] <- latex_number(nrow(frame), 0)
numbers[["descStarted"]] <- latex_number(nrow(respondents), 0)
numbers[["descCompleted"]] <- latex_number(nrow(completed_persons), 0)
numbers[["descResponseRate"]] <- paste0(latex_number(100 * rate_all, 1), "\\%")
numbers[["descResponseRateNoTonline"]] <-
  paste0(latex_number(100 * rate_without_tonline, 1), "\\%")
numbers[["descShareTonline"]] <-
  paste0(latex_number(100 * share_tonline, 1), "\\%")

# 3. Who was invited, who completed (R3) ---------------------------------------
composition <- bind_rows(
  recruitment %>%
    filter(is_group) %>%
    transmute(group, series = "Invited (persons)", share = share_invited),
  recruitment %>%
    filter(is_group) %>%
    transmute(group, series = "Completed (persons)", share = share_completed)
) %>%
  # coord_flip() puts the first level at the bottom, and position_dodge()
  # places the series in the order of `dodge`: both reversed, so that the
  # first group and the first series are at the top
  mutate(group = factor(group, levels = rev(GROUP_LABELS)),
         series = factor(series, levels = names(COMPOSITION_COLOURS)),
         dodge = factor(series, levels = rev(names(COMPOSITION_COLOURS))))
# A series without a colour would become NA
stopifnot(!anyNA(composition$series))

plot_composition <- ggplot(composition,
                           aes(x = group, y = share, fill = series,
                               group = dodge)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = percent_label(share)),
            position = position_dodge(width = 0.75), hjust = -0.15,
            size = TEXT_SIZE, colour = ZEW_DARK) +
  coord_flip() +
  scale_fill_manual(values = COMPOSITION_COLOURS,
                    breaks = names(COMPOSITION_COLOURS)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Share of the three groups") +
  theme_rcl(FONT_SIZE) +
  theme(panel.grid.major.y = element_blank(),
        legend.key.size = unit(LEGEND_KEY_SIZE, "pt"))
save_figure_pdf(plot_composition,
                file.path(figures_dir, "fig_desc_composition.pdf"),
                FIGURE_SIZES$full)

numbers[["descNoteComposition"]] <- latex_text(paste0(
  "Invited: ", latex_number(sum(recruitment$invited[is_group]), 0),
  " persons, by the list they were drawn from. Completed: ",
  latex_number(sum(recruitment$completed[is_group]), 0), " persons with an ",
  "occupational group. ", grouping_note
))

# 4. From the invitation to the estimation sample (R4) -------------------------
# A repeat completion: a completed questionnaire of an invitation (wave x
# token) that had completed before. It counts once among the persons, but
# once per questionnaire among the completed questionnaires, and so in the
# models. Questionnaires without a token are not counted.
completions_per_token <- completers %>%
  filter(!is.na(meta_token)) %>%
  count(wave, meta_token)
n_repeat <- sum(completions_per_token$n - 1)
cat("Completed questionnaires without a token:",
    sum(is.na(completers$meta_token)), "| repeat completions:", n_repeat, "\n")

arm_totals <- arm_sample %>%
  group_by(arm) %>%
  summarise(experts = sum(experts), vignettes = sum(vignettes),
            .groups = "drop")
experts_fw <- arm_totals$experts[arm_totals$arm == "fw"]
experts_nofw <- arm_totals$experts[arm_totals$arm == "nofw"]
vignettes_fw <- arm_totals$vignettes[arm_totals$arm == "fw"]
vignettes_nofw <- arm_totals$vignettes[arm_totals$arm == "nofw"]

sample_flow <- tribble(
  ~step,                                                           ~experts,                        ~vignettes,
  "Invited (persons)",                                             nrow(frame),                     NA,
  "Started the questionnaire (persons)",                           nrow(respondents),               NA,
  "Completed the questionnaire (persons)",                         nrow(completed_persons),                 NA,
  "Completed questionnaires",                                      nrow(completers),                NA,
  "of which repeat completions of one invitation",                 n_repeat,                        NA,
  "of which without an occupational group",                        nrow(completers) - nrow(experts), NA,
  "Experts of the descriptives",                                   nrow(experts),                   NA,
  "with the district-heating option, with an answered vignette",   experts_fw,                      vignettes_fw,
  "without the district-heating option, with an answered vignette", experts_nofw,                   vignettes_nofw
)
is_indented <- sample_flow$step %in% c(
  "of which repeat completions of one invitation",
  "of which without an occupational group",
  "with the district-heating option, with an answered vignette",
  "without the district-heating option, with an answered vignette"
)
sample_flow_cells <- sample_flow %>%
  transmute(label = latex_text(step),
            experts = latex_number(experts, 0),
            vignettes = latex_number(vignettes, 0))
sample_flow_cells$label[is_indented] <-
  paste0("\\quad ", sample_flow_cells$label[is_indented])

sample_flow_tex <- table_float(
  column_spec = "lcc",
  header = paste0(" & Experts & ", header_cell("Answered\\\\vignettes"),
                  " \\\\"),
  body = cell_rows(sample_flow_cells),
  caption = "From the invitation to the estimation sample",
  label = "tab:desc_sample_flow",
  notes = latex_text(paste0(
    "Persons as in the table of the invited and the responses. Completed ",
    "questionnaires: as in the data; a repeat completion counts again here ",
    "and in the models. Occupational group from Q2. The mixed logit models ",
    "of an arm use its experts and their answered vignettes."
  )),
  font_size = TABLE_FONT_SIZE
)
write_tex(sample_flow_tex, file.path(tables_dir, "tab_desc_sample_flow.tex"))

excel_sheets$sample_flow <- sample_flow
excel_sheets$estimation_sample <- arm_sample
numbers[["descCompletedQuestionnaires"]] <- latex_number(nrow(completers), 0)
numbers[["descExperts"]] <- latex_number(nrow(experts), 0)
numbers[["descExpertsFw"]] <- latex_number(experts_fw, 0)
numbers[["descExpertsNofw"]] <- latex_number(experts_nofw, 0)
numbers[["descVignettesFw"]] <- latex_number(vignettes_fw, 0)
numbers[["descVignettesNofw"]] <- latex_number(vignettes_nofw, 0)
