################################################################################
# test_deck.R — the sections on recruitment, completion and selection, the frame
#               and the deck, on synthetic data
# Inputs : experts.rds, vignettes.rds, frame.rds of a synthetic run
# Outputs: in <SECURE_DIR>/output/test deck/ (removed first), a path with a space
# Checks : every exhibit file exists and is in the list; the numbers of the tables
#          add up and match a computation by hand; the minimum cell; no German;
#          the deck compiles without overfull boxes, also alone, and fails softly;
#          a sub-sample writes only what applies
################################################################################
# From the project root, after tests/simulate_data.R and R/run_cleaning.R:
#   Rscript tests/test_deck.R
# Five to ten minutes for 1,500 experts, mostly the maps and pdflatex. Do not edit
# this file while it runs: R reads it line by line.

source(file.path("R", "setup.R"))
if (!USE_SYNTHETIC_DATA) {
  stop("tests/test_deck.R writes test decks: synthetic data only")
}
test_dir <- file.path(OUTPUT_DIR, "test deck")
unlink(test_dir, recursive = TRUE)
dir.create(test_dir, recursive = TRUE)
project_dir <- getwd()

experts <- readRDS(file.path(CLEAN_DATA_DIR, "experts.rds"))
vignettes <- readRDS(file.path(CLEAN_DATA_DIR, "vignettes.rds"))

#' The cells of the row of a .tex table that starts with this label, without the
#' indent of a panel
table_row <- function(file, label) {
  table_lines <- readLines(file, encoding = "UTF-8")
  pattern <- paste0("^(\\\\hspace\\{1em\\})?", label, " &")
  row <- table_lines[str_detect(table_lines, pattern)]
  stopifnot(length(row) == 1)
  # str_split(): the text between the "&"; str_trim(): without the blanks around it
  cells <- str_trim(str_split(str_remove(row, "\\\\\\\\.*$"), "&")[[1]])
  cells[-1]
}

#' The messages of an expression: it runs on, and the messages go into a vector.
#' withCallingHandlers() catches each message, invokeRestart() lets the expression
#' go on, and <<- adds to the vector of this function from inside the handler.
messages_of <- function(expression) {
  messages <- character(0)
  withCallingHandlers(
    expression,
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  messages
}

# 1. Small functions ----------------------------------------------------------
stopifnot(
  is.na(response_rate(9, 100)),
  is.na(response_rate(10, 29)),
  all.equal(response_rate(10, 30), 1 / 3),
  is.na(shown_count(9)),
  shown_count(10) == 10,
  is.na(shown_count(10, 29)),
  shown_count(10, 30) == 10
)

codes <- question_code(QUESTIONNAIRE$label)
stopifnot(!anyNA(codes), !anyDuplicated(codes))

answer_columns <- question_columns(experts)
vignette_items <- c(paste0("vignette_", 1:6), "vignette_cost")
stopifnot(setequal(names(answer_columns),
                   setdiff(QUESTIONNAIRE$item, vignette_items)),
          all(lengths(answer_columns) > 0))
cat("Answer columns not in the data (optional):",
    setdiff(unlist(answer_columns), names(experts)), "\n")

halves <- two_halves(tibble(label = letters[1:9], n = as.character(1:9)),
                     c(rep("A", 2), rep("B", 3), rep("C", 4)))
# Three headings and nine rows: split after B, the section end closest to the middle
stopifnot(nrow(halves) == 7,
          ncol(halves) == 5,
          identical(halves$left_label[1], "\\textbf{A}"),
          identical(halves$right_label[1], "\\textbf{C}"),
          identical(halves$right_label[6], ""))

# 2. The frame ----------------------------------------------------------------
stopifnot(all(FRAME_TYPE_LABELS %in% EXPERT_GROUPS))
# One address twice (with another case, a blank, and on another list), a name in
# latin1 as in the real file, a fragment of a postcode, a postcode the lookup does
# not have, whose first three digits are unanimous, and a row without an address
plz_lookup <- read_csv(PLZ_LOOKUP_FILE, show_col_types = FALSE,
                       col_types = cols(plz = col_character(),
                                        bl_code = col_character()))
unanimous <- plz_lookup %>%
  count(plz3 = str_sub(plz, 1, 3), bundesland) %>%
  group_by(plz3) %>%
  filter(n() == 1) %>%
  ungroup()
prefix_state <- unanimous$bundesland[1]
prefix_candidates <- paste0(unanimous$plz3[1], sprintf("%02d", 0:99))
prefix_plz <- setdiff(prefix_candidates, plz_lookup$plz)[1]
tiny_frame <- data.frame(
  id = 1:6,
  Email = c("Anna@example.org", "anna@example.org ", "j\xf6rg@t-online.de",
            "b@example.org", "c@example.org", NA),
  type = c("DENA", "SHK", "SHK", "Schornsteinfeger", "DENA", "DENA"),
  PLZ = c("10115", "10115", "123", NA, prefix_plz, "10115")
)
tiny_file <- file.path(test_dir, "tiny_frame.rds")
saveRDS(tiny_frame, tiny_file)
tiny <- load_frame(tiny_file)
# The row without an address is not invited
stopifnot(
  nrow(tiny) == 4,
  attr(tiny, "n_raw") == 6,
  attr(tiny, "n_no_address") == 1,
  attr(tiny, "n_duplicate_contacts") == 1,
  attr(tiny, "n_type_conflicts") == 1,
  identical(as.character(tiny$expert_group),
            c("Energy advisers", "HVAC installers", "Chimney sweeps",
              "Energy advisers")),
  identical(tiny$is_tonline, c(FALSE, TRUE, FALSE, FALSE)),
  identical(is.na(tiny$bundesland), c(FALSE, TRUE, TRUE, FALSE)),
  as.character(tiny$bundesland[4]) == prefix_state,
  !("email" %in% names(tiny)),
  setequal(names(STATE_LABELS), levels(tiny$bundesland))
)
tiny_frame$type[1] <- "Unbekannt"
saveRDS(tiny_frame, tiny_file)
# tryCatch(): the message of the error instead of a stop
unknown_list <- tryCatch(load_frame(tiny_file), error = conditionMessage)
stopifnot(is.character(unknown_list), str_detect(unknown_list, "Unbekannt"))

frame <- load_frame()
stopifnot(nrow(frame) == n_distinct(readRDS(FRAME_FILE)$Email))

# 3. The full sample, with the waves of the dispatch log ----------------------
dispatch <- load_versand()
stopifnot(nrow(dispatch) == 12, sum(dispatch$invitations) == 45965)

full_dir <- file.path(test_dir, "full sample", "summary statistics")
exhibits <- run_summary_statistics(experts, vignettes, full_dir, "Full sample",
                                   frame = frame)
tables_dir <- file.path(full_dir, "tables")

# cross_join(): every id with every group
expected <- bind_rows(
  tibble(id = c("sample_flow", "response_state", "response_state_started",
                "response_state_table")) %>%
    cross_join(tibble(group = c(ALL_EXPERTS, EXPERT_GROUPS))),
  tibble(id = c("response_group", "composition", "response_wave"),
         group = ALL_EXPERTS),
  tibble(id = c("survival", "breakoff_section", "breakoff_question",
                "breakoff_state", "duration", "vignette_times", "item_response",
                "meta", "selection", "selection_prices", "selection_state",
                "selection_later", "selection_barriers",
                "selection_vignettes")) %>%
    cross_join(tibble(group = c(ALL_EXPERTS, EXPERT_GROUPS)))
)
written <- exhibits %>%
  filter(block %in% c("A. Recruitment", "B. Survey metadata", "C. Selection"))
stopifnot(nrow(anti_join(expected, written, by = c("id", "group"))) == 0,
          nrow(anti_join(written, expected, by = c("id", "group"))) == 0,
          all(file.exists(file.path(full_dir, exhibits$file))),
          !anyDuplicated(exhibits$file))
main_exhibits <- written %>%
  filter(placement == "main") %>%
  transmute(id, group)
stopifnot(all(main_exhibits$group == ALL_EXPERTS),
          setequal(main_exhibits$id,
                   c("sample_flow", "response_state", "response_group",
                     "composition", "survival", "breakoff_section", "selection",
                     "selection_prices", "selection_vignettes")))
# Every file on the disk is in the list
files_on_disk <- c(file.path("tables", list.files(tables_dir)),
                   file.path("figures", list.files(file.path(full_dir, "figures"))),
                   file.path("maps", list.files(file.path(full_dir, "maps"))))
stopifnot(setequal(files_on_disk, exhibits$file))
# A file name follows the convention, with the group of its row
prefixes <- c(table = "tab_", figure = "fig_", map = "map_")
extensions <- c(table = ".tex", figure = ".pdf", map = ".pdf")
stopifnot(identical(basename(written$file),
                    paste0(prefixes[written$type], written$id, "_",
                           group_slug(written$group), extensions[written$type])))
# By group a note says what the rate rests on, in the table or below the figure
is_by_group <- written$block == "A. Recruitment" & written$group != ALL_EXPERTS
for (k in which(is_by_group)) {
  if (written$type[k] == "table") {
    table_lines <- readLines(file.path(full_dir, written$file[k]), encoding = "UTF-8")
    stopifnot(any(str_detect(table_lines,
                             "the mailing list and the numerator the answer")))
  } else {
    stopifnot(str_detect(written$note[k], fixed(GROUP_RATE_NOTE)))
  }
}
cat("Exhibits of blocks A to C:", nrow(written), "\n")

# 3.1 The numbers against a computation by hand
answered <- vignettes %>%
  filter(!is.na(vig_rec))
n_completed <- sum(experts$is_complete)
flow_file <- file.path(tables_dir, "tab_sample_flow_all.tex")
stopifnot(
  identical(table_row(flow_file, "Invited"), c(fmt_int(nrow(frame)), "")),
  identical(table_row(flow_file, "Started the questionnaire"),
            c(fmt_int(nrow(experts)), fmt_int(nrow(answered)))),
  identical(table_row(flow_file, "Completed the questionnaire"),
            c(fmt_int(n_completed), fmt_int(sum(answered$is_complete)))),
  identical(table_row(flow_file, "Completed, with a group"),
            c(fmt_int(sum(experts$is_complete & !is.na(experts$expert_group))),
              fmt_int(sum(answered$is_complete & !is.na(answered$expert_group)))))
)
in_fw <- answered %>%
  filter(is_complete, !is.na(expert_group), vig_arm == "fw")
stopifnot(identical(table_row(flow_file, "\\\\quad With the district-heating option"),
                    c(fmt_int(n_distinct(in_fw$resp_uid)), fmt_int(nrow(in_fw)))))

group_file <- file.path(tables_dir, "tab_response_group_all.tex")
n_invited_hvac <- sum(frame$expert_group == "HVAC installers")
is_hvac <- experts$expert_group %in% "HVAC installers"
n_no_tonline <- sum(frame$expert_group == "HVAC installers" & !frame$is_tonline)
stopifnot(identical(
  table_row(group_file, "HVAC installers"),
  c(fmt_int(n_invited_hvac), fmt_int(sum(is_hvac)),
    fmt_int(sum(is_hvac & experts$is_complete)),
    fmt_pct(sum(is_hvac) / n_invited_hvac),
    fmt_pct(sum(is_hvac & experts$is_complete) / n_invited_hvac),
    fmt_int(n_no_tonline),
    fmt_pct(sum(is_hvac) / n_no_tonline),
    fmt_pct(sum(is_hvac & experts$is_complete) / n_no_tonline))
))

# The minimum cell of a state, in the group with the smallest cells, the chimney
# sweeps: a count of answers goes with its rate, and the denominator without
# t-online addresses with its own minimum
sweeps <- experts %>%
  filter(expert_group == "Chimney sweeps")
sweep_file <- file.path(tables_dir, "tab_response_state_table_chimney_sweeps.tex")
sweep_counts <- count_by_state(frame %>% filter(expert_group == "Chimney sweeps"),
                               sweeps)
n_hidden <- 0
for (k in seq_len(nrow(sweep_counts))) {
  cells <- table_row(sweep_file, STATE_LABELS[[sweep_counts$state[k]]])
  is_shown <- sweep_counts$invited[k] >= MIN_CONTACTS &
    sweep_counts$completed[k] >= MIN_N
  stopifnot(identical(cells[5] != "--", is_shown),
            identical(cells[3] != "--", is_shown),
            identical(cells[6] != "--",
                      sweep_counts$invited_no_tonline[k] >= MIN_CONTACTS))
  n_hidden <- n_hidden + as.integer(!is_shown)
}
cat("Chimney sweeps: states below the minimum cell, hidden:", n_hidden, "\n")

# Item response, in its two halves: no count below the minimum cell, and where the
# share is hidden, the counts it rests on are too
for (slug in GROUP_SLUGS) {
  item_lines <- readLines(file.path(tables_dir,
                                    paste0("tab_item_response_", slug, ".tex")),
                          encoding = "UTF-8")
  for (item_line in item_lines) {
    # strsplit(): the cells of the line; [[1]]: of this one line
    cells <- str_trim(str_remove(strsplit(item_line, " & ", fixed = TRUE)[[1]],
                                 "\\\\\\\\$"))
    if (length(cells) != 11 || cells[2] == "Reached") next
    for (first_count in c(2, 8)) {
      counts <- cells[first_count:(first_count + 2)]
      shown <- counts[counts != "--" & counts != ""]
      stopifnot(all(as.numeric(gsub(",", "", shown)) >= MIN_N))
      if (cells[first_count + 3] == "--") {
        stopifnot(all(counts[2:3] == "--"))
      }
    }
  }
}

# Nobody below the minimum cell among those who stopped at a question, in any version
for (slug in GROUP_SLUGS) {
  stop_lines <- readLines(file.path(tables_dir,
                                    paste0("tab_breakoff_question_", slug, ".tex")),
                          encoding = "UTF-8")
  for (stop_line in stop_lines) {
    cells <- str_trim(str_remove(strsplit(stop_line, " & ", fixed = TRUE)[[1]],
                                 "\\\\\\\\$"))
    if (length(cells) != 7 || cells[2] == "N") next
    counts <- cells[c(2, 6)]
    shown <- counts[counts != "--" & counts != ""]
    numbers <- as.numeric(gsub(",", "", shown))
    stopifnot(all(numbers == 0 | numbers >= MIN_N))
  }
}

# A second run with nothing changed writes nothing
deck_time <- file.mtime(file.path(full_dir, "summary_statistics.pdf"))
exhibits_again <- run_summary_statistics(experts, vignettes, full_dir, "Full sample",
                                         frame = frame)
stopifnot(file.mtime(file.path(full_dir, "summary_statistics.pdf")) == deck_time,
          nrow(exhibits_again) == nrow(exhibits))

# The waves: from the dispatch log, in the table
wave_file <- file.path(tables_dir, "tab_response_wave_all.tex")
wave_row <- table_row(wave_file, dispatch$wave[1])
stopifnot(wave_row[3] == fmt_int(dispatch$invitations[1]),
          wave_row[2] == format(dispatch$first_sent[1], "%Y-%m-%d"),
          table_row(wave_file, "Invitations in the dispatch log")[3] == "45,965",
          table_row(wave_file, "Rows of the sampling frame")[3] ==
            fmt_int(attr(frame, "n_raw")),
          # the row that makes the panel add up to the persons on the lists
          table_row(wave_file,
                    "Rows that repeat the address of another row \\(counted once\\)")[3] ==
            fmt_int(attr(frame, "n_duplicate_contacts")),
          table_row(wave_file,
                    "Persons on the lists \\(one row per e-mail address\\)")[3] ==
            fmt_int(nrow(frame)),
          table_row(wave_file, "All waves")[3] == "45,965")

# The comparison against a computation by hand: a number and a share
selection_file <- file.path(tables_dir, "tab_selection_all.tex")
prices_file <- file.path(tables_dir, "tab_selection_prices_all.tex")
hdd_completers <- experts$hdd[experts$is_complete & !is.na(experts$hdd)]
hdd_breakoffs <- experts$hdd[!experts$is_complete & !is.na(experts$hdd)]
hdd_difference <- (mean(hdd_completers) - mean(hdd_breakoffs)) /
  sqrt((var(hdd_completers) + var(hdd_breakoffs)) / 2)
hdd_row <- table_row(selection_file, "Heating degree days of the region")
stopifnot(hdd_row[1] == fmt_int(length(hdd_completers)),
          hdd_row[2] == fmt_num(mean(hdd_completers), 0),
          hdd_row[12] == fmt_num(quantile(hdd_breakoffs, 0.75, names = FALSE), 0),
          hdd_row[13] == fmt_num(hdd_difference, 2),
          hdd_row[14] == fmt_num(t.test(hdd_completers, hdd_breakoffs)$p.value, 3))

price_position <- QUESTIONNAIRE$position[QUESTIONNAIRE$item == "time_pexp"]
rise_reached <- experts %>%
  filter(!is.na(pexp_gas_rise),
         is_complete | last_question_position >= price_position)
rise_completers <- rise_reached$pexp_gas_rise[rise_reached$is_complete]
rise_breakoffs <- rise_reached$pexp_gas_rise[!rise_reached$is_complete]
rise_table <- matrix(c(sum(rise_completers), sum(1 - rise_completers),
                       sum(rise_breakoffs), sum(1 - rise_breakoffs)), nrow = 2)
rise_row <- table_row(prices_file, "Expects a rise: Gas")
stopifnot(rise_row[1] == fmt_int(length(rise_completers)),
          rise_row[2] == fmt_pct(mean(rise_completers)),
          rise_row[3] == "",
          rise_row[8] == fmt_pct(mean(rise_breakoffs)),
          rise_row[14] == fmt_num(chisq.test(rise_table, correct = FALSE)$p.value, 3))
# The shares of a question sum to 100 percent, among those who answered
revenue_shares <- numeric(length(REVENUE_LABELS))
for (k in seq_along(REVENUE_LABELS)) {
  revenue_label <- gsub("€", "\\\\\\\\texteuro\\\\{\\\\}", REVENUE_LABELS[[k]])
  revenue_row <- table_row(selection_file, paste0("Revenue: ", revenue_label))
  revenue_shares[k] <- as.numeric(str_remove(revenue_row[2], "\\\\%"))
}
stopifnot(abs(sum(revenue_shares) - 100) < 0.5)

# 3.2 The deck
full_log <- file.path(full_dir, "summary_statistics.log")
log_lines <- readLines(full_log, warn = FALSE)
stopifnot(file.exists(file.path(full_dir, "summary_statistics.pdf")),
          file.exists(file.path(full_dir, "summary_statistics.tex")),
          file.exists(file.path(full_dir, "exhibits.csv")),
          sum(str_detect(log_lines, "Overfull")) == 0,
          sum(str_detect(log_lines, "^!")) == 0,
          identical(getwd(), project_dir))
deck_file <- file.path(full_dir, "summary_statistics.tex")
deck_lines <- readLines(deck_file, encoding = "UTF-8")
stopifnot(sum(str_detect(deck_lines, "^\\\\begin\\{frame\\}")) == nrow(exhibits) + 2,
          any(str_detect(deck_lines, "Commit [0-9a-f]+")),
          any(str_detect(deck_lines, "max totalheight=0.82")),
          any(str_detect(deck_lines, "All experts \\(main\\)")))

# 3.3 No German in the tables and figure notes, and no raw name
# Ja and Nein as whole words, as January is English
german_words <- paste0(
  "\\b(Ja|Nein)\\b|",
  "\\b(Nicht Gew|Keine|Sonstig|Firmen|Personen|Fernw|Wärme|Heizung|Bayern|",
  "Hessen|Niedersachsen|Sachsen|Thüringen|Nordrhein|Rheinland|bundesweit|Abschluss)"
)
for (file in c(list.files(tables_dir, full.names = TRUE))) {
  german <- str_subset(readLines(file, encoding = "UTF-8"), german_words)
  if (length(german) > 0) {
    stop("German in ", file, ": ", german[1])
  }
}
stopifnot(!any(str_detect(exhibits$note, german_words)),
          !any(str_detect(exhibits$caption, german_words)))

# 4. The deck alone, on an existing folder ------------------------------------
unlink(file.path(full_dir, "summary_statistics.pdf"))
alone <- build_presentation(NULL, full_dir, "Full sample")
stopifnot(identical(alone, file.path(full_dir, "summary_statistics.pdf")),
          file.exists(alone),
          identical(getwd(), project_dir))
deck_lines <- readLines(deck_file, encoding = "UTF-8")
stopifnot(any(str_detect(deck_lines, "Number of experts and vignettes not given")))

# Exhibits as the other sections write them: nothing overflows, however long
fake_dir <- file.path(test_dir, "fake deck")
dir.create(file.path(fake_dir, "figures"), recursive = TRUE)
dir.create(file.path(fake_dir, "maps"), recursive = TRUE)
dir.create(file.path(fake_dir, "tables"), recursive = TRUE)
plot_fake <- ggplot(tibble(x = 1:3, y = 1:3), aes(x = x, y = y)) +
  geom_line()
long_note <- paste(
  rep("A note with 80 percent of a line, & more, as long as a note gets.", 7),
  collapse = " "
)
long_caption <- paste("A caption of a certain length, with & and % and a group",
                      "after it so that it takes two lines of the frame title")
save_figure(plot_fake, file.path(fake_dir, "figures", "fig_fake_full.pdf"),
            size = "full")
save_figure(plot_fake, file.path(fake_dir, "figures", "fig_fake_tall.pdf"),
            size = "tall")
save_figure(plot_fake, file.path(fake_dir, "maps", "map_fake.pdf"), size = "map")
long_table <- tibble(label = paste("Row", 1:150), a = "1.0", b = "2.0")
table_custom(long_table, file.path(fake_dir, "tables", "tab_fake_long.tex"), "A note.")
wide_table <- as_tibble(matrix("1,000", 3, 40,
                               dimnames = list(NULL, paste0("column", 1:40))))
table_custom(wide_table, file.path(fake_dir, "tables", "tab_fake_wide.tex"), "A note.")
fake_block <- "D. Experts and firms"
fake_exhibits <- bind_rows(
  exhibit_row(fake_block, "full", "HVAC installers", "figure",
              "figures/fig_fake_full.pdf", long_caption, "main", long_note),
  exhibit_row(fake_block, "tall", ALL_EXPERTS, "figure",
              "figures/fig_fake_tall.pdf", long_caption, "appendix", long_note),
  exhibit_row(fake_block, "map", ALL_EXPERTS, "map", "maps/map_fake.pdf",
              long_caption, "appendix", long_note),
  exhibit_row(fake_block, "map without a note", ALL_EXPERTS, "map",
              "maps/map_fake.pdf", "A short caption", "appendix"),
  exhibit_row("J. Vignettes", "long", ALL_EXPERTS, "table",
              "tables/tab_fake_long.tex", long_caption, "appendix"),
  exhibit_row("J. Vignettes", "wide", ALL_EXPERTS, "table",
              "tables/tab_fake_wide.tex", long_caption, "appendix")
)
fake_pdf <- build_presentation(fake_exhibits, fake_dir, "Fake")
fake_log <- readLines(file.path(fake_dir, "summary_statistics.log"), warn = FALSE)
stopifnot(file.exists(fake_pdf),
          sum(str_detect(fake_log, "Overfull")) == 0)
fake_deck <- readLines(file.path(fake_dir, "summary_statistics.tex"),
                       encoding = "UTF-8")
# All experts first in a block, then the groups; blocks without exhibits are named
first_frames <- str_subset(fake_deck, "^\\\\begin\\{frame\\}.*(All experts|HVAC)")
stopifnot(str_detect(first_frames[1], "All experts"),
          str_detect(first_frames[4], "HVAC"),
          any(str_detect(fake_deck, "A. Recruitment")),
          !any(str_detect(fake_deck, "D. Experts and firms\\}: ")))

# 5. The deck fails softly ----------------------------------------------------
# A file of the list is gone: it is named under "Not shown", and the rest compiles
meta_file <- file.path(full_dir, "tables", "tab_meta_all.tex")
meta_lines <- readLines(meta_file, encoding = "UTF-8")
unlink(meta_file)
gone <- build_presentation(NULL, full_dir, "Full sample")
deck_lines <- readLines(deck_file, encoding = "UTF-8")
stopifnot(file.exists(gone),
          any(str_detect(deck_lines,
                         "the file does not exist: tables/tab\\\\_meta\\\\_all.tex")))

# A table that does not compile: a message with the log, the .tex stays, no stop
writeLines(c("\\undefinedcommand", meta_lines), meta_file, useBytes = TRUE)
broken_messages <- messages_of(
  broken <- build_presentation(NULL, full_dir, "Full sample")
)
stopifnot(is.na(broken),
          any(str_detect(broken_messages, fixed(full_log))),
          file.exists(deck_file),
          identical(getwd(), project_dir))
writeLines(meta_lines, meta_file, useBytes = TRUE)

# No pdflatex: a message, the .tex stays, no stop, no old deck left behind
old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = "")
missing_messages <- messages_of(
  missing_deck <- build_presentation(NULL, full_dir, "Full sample")
)
Sys.setenv(PATH = old_path)
stopifnot(is.na(missing_deck),
          any(str_detect(missing_messages, "pdflatex is missing")),
          file.exists(deck_file),
          !file.exists(file.path(full_dir, "summary_statistics.pdf")),
          identical(getwd(), project_dir))

# 6. A sub-sample: completers of one arm, no frame ----------------------------
completers_fw <- experts %>%
  filter(is_complete, arm == "fw")
vignettes_fw <- vignettes %>%
  filter(resp_uid %in% completers_fw$resp_uid)
sub_dir <- file.path(test_dir, "completers fw")
sub_exhibits <- run_summary_statistics(completers_fw, vignettes_fw, sub_dir,
                                       "Completers, with district heating & more")
metadata_ids <- sub_exhibits$id[sub_exhibits$block == "B. Survey metadata"]
stopifnot(setequal(unique(metadata_ids), c("duration", "vignette_times")),
          !any(sub_exhibits$block %in% c("0. Data cleaning", "A. Recruitment",
                                         "C. Selection")),
          all(file.exists(file.path(sub_dir, sub_exhibits$file))),
          file.exists(file.path(sub_dir, "summary_statistics.pdf")))
sub_deck <- readLines(file.path(sub_dir, "summary_statistics.tex"),
                      encoding = "UTF-8")
stopifnot(any(str_detect(sub_deck, "A. Recruitment.*sampling frame")),
          any(str_detect(sub_deck, "C. Selection")),
          any(str_detect(sub_deck, fixed("district heating \\& more"))))

# 7. Other samples ------------------------------------------------------------
section_dirs <- list(tables = file.path(test_dir, "sections", "tables"),
                     figures = file.path(test_dir, "sections", "figures"),
                     maps = file.path(test_dir, "sections", "maps"))
for (folder in section_dirs) {
  dir.create(folder, recursive = TRUE)
}

# Without some columns: the rows and blocks that rest on them drop out
no_mix <- experts %>%
  select(-starts_with("mix25_"), -starts_with("cpd_"))
no_mix_selection <- section_selection(no_mix, vignettes, section_dirs)
no_mix_completion <- section_completion(no_mix, vignettes, section_dirs)
no_mix_later <- readLines(file.path(section_dirs$tables,
                                    "tab_selection_later_all.tex"),
                          encoding = "UTF-8")
stopifnot(!any(str_detect(no_mix_later, "Heating systems installed")),
          !any(str_detect(no_mix_later, "Continuing education")))
item_row <- table_row(file.path(section_dirs$tables, "tab_item_response_all.tex"),
                      "\\\\quad Q11b Heating systems installed 2025")
stopifnot(item_row[3] == "--", item_row[4] == "--")

# Without the times: no metadata
no_times <- experts %>%
  select(-starts_with("time_"))
stopifnot(nrow(section_completion(no_times, vignettes, section_dirs)) == 0)

# A few break-offs: enough for the comparison in the first blocks, and not for later
# questions or the groups
breakoff_ids <- experts %>%
  filter(!is_complete, last_question_position >= 1) %>%
  arrange(desc(last_question_position)) %>%
  pull(resp_uid)
few_breakoffs <- experts %>%
  filter(is_complete | resp_uid %in% breakoff_ids[seq_len(3 * MIN_N)])
few_vignettes <- vignettes %>%
  filter(resp_uid %in% few_breakoffs$resp_uid)
few_selection <- section_selection(few_breakoffs, few_vignettes, section_dirs)
stopifnot("selection" %in% few_selection$id,
          all(file.exists(file.path(test_dir, "sections", few_selection$file))))

# Too few break-offs: nothing to compare, no break-off exhibits
too_few <- experts %>%
  filter(is_complete | resp_uid %in% breakoff_ids[seq_len(MIN_N - 1)])
too_few_vignettes <- vignettes %>%
  filter(resp_uid %in% too_few$resp_uid)
stopifnot(nrow(section_selection(too_few, too_few_vignettes, section_dirs)) == 0)
too_few_completion <- section_completion(too_few, too_few_vignettes, section_dirs)
stopifnot(setequal(unique(too_few_completion$id), c("duration", "vignette_times")))

cat("Deck, frame and sections: all checks passed\n")
