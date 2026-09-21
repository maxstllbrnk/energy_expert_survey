# =============================================================================
# 00_figures.R  -  the figures the first policy brief draws itself
# =============================================================================
# Sourced by 01_figure_report.R, not run on its own.
#
# Most figures of the brief are those of the summary statistics, which
# 01_figure_report.R copies as they are. The few the brief needs in another
# form are drawn here, with the same plot functions and so the same look:
#
#   Q10b_SQ001_adv_share_hp      the share of heat pumps in the recommendations
#   Q11b_WP_mix25_hp             and in the installations of SHK firms, in bins
#                                of 10 instead of 5 percentage points
#   empfehlungen_nach_baujahr_   the recommendations by year of construction,
#     ohne_1970_unsaniert        without the unrenovated 1970 houses
#
# Each is written to tempdir()/eigene/<group>/<file>.png/.svg, which FIGURES in
# 01_figure_report.R lists as eigene/<group>/<file>, and the report copies it
# into abbildungen/ like every other figure.
#
# Reads the cleaned data from CLEAN_DATA_DIR, so the report needs the secure
# location.
# =============================================================================

# Emptied first, so that a second run in the same R session cannot pick up a
# figure that is no longer drawn.
out_dir <- file.path(tempdir(), "eigene")
unlink(out_dir, recursive = TRUE)

# The brief shows submitted questionnaires only. The plot functions write the
# sample and the group into the caption, so group_label is set before each figure.
sample_spec <- SAMPLES %>% filter(folder == "completer")
dat <- load_analysis_data(completers_only = sample_spec$completers_only)


# =============================================================================
# 1. heat pump shares, in bins of 10 percentage points
# =============================================================================
# Most experts answered in multiples of 10. With the bins of 5 of the summary
# statistics, every bin ending on a multiple of 10 collects them and the bin
# before it stays nearly empty, so the histogram zigzags. Bins of 10 hold one
# multiple of 10 each. Outlier rule, mean and median are unchanged.
BINWIDTH_SHARES <- 10

group_label <- ALL_GROUP
dat$main %>%
  plot_histogram("adv_share_hp", x_label = "Anteil in %", binwidth = BINWIDTH_SHARES) %>%
  save_figure(folder_name(group_label), file_name("adv_share_hp"))

# Q11b was asked of SHK firms only.
group_label <- "SHK-Handwerk"
dat$main %>%
  filter(berufsgruppe == group_label) %>%
  plot_histogram("mix25_hp", x_label = "Anteil in %", binwidth = BINWIDTH_SHARES) %>%
  save_figure(folder_name(group_label), file_name("mix25_hp"))


# =============================================================================
# 2. recommendations by year of construction, same renovation levels
# =============================================================================
# The vignette design combines 1970 with all three renovation levels but 2000
# only with teilsaniert and vollständig saniert: there is no unrenovated 2000
# house. By Baujahr alone, only the 1970 bars contain houses at 200 kWh/m², so
# they differ in renovation as well as in age. Without the unrenovated 1970
# houses, both years have the same two renovation levels.
group_label <- ALL_GROUP

vig <- dat$vignettes %>%
  filter(!is.na(vig_rec), !is.na(vig_arm)) %>%
  mutate(arm         = factor(unname(ARM_LABELS[as.character(vig_arm)]), levels = ARM_LABELS),
         vig_rec     = fct_relevel(vig_rec, names(TECH_COLORS)),
         unrenovated = str_detect(att_renovation, "nicht saniert"))

# If the design ever gets unrenovated 2000 houses, this comparison is no longer needed.
if (any(vig$unrenovated & vig$att_build_year == "2000"))
  stop("There are unrenovated 2000 houses in the vignettes, so leaving out the ",
       "unrenovated 1970 ones no longer makes the years comparable.", call. = FALSE)

vig %>%
  filter(!(unrenovated & att_build_year == "1970")) %>%
  plot_stacked(y = "att_build_year", fill = "vig_rec", colors = TECH_COLORS, facet = "arm",
               title = question_text("vig_rec"),
               subtitle = str_c("Nach Vignettenmerkmal: Baujahr des Gebäudes · ohne die Vignetten mit ",
                                "Baujahr 1970 und nicht saniert (200 kWh/m²), da Baujahr 2000 nur ",
                                "teil- oder vollständig saniert vorkommt")) %>%
  save_figure(folder_name(group_label), "empfehlungen_nach_baujahr_ohne_1970_unsaniert", height = 11.5)
