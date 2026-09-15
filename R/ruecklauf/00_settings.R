# =============================================================================
# 00_settings.R  -  every choice the response-rate analysis depends on
# =============================================================================
# Change things here rather than in the scripts. Sourced by R/run_ruecklauf.R,
# after R/summary_statistics/00_settings.R, whose colours, figure size and
# number formats this analysis deliberately reuses so that the figures look
# like the ones in the descriptive report.
# =============================================================================

if (!file.exists(FRAME_FILE))
  stop("The sampling frame is missing:\n  ", FRAME_FILE,
       "\n\nIt is the denominator of every response rate. Set FRAME_FILE in ",
       "config.R\nif the file lives somewhere else on this machine.", call. = FALSE)


# =============================================================================
# the three samples the lists were drawn from
# =============================================================================
# `type` in the frame file names the register each contact was scraped from.
# These are the groups the response rate is split by - the definition the
# mailing lists were actually built on, not the self-report in Q2.
#
#   DENA              the dena list of energy advisors  -> Energieberater
#   Schornsteinfeger  the chimney-sweep register
#   SHK               the sanitary/heating/air-conditioning trade register
#
# The labels on the right are the ones used in the figures, and they match the
# BERUFSGRUPPEN of R/prepare_analysis_data.R so that frame and survey can be put
# side by side.
FRAME_TYPE_LABELS <- c(
  "SHK"              = "SHK-Handwerk",
  "Schornsteinfeger" = "Schornsteinfeger",
  "DENA"             = "Energieberater"
)

ALL_TYPES <- "Alle Berufsgruppen"
RUECKLAUF_GRUPPEN <- c(ALL_TYPES, unname(FRAME_TYPE_LABELS))


# =============================================================================
# t-online
# =============================================================================
# Telekom appears to have blocked part of the mailing: the dispatch log notes
# "nach dieser umfrage tendenziell geblocked von telekom??" and later waves were
# reminded "nicht t-online". Contacts whose invitation may never have arrived
# depress the response rate, so every figure is produced twice - once over the
# whole frame, once with t-online contacts removed from the denominator.
TONLINE_PATTERN <- "@t-online\\."

TONLINE_VARIANTS <- tribble(
  ~folder,          ~drop_tonline, ~label,
  "mit_tonline",    FALSE,         "Alle Kontakte",
  "ohne_tonline",   TRUE,          "Ohne t-online-Kontakte"
)


# =============================================================================
# what counts as a response
# =============================================================================
# Two definitions of the numerator, so that a regional pattern in break-offs
# becomes visible: everyone who opened the questionnaire, and everyone who
# submitted it.
RUECKLAUF_SAMPLES <- tribble(
  ~folder,                    ~completers_only, ~label,
  "alle_antworten",           FALSE,            "Alle Antworten",
  "vollstaendige_antworten",  TRUE,             "Nur abgeschlossene Fragebögen"
)


# =============================================================================
# how a respondent is assigned to one of the three samples
# =============================================================================
# The frame file carries no token, and the survey carries no e-mail, so a
# respondent cannot be matched to their frame row. The denominator therefore
# uses the frame's own `type`; the numerator has to be classified from the
# questionnaire.
#
#   "q2"     add_berufsgruppe() from Q2 - the closest available proxy. The
#            cross-tabulation of dispatch target group against Q2 supports it:
#            Firmen waves are 84 % SHK and hold no Schornsteinfeger at all.
#   "token"  the exact route, used automatically as soon as a token -> frame
#            bridge exists (see TOKEN_BRIDGE_FILE below).
#
# Whichever is used is stated in every caption, in tabellen_ruecklauf.xlsx and
# in the report, so no figure can be read as if the two sides matched exactly.
NUMERATOR_SOURCE_LABELS <- c(
  "q2"    = str_c("Nenner: Stichprobenrahmen (DENA / Schornsteinfeger / SHK). ",
                  "Zähler: Selbstauskunft Q2 - der Rahmen enthält keinen Token, ",
                  "eine exakte Zuordnung der Befragten ist daher nicht möglich."),
  "token" = str_c("Nenner und Zähler aus dem Stichprobenrahmen, über den Token ",
                  "exakt zugeordnet.")
)

# An optional CSV with the columns `wave`, `token` and `id` (the frame's id),
# i.e. the bridge the 47 LimeSurvey token lists would provide. When it appears
# next to the frame file, 02_link.R uses it and the Berufsgruppe split becomes
# exact; until then nothing looks for it beyond file.exists().
TOKEN_BRIDGE_FILE <- file.path(dirname(FRAME_FILE), "token_frame_bridge.csv")


# =============================================================================
# output
# =============================================================================
RUECKLAUF_DIR <- file.path(OUTPUT_DIR, "ruecklauf")

# Sub-folders inside one sample folder, and their headings in the report.
RUECKLAUF_SECTIONS <- c(
  "01_ueberblick"    = "Überblick",
  "02_karten"        = "Rücklauf nach Bundesland",
  "03_berufsgruppen" = "Rücklauf nach Berufsgruppe"
)

# Below this many contacts a cell is not drawn or shown, so that a rate built
# on a handful of people is never plotted.
MIN_KONTAKTE <- 30

# Response rates sit between 2 % and 15 %, and whole percent would collapse
# most of that range onto three values - percent_de() from the descriptive
# report rounds to 1 % and is right for the shares it is used for there, but
# not here. One decimal, German decimal comma.
percent_rate <- scales::label_percent(accuracy = 0.1, suffix = " %", decimal.mark = ",")

# The choropleths show a rate, not a count, so they need their own colour ramp
# end points; MAP_LOW/MAP_HIGH from the descriptive report are reused for the
# response rate and a warmer ramp marks the break-off rate, which is a different
# quantity and should not be mistaken for it.
MAP_RATE_LOW   <- MAP_LOW
MAP_RATE_HIGH  <- MAP_HIGH
MAP_ABBR_LOW   <- "#fde3d5"
MAP_ABBR_HIGH  <- "#8c2d0a"
