# =============================================================================
# 02_kl_divergence_prices.R  -  do the professional groups expect different
#                               energy prices?
# =============================================================================
# Run from the project root (open energy_expert_survey.Rproj, then "Source"),
# or in a terminal:  Rscript R/first_policy_brief/02_kl_divergence_prices.R
#
# Q17 asks how the prices of five fuels will develop, on an ordered scale
#   1 "mehr als 10% niedriger" ... 5 "mehr als 10% höher".
# For each fuel we compare SHK-Handwerk, Schornsteinfeger and Energieberater:
#
#   A. G-test          Are the complete answer distributions different?
#   B. Kruskal-Wallis  Does at least one group answer systematically higher
#                      or lower on the ordered scale?
#   C. Pairwise        Which pairs of groups differ, and in which direction?
#      Wilcoxon
#
# Output: FIRST_POLICY_BRIEF_DIR/preiserwartungen_tests.xlsx
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source("R/prepare_analysis_data.R")                 # also loads config.R
source("R/summary_statistics/00_settings.R")        # SAMPLES


# =============================================================================
# 0. Settings
# =============================================================================
FUELS <- c(
  pexp_gas    = "Gas",
  pexp_hpelec = "Wärmepumpenstrom",
  pexp_dh     = "Fernwärme",
  pexp_oil    = "Heizöl",
  pexp_pellet = "Pellets"
)

# Answers from lowest to highest; the position becomes the score 1-5. Labels
# must match the data exactly (note "10%" vs "10 %").
ANSWERS <- c(
  "mehr als 10% niedriger",
  "bis zu 10 % niedriger",
  "ungefähr gleich",
  "bis zu 10 % höher",
  "mehr als 10% höher"
)

# The three pairwise comparisons, as (group_1, group_2).
PAIRS <- list(
  c("SHK-Handwerk",     "Schornsteinfeger"),
  c("SHK-Handwerk",     "Energieberater"),
  c("Schornsteinfeger", "Energieberater")
)

ALPHA <- 0.05         # significance level for the Holm-adjusted p-values
N_SIM <- 10000        # simulated tables for the Monte Carlo G-test p-value
set.seed(20260921)    # makes the simulated p-values reproducible


# =============================================================================
# 1. Prepare data: one row per expert and fuel
# =============================================================================
# Sample: submitted questionnaires; experts without a berufsgruppe excluded.
sample_spec <- SAMPLES %>% filter(folder == "completer")
dat <- load_analysis_data(completers_only = sample_spec$completers_only)

long <- dat$main %>%
  filter(berufsgruppe != "Keine Zuordnung") %>%
  mutate(berufsgruppe = droplevels(berufsgruppe)) %>%
  select(resp_uid, berufsgruppe, all_of(names(FUELS))) %>%
  pivot_longer(all_of(names(FUELS)), names_to = "fuel", values_to = "answer") %>%
  filter(!is.na(answer)) %>%          # a skipped fuel drops out for that fuel only
  mutate(
    fuel   = factor(FUELS[fuel], levels = FUELS),
    answer = factor(as.character(answer), levels = ANSWERS, ordered = TRUE),
    score  = as.integer(answer)       # 1 (much lower) ... 5 (much higher)
  )

# A label in the data that is not in ANSWERS would silently become NA above.
stopifnot(!anyNA(long$answer))


# =============================================================================
# 2. Descriptive statistics
# =============================================================================
# The tests below must always be read alongside these distributions.

# Share of each answer, per fuel and group (one column per answer).
shares <- long %>%
  count(fuel, berufsgruppe, answer, .drop = FALSE) %>%
  group_by(fuel, berufsgruppe) %>%
  mutate(n_group = sum(n), share = n / n_group) %>%
  ungroup() %>%
  select(fuel, berufsgruppe, n = n_group, answer, share) %>%
  pivot_wider(names_from = answer, values_from = share)

# Summary per fuel and group. The mean score is descriptive only: it treats
# the five categories as equally spaced, which the tests below do not assume.
summary_groups <- long %>%
  group_by(fuel, berufsgruppe) %>%
  summarise(
    n               = n(),
    mean_score      = mean(score),
    median_score    = median(score),
    share_increase  = mean(score >= 4),   # "bis zu 10 % höher" or more
    .groups = "drop"
  )


# -------------------------------------------------------------------------
# 3. G-test of independence
# Question: Do the groups distribute their answers differently across the
#           five categories?
# H0: The five-category distribution is the same for all three groups.
# A small p-value rejects H0. The test treats the categories as unordered,
#   so it does not tell us which group expects higher prices.
#
# G = 2 * sum(O * log(O / E)), with E the expected counts under independence.
# (G = 2 * N * mutual information, i.e. the KL-divergence view of the test.)
# Some expected counts are below 5, so the chi-squared approximation may be
# poor. We therefore also simulate N_SIM tables with the same row and column
# totals (r2dtable) and take the share with a G at least as large as observed.
#
# Cramer's V: How strong is the association between group and answer?
#   0 = identical distributions, 1 = group fully determines the answer.
#   It is computed from Pearson's chi-squared statistic.
# -------------------------------------------------------------------------

# G for a table of counts; empty cells are skipped since 0 * log(0) = 0.
g_statistic <- function(tab) {
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  keep <- tab > 0
  2 * sum(tab[keep] * log(tab[keep] / expected[keep]))
}

g_results <- list()
for (f in levels(long$fuel)) {
  d   <- long %>% filter(fuel == f)
  tab <- table(d$berufsgruppe, d$answer)       # 3 groups x 5 answers
  tab <- tab[, colSums(tab) > 0, drop = FALSE] # drop answers nobody gave

  n        <- sum(tab)
  expected <- outer(rowSums(tab), colSums(tab)) / n
  G        <- g_statistic(tab)
  df       <- (nrow(tab) - 1) * (ncol(tab) - 1)

  G_sim <- sapply(r2dtable(N_SIM, rowSums(tab), colSums(tab)), g_statistic)

  X2 <- sum((tab - expected)^2 / expected)     # Pearson chi-squared

  g_results[[f]] <- tibble(
    fuel          = f,
    n             = n,
    G             = G,
    df            = df,
    p_chisq       = pchisq(G, df, lower.tail = FALSE),
    p_sim         = (1 + sum(G_sim >= G)) / (1 + N_SIM),  # smallest: 1/10001
    cells_below_5 = sum(expected < 5),
    min_expected  = min(expected),
    cramers_v     = sqrt(X2 / (n * (min(dim(tab)) - 1)))
  )
}
g_results <- bind_rows(g_results) %>%
  mutate(p_sim_holm = p.adjust(p_sim, method = "holm"))   # across 5 fuels


# -------------------------------------------------------------------------
# 4. Kruskal-Wallis test
# Question: Does at least one group tend to give systematically higher or
#           lower answers on the ordered 1-5 scale?
# H0: The groups do not differ systematically in their position on the scale.
# A small p-value rejects H0. It does not say WHICH group differs; that is
#   what the pairwise tests in section 5 are for.
#
# The test ranks all answers of a fuel together (ties get the average rank)
# and asks whether the average rank differs between groups. It uses the order
# of the categories but not their distances, so it is neither a test of equal
# means nor, strictly, of equal medians.
#
# epsilon_sq = H / (n - 1): share of the variation in ranks that lies between
#   groups (0 = none, 1 = all). It measures how well the groups separate on
#   the rank scale, NOT a percentage difference in expected prices.
# -------------------------------------------------------------------------
kw_results <- list()
for (f in levels(long$fuel)) {
  d  <- long %>% filter(fuel == f)
  kw <- kruskal.test(score ~ berufsgruppe, data = d)
  kw_results[[f]] <- tibble(
    fuel       = f,
    n          = nrow(d),
    H          = unname(kw$statistic),
    df         = unname(kw$parameter),
    p          = kw$p.value,
    epsilon_sq = unname(kw$statistic) / (nrow(d) - 1)
  )
}
kw_results <- bind_rows(kw_results) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))           # across 5 fuels


# -------------------------------------------------------------------------
# 5. Pairwise Wilcoxon rank-sum (Mann-Whitney) tests
# Question: Which pairs of groups differ on the ordered scale, and which
#           group of the pair tends to answer higher?
# H0: The two groups' answer distributions are the same.
# A small p-value rejects H0. The direction comes from prob_1_higher.
#
# prob_1_higher = P(group 1 higher) + 0.5 * P(same answer), comparing a random
#   member of group 1 with a random member of group 2 (ties count half).
#   0.5 = no tendency; > 0.5 group 1 tends higher; < 0.5 group 2 tends higher.
#   It equals W / (n_1 * n_2), R's Wilcoxon statistic scaled to 0-1.
#
# exact = FALSE: the answers have many ties, so R uses the tie-corrected normal
#   approximation. Two-sided tests; Holm correction across all 15 comparisons.
# -------------------------------------------------------------------------
pair_results <- list()
for (f in levels(long$fuel)) {
  d <- long %>% filter(fuel == f)
  for (pair in PAIRS) {
    x <- d$score[d$berufsgruppe == pair[1]]
    y <- d$score[d$berufsgruppe == pair[2]]
    w <- wilcox.test(x, y, exact = FALSE)

    pair_results[[length(pair_results) + 1]] <- tibble(
      fuel          = f,
      group_1       = pair[1],
      group_2       = pair[2],
      n_1           = length(x),
      n_2           = length(y),
      mean_1        = mean(x),                        # descriptive only
      mean_2        = mean(y),
      prob_1_higher = mean(outer(x, y, ">")) + 0.5 * mean(outer(x, y, "==")),
      W             = unname(w$statistic),
      p             = w$p.value
    )
  }
}
pair_results <- bind_rows(pair_results) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))           # across 15 pairs


# =============================================================================
# 6. Clean output tables
# =============================================================================
# significant_holm: TRUE if the Holm-adjusted p-value is below ALPHA.
# direction: which group of the pair tends to answer higher (point estimate).
#   Treat a direction as statistically supported only if significant_holm.
g_results <- g_results %>%
  mutate(significant_holm = p_sim_holm < ALPHA)

kw_results <- kw_results %>%
  mutate(significant_holm = p_holm < ALPHA)

pair_results <- pair_results %>%
  mutate(
    direction = case_when(
      prob_1_higher > 0.5 ~ paste(group_1, "higher"),
      prob_1_higher < 0.5 ~ paste(group_2, "higher"),
      TRUE                ~ "no directional difference"
    ),
    significant_holm = p_holm < ALPHA
  )

readme <- tibble(
  test = c("G-test", "Kruskal-Wallis", "Pairwise Wilcoxon"),
  sheet = c("g_test", "kruskal_wallis", "paarvergleiche"),
  question = c(
    "Are the full five-category response distributions different across the three professions?",
    "Do the three professions differ systematically on the ordered low-to-high response scale?",
    "Which two professions differ on the ordered response scale?"
  ),
  interpretation = c(
    "Significant -> at least one profession has a different distribution.",
    "Significant -> at least one profession tends to answer higher or lower.",
    "Use p_holm for significance and prob_1_higher for direction and size."
  ),
  direction = c(
    "No. The test ignores the order of the categories.",
    "Not by itself; see the pairwise tests.",
    "prob_1_higher > 0.5 -> group_1 higher; < 0.5 -> group_2 higher."
  ),
  effect_size = c(
    "cramers_v: strength of association between profession and answer (0 = none, 1 = perfect).",
    "epsilon_sq: share of rank variation between professions; not a price difference.",
    "prob_1_higher: P(group_1 higher) + 0.5 * P(tie) for a random pair of respondents."
  ),
  multiple_testing = c(
    "Holm across 5 tests (one per fuel), applied to the simulated p-value p_sim.",
    "Holm across 5 tests (one per fuel).",
    "Holm across 15 tests (5 fuels x 3 pairs)."
  )
)


# =============================================================================
# 7. Print and save
# =============================================================================
show <- function(title, x) {
  cat("\n==== ", title, "\n", sep = "")
  print(as.data.frame(mutate(x, across(where(is.double), ~ signif(.x, 3)))),
        row.names = FALSE)
}

show("Groups: n, scores and share expecting a price increase", summary_groups)
show("A. G-test: are the full distributions different?", g_results)
show("B. Kruskal-Wallis: does any group answer higher or lower?", kw_results)
show("C. Pairwise Wilcoxon: which group tends to answer higher?", pair_results)

dir.create(FIRST_POLICY_BRIEF_DIR, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(FIRST_POLICY_BRIEF_DIR, "preiserwartungen_tests.xlsx")
write_xlsx(
  list(
    readme         = readme,
    anteile        = shares,
    gruppen        = summary_groups,
    g_test         = g_results,
    kruskal_wallis = kw_results,
    paarvergleiche = pair_results
  ),
  out_file
)
message("saved: ", out_file)
