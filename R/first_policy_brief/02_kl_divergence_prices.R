# =============================================================================
# 02_kl_divergence_prices.R  -  do the occupational groups expect different
#                               energy prices?
# =============================================================================
# In RStudio: open energy_expert_survey.Rproj, open this file, click "Source".
# In a terminal, from the project folder:
#   Rscript R/first_policy_brief/02_kl_divergence_prices.R
#
# Question Q17 asks every expert how the price of five fuels will develop, on a
# five-point scale from "mehr als 10% niedriger" to "mehr als 10% höher". For
# each fuel separately, this script compares the answers of the three
# berufsgruppen (SHK-Handwerk, Schornsteinfeger, Energieberater) in two steps:
#
#   A. Are the answer DISTRIBUTIONS different?          G-test (part 3)
#      Treats the five answers as unordered categories. Detects any kind of
#      difference, but cannot say which group expects higher prices.
#
#   B. Does one group expect HIGHER prices than another? rank tests (part 4)
#      Uses the order of the answers. Kruskal-Wallis for "any group higher or
#      lower than the others", then pairwise Wilcoxon-Mann-Whitney tests for
#      "which group is higher than which".
#
# Every test is run once per fuel, so there are 5 G-tests, 5 Kruskal-Wallis
# tests and 5 x 3 = 15 pairwise tests. Within each of these families the
# p-values are corrected for multiple testing (Holm).
#
# Part 5 recomputes the statistics with DescTools, rcompanion and effsize and
# stops if any number differs, and runs a Brunner-Munzel test as a robustness
# check of the pairwise comparisons.
#
# Output, printed to the console and saved to
#   FIRST_POLICY_BRIEF_DIR/preiserwartungen_tests.xlsx
# with one sheet each for the answer shares, group summaries, three tests and
# the pairwise cross-check.
# =============================================================================

if (!file.exists("config.R"))
  stop("Run this from the project root (the folder containing config.R).\n",
       "In RStudio, open energy_expert_survey.Rproj first.", call. = FALSE)

source("R/prepare_analysis_data.R")                 # also loads config.R
source("R/summary_statistics/00_settings.R")        # SAMPLES


# =============================================================================
# 0. settings
# =============================================================================
# The five price questions, and the name each fuel gets in the output.
FUELS <- c(
  pexp_gas    = "Gas",
  pexp_hpelec = "Wärmepumpenstrom",
  pexp_dh     = "Fernwärme",
  pexp_oil    = "Heizöl",
  pexp_pellet = "Pellets"
)

# The answers from lowest to highest. The position in this vector becomes the
# score used by the rank tests: 1 = "mehr als 10% niedriger" ...
# 5 = "mehr als 10% höher". The labels must match the data exactly (note the
# different spacing in "10%" and "10 %").
ANSWERS <- c(
  "mehr als 10% niedriger",
  "bis zu 10 % niedriger",
  "ungefähr gleich",
  "bis zu 10 % höher",
  "mehr als 10% höher"
)

N_SIM <- 10000        # simulated tables for the simulated G-test p-value
set.seed(20260921)    # so the simulated p-values are the same on every run


# =============================================================================
# 1. the data: one row per expert and fuel
# =============================================================================
# The brief shows submitted questionnaires only.
sample_spec <- SAMPLES %>% filter(folder == "completer")
dat <- load_analysis_data(completers_only = sample_spec$completers_only)

long <- dat$main %>%
  # Experts without a berufsgruppe are not part of the comparison.
  filter(berufsgruppe != "Keine Zuordnung") %>%
  mutate(berufsgruppe = droplevels(berufsgruppe)) %>%
  select(resp_uid, berufsgruppe, all_of(names(FUELS))) %>%
  # Wide -> long: the five price columns become one column "answer", plus a
  # column "fuel" that says which question the answer belongs to.
  pivot_longer(all_of(names(FUELS)), names_to = "fuel", values_to = "answer") %>%
  # An expert who skipped a fuel drops out for that fuel only.
  filter(!is.na(answer)) %>%
  mutate(
    fuel   = factor(FUELS[fuel], levels = FUELS),
    answer = factor(as.character(answer), levels = ANSWERS, ordered = TRUE),
    score  = as.integer(answer)       # 1 (much lower) ... 5 (much higher)
  )

# A label in the data that is not in ANSWERS would silently become NA above.
stopifnot(!anyNA(long$answer))


# =============================================================================
# 2. describe the answers first
# =============================================================================
# 2a. Share of each answer, per fuel and berufsgruppe. These are the
# distributions the G-test compares. .drop = FALSE keeps answers nobody in a
# group gave, as a count of 0.
shares <- long %>%
  count(fuel, berufsgruppe, answer, .drop = FALSE) %>%
  group_by(fuel, berufsgruppe) %>%
  mutate(total = sum(n), share = n / total) %>%
  ungroup()

# 2b. One line per fuel and berufsgruppe: how many answered, the mean and
# median score, and the share expecting higher prices (score 4 or 5). These are
# what the rank tests compare, and what to quote next to their p-values.
summary_groups <- long %>%
  group_by(fuel, berufsgruppe) %>%
  summarise(
    n            = n(),
    mean_score   = mean(score),
    median_score = median(score),
    share_higher = mean(score >= 4),
    .groups = "drop"
  )


# =============================================================================
# 3. A: are the distributions different?  G-test of independence
# =============================================================================
# For one fuel, the answers form a table with 3 rows (berufsgruppen) and 5
# columns (answers). If all groups had the same distribution, the expected
# count in a cell would be
#
#     expected = row total * column total / N
#
# The G statistic measures how far the observed counts are from that:
#
#     G = 2 * sum( observed * log(observed / expected) )
#
# This is where the Kullback-Leibler divergence comes in. G / (2N) is the
# mutual information between berufsgruppe and answer, which equals the
# size-weighted average KL divergence between each group's distribution and the
# pooled distribution of all groups. It is 0 if all groups answer the same way,
# and grows the more they differ. We report it as the effect size (in nats).
#
# If the groups do not differ, G roughly follows a chi-squared distribution
# with (rows - 1) * (columns - 1) = 8 degrees of freedom. That approximation
# gets unreliable when some expected counts are small (rule of thumb: below 5),
# which happens for the rare "niedriger" answers. So we also compute a
# simulated p-value that needs no approximation: draw N_SIM random tables with
# the same row and column totals as the real one (i.e. tables in which the
# groups do NOT differ), and count how often their G is at least as large as
# the real G.

# G for a table of counts. Cells with 0 are left out, because
# 0 * log(0) is taken to be 0.
g_statistic <- function(tab) {
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  keep <- tab > 0
  2 * sum(tab[keep] * log(tab[keep] / expected[keep]))
}

# The full test for one fuel: group and answer are the two columns of data.
g_test <- function(group, answer) {
  tab <- table(group, answer)
  # An answer nobody gave carries no information and would add a column of
  # expected counts of 0, so it is dropped.
  tab <- tab[, colSums(tab) > 0, drop = FALSE]

  G  <- g_statistic(tab)
  df <- (nrow(tab) - 1) * (ncol(tab) - 1)

  # Random tables with the same margins; r2dtable() is base R.
  G_sim <- sapply(r2dtable(N_SIM, rowSums(tab), colSums(tab)), g_statistic)

  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  tibble(
    n             = sum(tab),
    G             = G,
    df            = df,
    p_chisq       = pchisq(G, df, lower.tail = FALSE),
    p_sim         = (1 + sum(G_sim >= G)) / (1 + N_SIM),
    cells_below_5 = sum(expected < 5),
    min_expected  = min(expected),
    mutual_info   = G / (2 * sum(tab))
  )
}

g_results <- list()
for (f in levels(long$fuel)) {
  d <- long %>% filter(fuel == f)
  g_results[[f]] <- g_test(d$berufsgruppe, d$answer) %>% mutate(fuel = f, .before = 1)
}
g_results <- bind_rows(g_results) %>%
  # Holm correction over the 5 fuels, applied to the simulated p-values.
  # With N_SIM = 10000 the smallest possible p_sim is 1/10001, about 0.0001.
  mutate(p_sim_holm = p.adjust(p_sim, method = "holm"))


# =============================================================================
# 4. B: does one group expect higher prices?  rank tests
# =============================================================================
# The G-test would give the same result if the five answers were shuffled into
# any other order. The rank tests use the order: they only ask whether the
# answers of one group tend to lie further up the scale than those of another.
# They use only the ranks of the scores 1-5, so they do not assume that the
# steps between answers are equally large. Ties (many experts giving the same
# answer) are handled by the tests' built-in tie correction.

# 4a. Kruskal-Wallis: do the three groups differ in their typical position on
# the scale? It is the rank-based version of a one-way ANOVA. As effect size we
# report epsilon-squared = H / (n - 1): the share of the variation in ranks
# explained by the berufsgruppe (0 = none, 1 = all).
kw_results <- list()
for (f in levels(long$fuel)) {
  d  <- long %>% filter(fuel == f)
  kw <- kruskal.test(score ~ berufsgruppe, data = d)
  kw_results[[f]] <- tibble(
    fuel        = f,
    n           = nrow(d),
    H           = unname(kw$statistic),
    df          = unname(kw$parameter),
    p           = kw$p.value,
    epsilon_sq  = unname(kw$statistic) / (nrow(d) - 1)
  )
}
kw_results <- bind_rows(kw_results) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))

# 4b. Pairwise Wilcoxon-Mann-Whitney tests: which group is higher than which?
#
# The effect size is the probability of superiority: take one random expert
# from group 1 and one from group 2. How likely is it that the expert from
# group 1 expects the higher price? Ties count half.
#
#     prob_1_higher = P(group 1 higher) + 0.5 * P(same answer)
#
#   0.5          no tendency either way
#   above 0.5    group 1 tends to expect higher prices than group 2
#   below 0.5    group 1 tends to expect lower prices than group 2
#
# (Also known as Vargha-Delaney A, or the common-language effect size.)
# The Wilcoxon test checks whether this probability differs from 0.5.

# Compares every expert in x with every expert in y, and averages.
prob_higher <- function(x, y) {
  mean(outer(x, y, ">") + 0.5 * outer(x, y, "=="))
}

pairs <- combn(levels(long$berufsgruppe), 2, simplify = FALSE)

pair_results <- list()
for (f in levels(long$fuel)) {
  d <- long %>% filter(fuel == f)
  for (pair in pairs) {
    x <- d$score[d$berufsgruppe == pair[1]]
    y <- d$score[d$berufsgruppe == pair[2]]
    # exact = FALSE: with this many ties the exact p-value cannot be computed,
    # so R uses the normal approximation with tie correction anyway.
    w <- wilcox.test(x, y, exact = FALSE)
    p_sup <- prob_higher(x, y)

    # Check: R's W statistic is the number of pairs in which x is higher (ties
    # counting half), so W / (n_1 * n_2) must equal prob_1_higher.
    stopifnot(isTRUE(all.equal(p_sup, unname(w$statistic) / (length(x) * length(y)))))

    pair_results[[length(pair_results) + 1]] <- tibble(
      fuel          = f,
      group_1       = pair[1],
      group_2       = pair[2],
      n_1           = length(x),
      n_2           = length(y),
      mean_1        = mean(x),
      mean_2        = mean(y),
      prob_1_higher = p_sup,
      W             = unname(w$statistic),
      p             = w$p.value
    )
  }
}
pair_results <- bind_rows(pair_results) %>%
  # Holm correction over all 15 comparisons (5 fuels x 3 pairs).
  mutate(p_holm = p.adjust(p, method = "holm"))


# =============================================================================
# 5. cross-check with independent implementations
# =============================================================================
# The statistics above are computed by hand so every step is visible. Here the
# same numbers are recomputed with published R packages. The script stops if
# they disagree:
#
#   G                DescTools::GTest()           (no continuity correction)
#   epsilon-squared  rcompanion::epsilonSquared()
#   prob_1_higher    effsize::VD.A()              (Vargha-Delaney A)
#
# One more column is a robustness check, not a recomputation. The Wilcoxon
# test's reading as "group 1 tends to be higher" assumes the two groups'
# distributions have a similar shape. The Brunner-Munzel test
# (brunnermunzel::brunnermunzel.test) drops that assumption and tests
# prob_1_higher = 0.5 directly. If its p-value leads to the same conclusion as
# the Wilcoxon p-value, the shape assumption does not matter here. Its estimate
# is P(group 1 lower) + 0.5 * P(same answer), which is 1 - prob_1_higher.

check_g <- list()
check_kw <- list()
for (f in levels(long$fuel)) {
  d   <- long %>% filter(fuel == f)
  tab <- table(d$berufsgruppe, d$answer)
  tab <- tab[, colSums(tab) > 0, drop = FALSE]
  check_g[[f]] <- tibble(
    fuel        = f,
    G_script    = g_results$G[g_results$fuel == f],
    G_DescTools = unname(DescTools::GTest(tab)$statistic)
  )
  check_kw[[f]] <- tibble(
    fuel                  = f,
    epsilon_sq_script     = kw_results$epsilon_sq[kw_results$fuel == f],
    epsilon_sq_rcompanion = unname(rcompanion::epsilonSquared(d$score, d$berufsgruppe,
                                                              digits = 10))
  )
}
check_g  <- bind_rows(check_g)
check_kw <- bind_rows(check_kw)

check_pairs <- list()
for (i in seq_len(nrow(pair_results))) {
  r <- pair_results[i, ]
  d <- long %>% filter(fuel == r$fuel)
  x <- d$score[d$berufsgruppe == r$group_1]
  y <- d$score[d$berufsgruppe == r$group_2]
  bm <- brunnermunzel::brunnermunzel.test(x, y)
  check_pairs[[i]] <- tibble(
    fuel               = r$fuel,
    group_1            = r$group_1,
    group_2            = r$group_2,
    prob_1_higher      = r$prob_1_higher,
    prob_effsize_VD.A  = unname(effsize::VD.A(x, y)$estimate),
    prob_brunnermunzel = 1 - unname(bm$estimate),
    p_wilcoxon         = r$p,
    p_brunnermunzel    = bm$p.value
  )
}
check_pairs <- bind_rows(check_pairs)

stopifnot(
  isTRUE(all.equal(check_g$G_script, check_g$G_DescTools)),
  isTRUE(all.equal(check_kw$epsilon_sq_script, check_kw$epsilon_sq_rcompanion)),
  isTRUE(all.equal(check_pairs$prob_1_higher, check_pairs$prob_effsize_VD.A)),
  isTRUE(all.equal(check_pairs$prob_1_higher, check_pairs$prob_brunnermunzel))
)


# =============================================================================
# 6. print and save
# =============================================================================
show <- function(title, x) {
  cat("\n==== ", title, "\n", sep = "")
  print(as.data.frame(mutate(x, across(where(is.double), ~ signif(.x, 3)))),
        row.names = FALSE)
}

show("Groups: n, mean score (1-5) and share expecting higher prices", summary_groups)
show("A. G-test: are the distributions different?", g_results)
show("B1. Kruskal-Wallis: does any group lie higher or lower?", kw_results)
show("B2. Pairwise: prob_1_higher > 0.5 means group 1 expects higher prices", pair_results)
show("Check: pairwise, against effsize and Brunner-Munzel", check_pairs)
message("\nAll statistics match DescTools, rcompanion and effsize.")

dir.create(FIRST_POLICY_BRIEF_DIR, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(FIRST_POLICY_BRIEF_DIR, "preiserwartungen_tests.xlsx")
write_xlsx(
  list(
    anteile        = shares,
    gruppen        = summary_groups,
    g_test         = g_results,
    kruskal_wallis = kw_results,
    paarvergleiche = pair_results,
    kontrolle      = check_pairs
  ),
  out_file
)
message("saved: ", out_file)
