################################################################################
# section_market.R — Block E: what the firms offer, whom they serve, and which
#                    technologies they recommend and install
# Inputs : experts (docs/data_contract.md)
# Outputs: tables/tab_market_<group>.tex, tab_technology_shares_<group>.tex,
#          tab_technology_shares_groups_all.tex;
#          figures/fig_technology_shares_<group>.pdf
# Checks : unknown answers and the minimum cell (R/functions/exhibits.R)
################################################################################
# Q11c is free text. At most this many manufacturers are shown by name.
N_BRANDS_SHOWN <- 10
# Free text that names no manufacturer, as spelled after the harmonisation below;
# it counts as "Other". This list cannot be complete: another German word with at
# least MIN_N mentions would reach the table, so read the panel of Q11c of the
# first real run, and add the word here.
NOT_A_BRAND <- c("Sonstige", "Sonstiges", "Other", "Andere", "Keine", "Keiner",
                 "Keine Angabe", "Keine Ahnung", "K.a.", "K. A.", "Ka", "Weiß Nicht",
                 "Weiss Nicht", "Weiß Ich Nicht", "Weiss Ich Nicht", "Nicht Bekannt",
                 "Unbekannt", "Diverse", "Verschiedene", "Unterschiedlich",
                 "Mehrere", "Viele", "Alle", "-", "?")

#' The market: services, households, customers, manufacturers (appendix), and the
#' technologies among the recommendations and installations (main)
#'
#' @param experts one row per expert of the sample, as in experts.rds
#' @param dirs    the folders of the summary statistics: tables, figures, maps
#' @return the list of exhibits of this block
section_market <- function(experts, dirs) {
  block <- "E. The market"
  exhibits <- tibble()

  # 1. Services, households, customers, manufacturers --------------------------
  market_rows <- bind_rows(
    tibble(panel = "Services offered, several ticks possible (Q8)",
           variable = names(SERVICE_LABELS), kind = "level", level = "Ja",
           label = unname(SERVICE_LABELS), digits = 1),
    tibble(panel = "Households", variable = "adv_households_clean", kind = "num",
           level = NA_character_, label = "Households advised, last year (Q10a)",
           digits = 1),
    tibble(panel = "Most important customer group (Q7)", variable = "cust_rank1",
           kind = "level", level = names(CUSTOMER_LABELS),
           label = unname(CUSTOMER_LABELS), digits = 1),
    tibble(panel = "Second most important customer group (Q7)",
           variable = "cust_rank2", kind = "level",
           level = names(CUSTOMER_LABELS), label = unname(CUSTOMER_LABELS),
           digits = 1)
  )
  check_listed_answers(market_rows, experts)
  market_notes <- paste(
    "Shares among the experts who answered. Q8 and Q10a: firms with energy",
    "advice, engineering or chimney sweeping. Households advised on replacing the",
    "heating, without negative answers and without the",
    paste0(100 * TRIM_QUANTILES[1], "% lowest and highest answers of all experts."),
    "Q7: the customer groups for energy services; in one soft-launch wave a ranking",
    "of six places with the answer \"don't know\", which counts as not answered, so",
    "the question is not like for like across the waves; all waves are pooled. Q11c:",
    "HVAC firms, which manufacturer they sell most of the technology they",
    "install most; free text, harmonized in case and spacing only. Manufacturers",
    "with at least", MIN_N, "mentions, at most", N_BRANDS_SHOWN, "of them. Other:",
    "the rest, and answers that name no manufacturer, which stay in N.",
    cell_note()
  )

  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_experts <- subset_group(experts, group)

    # The manufacturers of this version: a name below the minimum cell would
    # describe too few experts
    brand_rows <- tibble()
    if ("brand_top1" %in% names(group_experts)) {
      # str_squish(): no spaces at the ends, one space within; str_to_title():
      # capital first letters
      brand <- str_to_title(str_squish(group_experts$brand_top1))
      # desc(): from most to fewest; row_number(): the place after sorting
      brands_shown <- tibble(brand = brand) %>%
        filter(!is.na(brand), !(brand %in% NOT_A_BRAND)) %>%
        count(brand) %>%
        filter(n >= MIN_N) %>%
        arrange(desc(n), brand) %>%
        filter(row_number() <= N_BRANDS_SHOWN) %>%
        pull(brand)
      group_experts$brand_shown <- case_when(
        is.na(brand) ~ NA_character_,
        brand %in% brands_shown ~ brand,
        TRUE ~ "Other"
      )
      brand_rows <- tibble(panel = "Manufacturer sold most (Q11c)",
                           variable = "brand_shown", kind = "level",
                           level = c(brands_shown, "Other"),
                           label = c(brands_shown, "Other"), digits = 1)
    }

    version_rows <- bind_rows(market_rows, brand_rows)
    n_answers <- answers_per_row(version_rows, group_experts)
    if (max(n_answers) < MIN_N) {
      next
    }
    file_name <- paste0("tab_market_", group_slug(group), ".tex")
    summary_table(group_experts, version_rows[n_answers > 0, ],
                  file.path(dirs$tables, file_name), market_notes)
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "market", group, "table", file.path("tables", file_name),
      "Services, households, customers and manufacturers", "appendix"
    ))
  }

  # 2. The technologies, table -------------------------------------------------
  shares_notes <- paste(
    "Q10b: share of each technology among the firm's recommendations of heating",
    "systems in the last year, asked of firms with energy advice, engineering or",
    "chimney sweeping. Q11b: share among the heating systems the firm installed",
    "in 2025, asked of HVAC firms. In percent. A blank counts as 0 for an expert",
    "who gave other shares. As reported, also where the shares do not sum to 100.",
    "Each firm counts once: the mean is not weighted by the number of",
    "recommendations or installations."
  )
  suffixes <- names(SHARE_TECHNOLOGY_LABELS)
  share_rows <- bind_rows(
    tibble(panel = "Share among the recommendations, last year (Q10b)",
           variable = paste0("adv_share_", suffixes, "_filled"),
           kind = "num", level = NA_character_,
           label = unname(SHARE_TECHNOLOGY_LABELS), digits = 1),
    tibble(panel = "Share among the installations, 2025 (Q11b)",
           variable = paste0("mix25_", suffixes, "_filled"),
           kind = "num", level = NA_character_,
           label = unname(SHARE_TECHNOLOGY_LABELS), digits = 1)
  )
  exhibits <- bind_rows(exhibits, summary_exhibits(
    experts, share_rows, "technology_shares",
    "Technologies among the recommendations and installations", shares_notes,
    block, "main", dirs
  ))

  # 3. The technologies, stacked bars ------------------------------------------
  questions <- tribble(
    ~prefix,      ~bar,
    "adv_share_", "Recommendations, last year (Q10b)",
    "mix25_",     "Installations, 2025 (Q11b)"
  )
  # The segments in the order of the colours, which keeps neighbours apart.
  # intersect(): what is in both vectors, in the order of the first
  technology_order <- intersect(names(TECHNOLOGY_COLOURS), SHARE_TECHNOLOGY_LABELS)
  for (group in c(ALL_EXPERTS, EXPERT_GROUPS)) {
    group_experts <- subset_group(experts, group)
    bars <- tibble()
    for (q in seq_len(nrow(questions))) {
      for (suffix in suffixes) {
        column <- paste0(questions$prefix[q], suffix, "_filled")
        if (!(column %in% names(group_experts))) {
          next
        }
        values <- group_experts[[column]]
        bars <- bind_rows(bars, tibble(
          panel = "", bar = questions$bar[q],
          segment = SHARE_TECHNOLOGY_LABELS[[suffix]],
          share = mean(values, na.rm = TRUE) / 100,
          n_bar = sum(!is.na(values))
        ))
      }
    }
    if (nrow(bars) == 0) {
      next
    }
    bars <- bars %>%
      filter(n_bar >= MIN_N) %>%
      mutate(bar = factor(bar, levels = questions$bar),
             segment = factor(segment, levels = technology_order))
    if (nrow(bars) == 0) {
      next
    }
    plot <- plot_stacked_shares(bars, TECHNOLOGY_COLOURS, "Mean share")
    file_name <- paste0("fig_technology_shares_", group_slug(group), ".pdf")
    save_figure(plot, file.path(dirs$figures, file_name))
    exhibits <- bind_rows(exhibits, exhibit_row(
      block, "technology_shares", group, "figure",
      file.path("figures", file_name),
      "Technologies among the recommendations and installations",
      if_else(group == ALL_EXPERTS, "main", "appendix"),
      note = paste(
        "Mean share among the firm's recommendations in the last year (Q10b, firms",
        "with energy advice, engineering or chimney sweeping) and installations in",
        "2025 (Q11b, HVAC firms). Each firm counts once. n: experts who gave",
        "shares. A bar of fewer than", MIN_N, "experts is left out."
      )
    ))
  }
  exhibits
}
