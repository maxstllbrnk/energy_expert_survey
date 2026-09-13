# =============================================================================
# 06_maps.R  -  where the experts' firms are
# =============================================================================
# Two maps from the company postcode (Q0), both drawn on the official state
# boundaries of the BKG (map_states, loaded once by the runner):
#
#   experten_nach_bundesland   number of experts per federal state
#   experten_nach_plz          one dot per postcode, dot area = number of experts
#
# The federal state is the `bundesland` the cleaning pipeline derives from the
# postcode; the dot positions come from the GeoNames postcode file
# (map_postcodes). Figures in 03_karten/.
#
# Uses sm (one row per expert) of the current subset.
# =============================================================================

# --- per federal state ---------------------------------------------------------
by_state <- sm %>%
  filter(!is.na(bundesland)) %>%
  count(bundesland) %>%
  mutate(bundesland = as.character(bundesland))

if (sum(by_state$n) >= MIN_N) {
  p <- map_states %>%
    left_join(by_state, by = c("GEN" = "bundesland")) %>%
    mutate(n = coalesce(n, 0L)) %>%
    ggplot() +
    geom_sf(aes(fill = n), color = "white", linewidth = 0.3) +
    geom_sf_label(aes(label = fmt_n(n)), size = LABEL_SIZE, color = INK_PRIMARY,
                  fill = "white", alpha = 0.85, linewidth = 0,
                  label.padding = unit(0.12, "lines")) +
    scale_fill_gradient(low = MAP_LOW, high = MAP_HIGH, labels = number_de, name = "Experten") +
    coord_sf(datum = NA) +
    labs(title = "Wo haben die befragten Experten ihren Unternehmenssitz?",
         subtitle = "Anzahl der Experten je Bundesland, abgeleitet aus der Postleitzahl (Q0)",
         x = NULL, y = NULL, caption = make_caption(sum(by_state$n))) +
    theme(legend.position = "right",
          legend.title = element_text(color = INK_SECONDARY))
  save_figure(p, "03_karten", "experten_nach_bundesland", height = 19)
}

# --- per postcode ----------------------------------------------------------------
by_postcode <- sm %>%
  filter(!is.na(plz)) %>%
  count(plz) %>%
  inner_join(map_postcodes, by = "plz")

n_not_placed <- sum(!is.na(sm$plz)) - sum(by_postcode$n)

if (sum(by_postcode$n) >= MIN_N) {
  points <- by_postcode %>%
    arrange(desc(n)) %>%                       # small dots drawn on top of large ones
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(st_crs(map_states))

  p <- ggplot() +
    geom_sf(data = map_states, fill = MAP_LAND, color = "white", linewidth = 0.5) +
    geom_sf(data = points, aes(size = n), shape = 16, color = COL_BAR, alpha = 0.45) +
    scale_size_area(max_size = 6, name = "Experten je PLZ",
                    breaks = function(limits) unique(round(seq(1, limits[2], length.out = 4)))) +
    coord_sf(datum = NA) +
    labs(title = "Wo haben die befragten Experten ihren Unternehmenssitz?",
         subtitle = "Ein Punkt je Postleitzahl; die Fläche des Punktes entspricht der Anzahl der Experten",
         x = NULL, y = NULL,
         caption = make_caption(sum(by_postcode$n),
                                if (n_not_placed > 0) str_c(n_not_placed, " Experten mit einer Postleitzahl ",
                                                            "ohne Koordinaten nicht dargestellt"))) +
    theme(legend.position = "right",
          legend.title = element_text(color = INK_SECONDARY))
  save_figure(p, "03_karten", "experten_nach_plz", height = 19)
}
