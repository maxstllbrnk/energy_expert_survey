#' Shared figure style: one theme, fixed sizes, vector PDF.
#' Core file of the economics-r skill: identical in every project. The
#' project's colours (paper_colors) and sizes (figure_sizes) are set in the
#' project settings of code/00_setup.R, which also applies theme and palette to
#' every plot, so analysis scripts only add labs() and save with save_figure().
#' Needs: ggplot2.


# 1. Theme ---------------------------------------------------------------------

#' ggplot theme for papers and slides.
#' No titles inside figures: the LaTeX caption carries the title and the source.
#' @param base_size font size in points at the saved size
theme_paper <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      axis.line = element_line(colour = "grey30", linewidth = 0.3),
      axis.ticks = element_line(colour = "grey30", linewidth = 0.3),
      axis.text = element_text(size = base_size - 1, colour = "grey20"),
      axis.title = element_text(size = base_size),
      legend.position = "bottom",
      legend.title = element_blank(),
      # legend close to the axis, so short figures keep their height for data
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(4, "pt"),
      strip.text = element_text(size = base_size, hjust = 0),
      plot.tag = element_text(size = base_size, face = "bold"),
      plot.margin = margin(4, 8, 4, 4),
      # no background of its own, so slides show their own background colour
      plot.background = element_blank()
    )
}


# 2. Saving --------------------------------------------------------------------

#' Save a ggplot or patchwork figure as a vector PDF at a fixed size.
#' The sizes come from figure_sizes in the project settings of 00_setup.R.
#' @param plot ggplot or patchwork object
#' @param file path of the .pdf file, e.g. file.path(dir_figures, "fig_x.pdf")
#' @param size a name in figure_sizes, e.g. "full", "tall", "half"
save_figure <- function(plot, file, size = "full") {
  if (!exists("figure_sizes")) stop("figure_sizes is not set; see the project settings in 00_setup.R")
  if (!grepl("\\.pdf$", file)) stop("Figure files must end in .pdf: ", file)
  if (!size %in% names(figure_sizes)) stop("Unknown size: ", size)

  dims <- figure_sizes[[size]]
  # cairo_pdf embeds the fonts and handles characters such as the euro sign;
  # a transparent background shows as white on paper and takes on the
  # background colour of slides instead of sitting in a white box
  ggsave(file, plot, width = dims[["width"]], height = dims[["height"]],
         units = "in", device = cairo_pdf, bg = "transparent")
}
