################################################################################
# setup.R — packages, paths, figure style and shared functions. Sourced at the
#           top of every script; no other script loads packages or sets options.
# Inputs : config.R, R/functions/, R/summary_statistics/
# Outputs: none
# Checks : scripts run from the project root
################################################################################

if (!file.exists("config.R")) {
  stop("Run this from the project root (the folder containing config.R). ",
       "In RStudio, open energy_expert_survey.Rproj first.")
}

# 1. Packages ------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(mlogit)
library(sf)
library(scales)
library(patchwork)
library(kableExtra)
library(modelsummary)

# 2. Paths and constants -------------------------------------------------------
source("config.R")

# 3. Figure settings -----------------------------------------------------------
# The original palette of the project (commit ced89b0, 13 September 2026), in the
# order that keeps neighbouring segments distinguishable for colour-blind readers
paper_colors <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
                  "#898781")

# In inches, for 16:9 slides and used unchanged in the paper: figures are
# included at their natural size, so their text is the same size everywhere
figure_sizes <- list(
  full = c(width = 5.5, height = 2.3),
  half = c(width = 2.7, height = 2.3),
  tall = c(width = 5.5, height = 4.6),
  map = c(width = 3.3, height = 3.2)
)
figure_font_size <- 9

# 4. Options -------------------------------------------------------------------
options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  ggplot2.discrete.colour = paper_colors,
  ggplot2.discrete.fill = paper_colors
)

# 5. Shared functions ----------------------------------------------------------
function_files <- c(
  list.files(file.path("R", "functions"), pattern = "\\.R$", full.names = TRUE),
  list.files(file.path("R", "summary_statistics"), pattern = "\\.R$", full.names = TRUE)
)
for (function_file in function_files) {
  source(function_file, encoding = "UTF-8")
}

theme_set(theme_paper(base_size = figure_font_size))
