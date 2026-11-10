#' Checks written into the code: sample construction and unique keys.
#' Core file of the economics-r skill: identical in every project.
#' Needs: dplyr.


#' One row of a sample-construction log.
#' Bind the rows of successive steps and write them as a table with table_custom().
#' @param data data frame after the step
#' @param step description of the step, e.g. "Drop households ever in social housing"
#' @param id name of the unit identifier, e.g. "hid"
#' @return one-row tibble: step, observations, units
count_sample <- function(data, step, id) {
  tibble(step = step, observations = nrow(data), units = n_distinct(data[[id]]))
}

#' Stop if the key does not identify the rows uniquely.
#' @param data data frame
#' @param key names of the key columns, e.g. c("hid", "year")
check_unique <- function(data, key) {
  n_duplicates <- sum(duplicated(data[, key]))
  if (n_duplicates > 0) {
    stop(n_duplicates, " rows repeat the key ", paste(key, collapse = " x "))
  }
  invisible(TRUE)
}
