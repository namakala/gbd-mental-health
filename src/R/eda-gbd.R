# Functions to perform EDA on GBD data

groupSummary <- function(tbl, group, ...) {
  #' Calculate summary statistics per group
  #'
  #' @param tbl A GBD data frame
  #' @param group A character object of group variable
  #' @param ... Parameters to pass on to `base::aggregate`
  #' @return List of data frames containing the summary statistics
  res <- gtsummary::tbl_summary(tbl)
}


sanity <- function() {
  tbl <- tar_read(tbl_gbd)
  tbl %>% aggregate(findCol(tbl, "daly", fetch = TRUE) ~ group, mean)
}

sanity()


cbind(findCol(tar_read(tbl_gbd), "daly", fetch = TRUE))
