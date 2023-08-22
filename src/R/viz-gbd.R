# Functions to visualize GBD data

cleanViz <- function(tbl, transform = TRUE) {
  #' Clean Visual
  #'
  #' Clean a data frame for visualization
  #'
  #' @param tbl A table of GBD data frame
  #' @return A clean data frame in a long format
  tbl_clean <- tbl %>%
    inset2("group", value = {
      factor(.$group, levels = c(
        "Low Income", "Lower Middle Income", "Middle Income", "High Income"
      ))
    })

  if (transform) {
    labels <- labelGBD() # Label GBD variables
    tbl_clean %<>% transformGBD("daly|prev", shape = "long") %>%
      inset("vars", value = labels[.$vars])
  }

  return(tbl_clean)
}

swarmVizGBD <- function(tbl, ...) {
  #' Swam Plot GBD
  #'
  #' Create a beeswarm plot using GGPlot2 using GBD dataset
  #'
  #' @param tbl A table of GBD data frame
  #' @param ... Parameters to pass on to `ggplot2::ggplot`
  #' @return A beeswarm plot
  pkgs <- c("ggplot2", "ggpubr")
  pkgs_load <- sapply(pkgs, require, character.only = TRUE)

  tbl %<>% cleanViz()

  plt <- ggboxplot(
    tbl, x = "group", y = "value", add = "mean_sd", alpha = 0.6,
    facet.by = "vars", scale = "free", nrow = 1
  )

  return(plt)
}

lineVizGBD <- function(tbl, ...) {
  #' Line Plot GBD
  #'
  #' Create a line plot using a GBD data frame
  #' 
  #' @param tbl A table of GBD data frame
  #' either `prev` for prevalence and `daly` for DALY measure as an argument
  #' @param ... Parameters to pass on to `ggplot2::ggplot`
  #' @return A line plot
  pkgs <- c("ggplot2")
  pkgs_load <- sapply(pkgs, require, character.only = TRUE)

  tbl %<>% cleanViz()

  plt <- ggplot(tbl, aes(x = year, y = value, color = vars)) +
    geom_point() +
    facet_wrap(~ group, scales = "free")

  return(plt)
}
