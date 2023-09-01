# Load targets to build pipeline
pkgs <- c("magrittr", "targets", "renv", "docstring", "tarchetypes")
sapply(pkgs, library, character.only = TRUE)

# Set up package dependencies
tar_option_set(packages = c(
  "magrittr", "readr", "ggplot2"
))

# Source user-defined functions
funs <- list.files("src", pattern = "*R", recursive = TRUE, full.names = TRUE)
sapply(funs, source)

# List all data
gbd_data <- list.files("data", recursive = TRUE, full.names = TRUE, pattern = "csv") %>%
  set_names(gsub(x = ., ".*/(.*).csv", "\\1"))

# Analysis pipeline
list(
  tar_target(files, gbd_data),
  tar_target(tbls, lapply(files, readGBD, entity = c("Income"))),
  tar_target(
    merge_tbl,
    merge(
      tbls[["psych-daly"]],
      tbls[["psych-prev"]],
      by = c("group", "ctry_code", "year"))
  ),
  tar_target(gbd_describe, groupSummary(merge_tbl, "group")),
  tar_target(gbd_report, tblSummary(merge_tbl)),
  tar_target(gbd_cor, groupCor(merge_tbl, "group") %>% lapply(corSummary)),
  tar_target(viz_daly, swarmVizGBD(tbls[["psych-daly"]])),
  tar_target(viz_prev, lineVizGBD(tbls[["psych-prev"]])),
  tar_quarto(doc_descriptive, "docs/descriptive-stat.qmd")
)
