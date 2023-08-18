# Load targets to build pipeline
pkgs <- c("magrittr", "targets")
sapply(pkgs, library, character.only = TRUE)

# Set up package dependencies
tar_option_set(packages = c(
  "magrittr", "readr", "ggplot2"
))

# Source user-defined functions
funs <- list.files("src", pattern = "*R", recursive = TRUE, full.names = TRUE)
sapply(funs, source)

# List all data
tbls <- list.files("data", recursive = TRUE, full.names = TRUE) %>%
  set_names(gsub(x = ., ".*/(.*).csv", "\\1"))

# Analysis pipeline
list(
  tar_target(gbd_file, tbls["gbd-mental-illness"], format = "file"),
  tar_target(tbl_gbd, readGBD(gbd_file, entity = c("Indonesia", "Income")))
)
