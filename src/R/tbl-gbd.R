# Function to process GBD data

readGBD <- function(fname, entity = NULL, ...) {
  #' Read GBD file and subset based on countries of interest
  #'
  #' @param fname File name of the data to read
  #' @param entity Vector of entity names to include in the subset comment
  #' @param ... Other parameters to pass on to `readr::read_csv`
  #' @return A tibble of data frame
  tbl <- readr::read_csv(fname, comment = "#", ...)

  if (is.null(entity)) {
    entity <- "all"
  } else {
    entity %<>% paste(collapse = "|")
  }

  if (entity != "all") {
    sub_tbl <- tbl %>%
      subset(grepl(x = .$group, pattern = entity, ignore.case = TRUE)) %>%
      inset2("group", value = gsub(x = .$group, "\\s\\(.*$", ""))
    return(sub_tbl)
  }

  return(tbl)
}

transformGBD <- function(tbl, pattern, shape = "wide", ...) {
  #' Transform the GBD data into wide or long format
  #'
  #' @param tbl A data frame of GBD data
  #' @param pattern Regular expression of variables to transform
  #' @param shape Shape of the data frame, accepts either `wide` or `long`
  #' @param ... Parameters to pass on to `tidyr::pivot_wider` or `tidyr::pivot_longer`
  #' @return A wide data frame
  query <- findCol(tbl, pattern)
  tbl_transform <- switch(
    shape,
    wide = tidyr::pivot_wider(tbl, names_from = group, values_from = all_of(query), ...),
    long = tidyr::pivot_longer(tbl, cols = all_of(query), names_to = "variable", ...)
  )

  return(tbl_transform)
}

parseGBD <- function(...) {
  #' Parse GBD data into long and wide format
  #'
  #' @param ... Parameters to pass on to `readGBD`
  #' @return List of data frames both in long and wide format
  tbl    <- readGBD(...)
  groups <- unique(tbl$group) %>%
    set_names( # Easing up name referencing
      stringr::str_to_lower(.) %>%
        gsub(x =., pattern = "\\W", replacement = "_") %>%
        gsub(x =., pattern = "_{2,}", replacement = "_")
    ) %>%
    lapply(function(groupname) {
      tbl %>% subset(.$group == groupname)
    })

  tbl_transform <- list(
    "long" = transformGBD(tbl, "daly", shape = "long"),
    "wide" = groups
  )

  return(tbl_transform)
}
