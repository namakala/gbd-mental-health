# Collection of general-use functions

findCol <- function(tbl, pattern, fetch = FALSE, ...) {
  #' Find Column
  #'
  #' Find column names using regex
  #'
  #' @param tbl A data frame containing column of interest
  #' @param pattern Regular expression of the variable name to search
  #' @param fetch Boolean to whether or not fetch the data
  #' @param ... Parameters to pass on to `base::grepl`
  #' @return Vector of the colum names
  cols  <- colnames(tbl)
  query <- cols %>%
    {.[grepl(x = ., pattern = pattern, ignore.case = TRUE, ...)]}
  
  if (fetch) {
    sub_tbl <- subset(tbl, select = query)
    return(sub_tbl)
  } else {
    return(query)
  }
}

subsetGroup <- function(tbl, group, ...) {
  #' Subset Group
  #'
  #' Subset a table into a group of tables
  #'
  #' @param tbl A data frame to subset
  #' @param group A character object of a group name
  #' @param ... Parameters to pass on to `base::subset`
  #' @return A list of subsets from the input table
  group  <- tbl %>% extract2(group)
  groups <- group %>%
    unique() %>%
      set_names( # Easing up name referencing
        stringr::str_to_lower(.) %>%
          gsub(x =., pattern = "\\W", replacement = "_") %>%
          gsub(x =., pattern = "_{2,}", replacement = "_")
      ) %>%
      lapply(function(groupname) {
        tbl %>% subset(group == groupname, ...)
      })
  
  return(groups)
}

cleanGroup <- function(tbl, group, ...) {
  #' Clean Group
  #'
  #' Clean group variable from the GBD table
  #'
  #' @param tbl A data frame of GBD data
  #' @param group A character object of group variable
  #' @param ... Parameters to pass on to `base::gsub`
  #' @return A data frame with cleaned group variable
  groupvar  <- tbl %>% extract2(group)
  tbl_clean <- tbl %>%
    inset2(group, value = {
      gsub(x = groupvar, "upper|\\scountr.*$", "", ignore.case = TRUE) %>%
        {gsub(x = ., "-", " ")} %>%
        {gsub(x = ., "^\\s+|\\s+$", "")} %>% # Remove white-space trails
        stringr::str_to_title()
    })

  return(tbl_clean)
}
