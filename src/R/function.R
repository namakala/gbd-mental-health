# Collection of general-use functions

findCol <- function(tbl, pattern, fetch = FALSE, ...) {
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

