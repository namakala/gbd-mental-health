# Functions to perform EDA on GBD data

groupSummary <- function(tbl, group, ...) {
  #' Group Summary
  #'
  #' Calculate summary statistics per group
  #'
  #' @param tbl A GBD data frame
  #' @param group A character object of group variable
  #' @param ... Parameters to pass on to `stats::aggregate`
  #' @return List of data frames containing the summary statistics
  mtx  <- findCol(tbl, "daly|prev", fetch = TRUE) %>% as.matrix()
  form <- sprintf("mtx ~ %s", group) %>% as.formula()
  funs <- list("mean", "median", "sd", "IQR", "min", "max") %>%
    set_names(., .) %>%
    lapply(get)

  res <- lapply(funs, function(fun) {
    aggregate(form, data = tbl, FUN = fun, ...)
  })

  return(res)
}

groupCor <- function(tbl, group, ...) {
  #' Group Correlation
  #'
  #' Calculate pairwise correlation for all group members
  #'
  #' @param tbl A GBD data frame
  #' @param group A character object of group variable
  #' @param ... Parameters to pass on to `stats::cor`
  #' @return List of correlation matrices
  groups <- tbl %>% subsetGroup(group, select = findCol(., "daly|prev"))
  res    <- lapply(groups, cor, use = "complete.obs", ...)

  return(res)
}

tblSummary <- function(tbl, type = "kable", ...) {
  #' Table Summary
  #'
  #' Create a knitr-ready descriptive statistics of GBD data
  #'
  #' @param tbl A GBD data frame
  #' @param type Type of table to return, accepts either `hux` or `kable`
  #' @param ... Parameters to pass on to `gtsummary::tbl_summary`
  #' @return A hux or kable object containing the statistical summary
  res <- tbl %>%
    gtsummary::select(-c(ctry_code, year)) %>%
    gtsummary::tbl_summary(
      by = group,
      label = list(
        daly_depress ~ "Depression",
        daly_anxiety ~ "Anxiety",
        daly_schizo ~ "Schizophrenia",
        daly_bipolar ~ "Bipolar",
        daly_eat_disorder ~ "Eating Disorder",
        prev_depress ~ "Depression",
        prev_anxiety ~ "Anxiety",
        prev_schizo ~ "Schizophrenia",
        prev_bipolar ~ "Bipolar",
        prev_eat_disorder ~ "Eating Disorder"
      ),
      statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
      ...
    )

    if (type == "kable") {
      res %<>%
        gtsummary::modify_header(
          update = list(gtsummary::all_stat_cols() ~ "**{level}**\nN = {n}")
        ) %>%
        gtsummary::as_kable_extra() %>%
        kableExtra::pack_rows("Disability-Adjusted Life Year", 1, 5) %>%
        kableExtra::pack_rows("Prevalence (%)", 6, 10)
    } else if (type == "hux") {
      res %<>%
        gtsummary::as_hux_table() %>%
        huxtable::add_rows(
          c("Disability-Adjusted Life Year", NA, NA, NA, NA), after = 1
        ) %>%
        huxtable::add_rows(
          c("Prevalence (%)", NA, NA, NA, NA), after = 7
        ) %>%
        huxtable::merge_cells(2, 1:5) %>%
        huxtable::merge_cells(8, 1:5) %>%
        huxtable::set_bottom_border(c(2, 8), 1:5, 0.4) %>%
        huxtable::set_top_border(c(2, 8), 1:5, 0.4)
    } else {
      return(res)
    }

  return(res)
}

corSummary <- function(gbd_cor) {
  #' Correlation Summary
  #'
  #' Create a knitr-ready summary of pairwise correlation
  #'
  #' @param gbd_cor A pre-calculated correlation matrix of a GBD data frame
  #' @return A pairwise correlation
  labels <- labelGBD()

  res <- modelsummary::datasummary_correlation_format(
    gbd_cor, fmt = 2, upper_triangle = "."
  ) %>%
    set_colnames(1:ncol(.)) %>%
    set_rownames(paste(labels[rownames(.)], sprintf("(%s)", 1:nrow(.))))

  return(res)
}

labelGBD <- function() {
  #' Label GBD
  #'
  #' Create a label for GBD data
  #'
  #' @return A named vector containing GBD's label and its description
  labels <- expand.grid(
    c("DALY", "Prevalence"),
    c(
      "Depressive Disorder", "Schizophrenia", "Bipolar Disorder",
      "Eating Disorder", "Anxiety"
    )
  ) %>%
    inset(c("prefix", "name"), value = list(
      "prefix" = dplyr::case_when(
        .$Var1 == "DALY" ~ "daly",
        .$Var1 == "Prevalence" ~ "prev"
      ),
      "name" = dplyr::case_when(
        grepl(x = .$Var2, "Depress") ~ "depress",
        grepl(x = .$Var2, "Schizo")  ~ "schizo",
        grepl(x = .$Var2, "Bipolar") ~ "bipolar",
        grepl(x = .$Var2, "Eating")  ~ "eat_disorder",
        grepl(x = .$Var2, "Anxiety") ~ "anxiety"
      )
  )) %>%
    inset(c("label", "labelname"), value = list(
      paste(.$Var1, .$Var2), paste(.$prefix, .$name, sep = "_")
    )) %>%
    {set_names(.$label, .$labelname)}

  return(labels)
}
