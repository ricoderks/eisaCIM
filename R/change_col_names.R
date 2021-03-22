#' @title Create custom column names for pivotting.
#'
#' @description  Create nice column names when pivoting MRM data.
#'
#' @param x not used, but need by pivot_wider()
#'
#' @return A character vector with custom column names.
#'
#' @author Rico derks
#'
change_col_names <- function(x) {
  return(c("rt", paste0("trans_", 1:3)))
}
