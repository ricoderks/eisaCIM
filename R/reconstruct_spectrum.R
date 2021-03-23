#' @title Calculate the ratio's for the reconstructed EISA fragmentation spectrum
#'
#' @description The ratio's of the precursor and the 3 fragments are calculate so a
#'     reconstructed EISA fragmentation spectrum can be created.
#'
#' @param peak_list data frame with the peak list
#'
#' @return The same data frame, but with additional columns `intens_perc` and `intens_ratio`.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
reconstruct_spectrum <- function(peak_list) {
  # some error checking
  if (!is(peak_list, "data.frame")) {
    stop("'peak_list' does not appear to be a data frame!")
  }

  my_cols <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim",
               "peak_group", "num_peaks", "min_rt", "max_rt")
  cols <- colnames(peak_list)

  if(!all(my_cols %in% cols)) {
    stop("'peak_list' doesn't contain all the correct column names!")
  }

  peak_list <- peak_list %>%
  group_by(.data$peak_group) %>%
  mutate(intens_perc = .data$maxo / sum(.data$maxo) * 100,
         intens_ratio = .data$maxo / sum(.data$maxo))

  return(peak_list)
}
