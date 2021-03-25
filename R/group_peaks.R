#' @title Group peaks
#'
#' @description  Group the peaks over all the traces
#'
#' @param peak_list a data frame with all the peaks found in each trace in long format.
#' @param rt_diff the retention time window to group around in seconds. Default is 5 seconds
#'
#' @return A data frame the same as the input, but with some extra columns added containing the grouping information.
#'
#' @author Rico Derks
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange mutate group_by ungroup select n lag
#' @importFrom rlang .data
#'
group_peaks <- function(peak_list, rt_diff = 5) {
  # some error checking
  if (!is(peak_list, "data.frame")) {
    stop("'peak_list' does not appear to be a data frame!")
  }

  my_cols <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim")
  cols <- colnames(peak_list)

  if(!all(my_cols %in% cols)) {
    stop("'peak_list' doesn't contain all the correct column names!")
  }

  if(length(rt_diff) != 1 | !is.numeric(rt_diff) | rt_diff[1] < 0) {
    stop("'rt_diff' should be a positive number and of length 1!")
  }

  peak_list <- peak_list %>%
    arrange(.data$rt) %>%
    mutate(diff_rt = .data$rt - lag(.data$rt, default = .data$rt[1]),
           peak_group = cumsum(.data$diff_rt > rt_diff / 60) + 1L) %>%
    group_by(.data$peak_group) %>%
    mutate(num_peaks = n(),
           min_rt = mean(.data$rtmin),
           max_rt = mean(.data$rtmax)) %>%
    ungroup() %>%
    select(-.data$diff_rt)

  return(peak_list)
}
