#' @title Group peaks
#'
#' @description  Group the peaks over all the traces
#'
#' @param peak_data a data frame with all the peaks found in each trace in long format.
#' @param rt_diff the retention time window to group around in seconds. Default is 5 seconds
#'
#' @return A data frame the same as the input, but with some extra columns added containing the grouping information.
#'
#' @author Rico Derks
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange mutate group_by ungroup select n
#' @importFrom rlang .data
#' @importFrom stats lag
#'
group_peaks <- function(peak_data, rt_diff = 5) {
  peak_data <- peak_data %>%
    arrange(.data$rt) %>%
    mutate(diff_rt = .data$rt - lag(.data$rt, default = .data$rt[1]),
           peak_group = cumsum(.data$diff_rt > rt_diff / 60) + 1L) %>%
    group_by(.data$peak_group) %>%
    mutate(num_peaks = n(),
           min_rt = mean(.data$rtmin),
           max_rt = mean(.data$rtmax)) %>%
    ungroup() %>%
    select(-.data$diff_rt)

  return(peak_data)
}
