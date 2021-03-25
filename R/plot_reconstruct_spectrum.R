#' @title Plot the reconstructed EISA fragmentation spectrum
#'
#' @description Plot the reconstructed EISA fragmentation spectrum.
#'
#' @param peak_list data frame with the peak list
#' @param peak_group integer, for which peak group you want to plot the reconstructed
#'     EISA fragmentation spectrum
#' @param title tilte of the plot
#' @param subtitle subtitle of the plot
#' @param xmin start of the *m/z* axis
#' @param xmax end of the *m/z* axis
#' @param xstep step size of the *m/z* axis
#'
#' @return A ggplot object containing the reconstructed EISA fragmentation spectrum.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot geom_linerange aes scale_x_continuous labs theme_minimal
#'
#' @author Rico Derks
#'

plot_reconstruct_spectrum <- function(peak_list, peak_group = NULL, title = NULL,
                                      subtitle = NULL, xmin = 20, xmax = 260, xstep = 20) {
  # some error checking
  if (!is(peak_list, "data.frame")) {
    stop("'peak_list' does not appear to be a data frame!")
  }

  my_cols <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim",
               "peak_group", "num_peaks", "min_rt", "max_rt", "intens_perc",
               "intens_ratio")
  cols <- colnames(peak_list)

  if(!all(my_cols %in% cols)) {
    stop("'peak_list' doesn't contain all the correct column names!")
  }

  if(is.null(peak_group)) {
    stop("No peak group selected!")
  } else {
    if(length(peak_group) != 1 | peak_group[1] <= 0 | !is.numeric(peak_group) ) {
      stop("'peak_group' should be a positive number of length 1!")
    }
  }

  if(length(xmin) != 1 | length(xmax) != 1 | length(xstep) != 1 | xmin[1] < 0 | xmax[1] <= 0 | xstep[1] <= 0 | !is.numeric(xmin) | !is.numeric(xmax) | !is.numeric(xstep)) {
    stop("'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")
  }

  if(xmin >= xmax) {
    stop("'xmin' should be smaller then 'xmax'!")
  }

  p <- peak_list %>%
    filter(.data$peak_group == peak_group) %>%
    mutate(sim_mz = as.numeric(as.character(.data$sim))) %>%
    ggplot() +
    geom_linerange(aes(x = .data$sim_mz,
                       ymin = 0,
                       ymax = .data$intens_perc)) +
    scale_x_continuous(breaks = seq(xmin, xmax, xstep),
                       limits = c(xmin, xmax)) +
    labs(x = expression(italic("m/z")),
         y = "Relative intensity [%]",
         subtitle = subtitle,
         title = title) +
    theme_minimal()

  return(p)
}
