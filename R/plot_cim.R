#' @ titla Plot CIM chromatogram
#'
#' @description Create a correlation ion monitoring chromatogram.
#'
#' @param eisa_data Data frame containing all the SIM traces (from extract_sims()).
#' @param peak_data Data frame containing peak information (from group_peaks), cleaned up.
#' @param select_sim A character vector, which SIM trace to show.
#' @param rt_line A numerical value. If not NULL a dashed line is shown at this position.
#' @param title A character vector. The title of the plot.
#' @param subtitle A character vector. The subtitle of the plot.
#'
#' @return A ggplot graph with the full CIM chromatogram and a zoom part
#'
#' @author Rico Derks
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr distinct mutate filter
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line scale_x_continuous scale_y_continuous labs theme_minimal geom_vline
#' @importFrom scales scientific
#' @importFrom ggforce facet_zoom
#'
#'
plot_cim <- function(eisa_data, peak_data, select_sim = NULL, rt_line = NULL, title = NULL, subtitle = NULL) {
  if(is.null(select_sim)) {
    stop("Define a sim trace to use!")
  }

  # get the retention time ranges I want to keep
  keep_ranges <- peak_data %>%
    distinct(.data$min_rt, .data$max_rt)

  # initialize the keep column
  new_chrom <- eisa_data %>%
    mutate(keep = FALSE)

  # determine what to keep
  for (a in 1:nrow(keep_ranges)) {
    new_chrom$keep[new_chrom$rt >= keep_ranges$min_rt[a] &
                     new_chrom$rt <= keep_ranges$max_rt[a] &
                     new_chrom$keep == FALSE] <- TRUE
  }

  new_chrom <- new_chrom %>%
    mutate(eisa = ifelse(.data$keep == TRUE,
                         .data$intensity,
                         0))

  p <- new_chrom %>%
    filter(.data$sim == select_sim) %>%
    ggplot() +
    geom_line(aes(x = .data$rt,
                  y = .data$eisa,
                  group = 1)) +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    scale_y_continuous(labels = scales::scientific) +
    labs(x = "Retention time [min]",
         title = title,
         subtitle = subtitle) +
    theme_minimal()

  # show the retention time line
  if(!is.null(rt_line)) {
    p <- p +
      geom_vline(aes(xintercept = rt_line),
                 linetype = 2,
                 colour = "gray") +
      facet_zoom(xy = .data$rt > rt_line - 1.5 & .data$rt < rt_line + 1.5,
                 horizontal = FALSE,
                 zoom.size = 1)
  }

  return(p)
}
