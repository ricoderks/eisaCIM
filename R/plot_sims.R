#' @title Show the SIM traces
#'
#' @description Plot all SIM trace, including optional line to show where the compound elutes.
#'
#' @param sim_data Data frame containing all the SIMs. Needs three columns: rt = retention time (x), intensity = intensity (y),
#'    sim = factor with the SIM names (used for faceting)
#' @param rt_line A numerical value. If not NULL a dashed line is shown at this position.
#' @param title Title of the plot (character).
#' @param subtitle Subtitle of the plot (character).
#' @param peak_data data frame containing all peak data. Output from find_peaks().
#'
#' @return A ggplot2 showing all the SIM traces.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous labs facet_wrap theme_minimal theme geom_vline geom_rect
#' @importFrom scales scientific
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
plot_sims <- function(sim_data, rt_line = NULL, title = NULL, subtitle = NULL, peak_data = NULL) {
  # some error checking
  if(!is(sim_data, "data.frame")) {
    stop("'sim_data' is not a data frame!")
  }

  col_sim <- colnames(sim_data)
  my_col_sim <- c("rt", "intensity", "sim")
  if(!all(col_sim %in% my_col_sim)) {
    stop("'sim_data' doesn't contain all the correct columns!")
  }

  if(!is.null(peak_data)) {
    if(!is(peak_data, "data.frame")) {
      stop("'peak_data' is not a data frame!")
    }

    col_peak <- colnames(peak_data)
    my_col_peak <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim")
    if(!all(my_col_peak %in% col_peak)) {
      stop("'peak_data' doesn't contain all the correct columns!")
    }
  }

  if(!is.null(rt_line)) {
    if(!is.numeric(rt_line) | rt_line < 0) {
      stop("'rt_line' should be a positive number!")
    }
  }

  p <- sim_data %>%
    ggplot() +
    geom_line(aes(x = .data$rt,
                  y = .data$intensity,
                  group = .data$sim)) +
    scale_y_continuous(labels = scales::scientific) +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    labs(x = "Retention time [minutes]",
         y = "Intensity",
         title = title,
         subtitle = subtitle) +
    facet_wrap(~ .data$sim,
               ncol = 1,
               scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "none")

  if (!is.null(rt_line)) {
    p <- p +
      geom_vline(aes(xintercept = rt_line),
                 linetype = 2,
                 colour = "gray")
  }

  if (!is.null(peak_data)) {
    p <- p +
      geom_rect(data = peak_data,
                aes(xmin = .data$rtmin,
                    xmax = .data$rtmax,
                    ymin = 0,
                    ymax = .data$maxo),
                fill = NA,
                colour = "red")
  }

  return(p)
}
