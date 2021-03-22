#' @title Extract the SIM traces
#'
#' @description  Extract the SIM traces from all the data and put them in a nice data frame.
#'
#' @param sim_data MChromatograms object from readSRMData() containing all the sims from your experiment
#' @param sim_names A character vector containing the names you want to give to each trace.
#' @param sim_ids a integer vector containing the index of each sim traces you want.
#'
#' @return A data frame containing the retention time and intensity of each sim trace in long format.
#'
#' @export
#' @importFrom xcms rtime intensity
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
extract_sim_data <- function(sim_data, sim_names, sim_ids) {
  if(length(sim_names) != length(sim_ids)) {
    stop("'sim_names' and 'sim_ids' need to have the same length!!")
  }
  eisa <- lapply(1:length(sim_names), function(x) {
    tmp <- data.frame(rt = rtime(sim_data[sim_ids[x], 1]),
                      intensity = intensity(sim_data[sim_ids[x], 1]),
                      sim = sim_names[x])
  })

  eisa <- do.call(rbind, eisa)

  eisa <- eisa %>%
    mutate(sim = factor(x = .data$sim,
                        levels = sim_names,
                        labels = sim_names))

  return(eisa)
}
