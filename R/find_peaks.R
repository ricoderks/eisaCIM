#' @title Find all peaks
#'
#' @description Perfrom peak picking (CentWave) on all traces.
#'
#' @param data MChromatograms object from readSRMData() containing all the sims from your experiment
#' @param sim_ids a integer vector containing the index of each sim traces you want.
#' @param sim_names A character vector containing the names you want to give to each trace.
#' @param noise a numeric vector containg the noise levels for each trace.
#' @param peakwidth numeric(2) with the expected approximate peak width in chromatographic space. Given as a range (min, max) in minutes.
#' @param snthresh numeric(1) defining the signal to noise ratio cutoff.
#'
#' @return A data frame with all the peaks found in each trace in long format.
#'
#' @author Rico Derks
#'
#' @export
#'
#' @importFrom xcms findChromPeaks CentWaveParam chromPeaks
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
find_peaks <- function(data, sim_ids, sim_names, noise, peakwidth = c(0.15, 0.5), snthresh = 10){
  # some error checking
  if(!is(data, "MChromatograms")) {
    stop("'data' is not a MChromatograms object!")
  }

  if(length(sim_ids) != length(sim_names) |
     length(sim_ids) != length(noise)) {
    stop("'sim_ids', 'sim_names' and 'noise' all need to have the same length!!")
  }

  if(!is.numeric(sim_ids)) {
    stop("'sim_ids' needs to be integer numbers!")
  }

  if(!is.numeric(noise)) {
    stop("'noise' needs to be numerical!")
  }

  if(!is.numeric(peakwidth) | length(peakwidth) != 2) {
    stop("'peakwidth' needs to be numerical and of length 2!")
  }

  if(!is.numeric(snthresh) | length(snthresh) != 1 | snthresh[1] < 0) {
    stop("'snthresh' needs to be positive numerical and of length 1!")
  }


  my_peaks <- lapply(1:length(sim_ids), function(x) {
    tmp <- findChromPeaks(object = data[sim_ids[x]],
                          param = CentWaveParam(peakwidth = peakwidth,
                                                snthresh = snthresh,
                                                prefilter = c(3, noise[x]),
                                                noise = noise[x]))
    cbind(chromPeaks(tmp), sim = x)
  })

  my_peaks <- as.data.frame(do.call(rbind, my_peaks)) %>%
    mutate(sim = factor(.data$sim,
                        labels = sim_names))

  return(my_peaks)
}
