#' @title Show all the SIM traces.
#'
#' @description Get all the *m/z* values of all SIM traces from a MChromatograms object.
#'
#' @param sim_data MChromatograms object from readSRMData().
#'
#' @return  A 1 column matrix with the *m/z* values of all SIM traces.
#'
#' @author Rico Derks
#'
#' @export
#' @importFrom MSnbase precursorMz
#'
get_all_sim <- function(sim_data) {
  # some error checking
  if(class(sim_data) != "MChromatograms") {
    stop("'sim_data' is not a MChromatograms object!")
  }

  res <- as.matrix(MSnbase::precursorMz(sim_data)[, "mzmin"])
  colnames(res) <- "sim"

  return(res)
}
