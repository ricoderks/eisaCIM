#' @title Read the files
#'
#' @description Read the mzML files contraining SIM data.
#'
#' @param files Character vector with the filename to read.
#'
#' @details This functions uses `readSRMData` from the MSnbase package.
#'     `readSRMData` supports reading chromatogram entries from mzML files. If multiple
#'     files are provided the same precursor and product m/z for SRM/MRM chromatograms
#'     are expected across files. The number of columns of the resulting MChromatograms()
#'     object corresponds to the number of files. Each row in the MChromatograms object
#'      is supposed to contain chromatograms with same polarity, precursor and product
#'      *m/z*. If chromatograms with redundant polarity, precursor and product *m/z*
#'      values and precursor collision energies are found, they are placed
#'      into multiple consecutive rows in the MChromatograms object.
#'
#' @return A MChromatograms() object. See details above for more information.
#'
#' @author Rico Derks
#'
#' @export
#'
#' @importFrom MSnbase readSRMData
#'
read_files <- function(files) {
  my_data <- readSRMData(files)

  return(my_data)
}
