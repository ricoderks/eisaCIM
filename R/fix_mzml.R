#' @title Fix mzML file
#'
#' @description Fix a mzML file with SIM data for reading.
#'
#' @param mzml_file character vector containing the filename of the mzML.
#' @param new_mzml_file character vector containing the filename of the new to
#'     create mzML file.
#'
#' @details mzML files made with more recent versions of ProteoWizard can generate
#'     the following error: `Error: Can not open file <filename>! Original error
#'     was: Error in pwizModule$open(filename): [IO::HandlerBinaryDataArray]
#'     Unknown binary data type.`
#'
#'     This is error is caused by the usage of an older ProteoWizard library by the
#'     package mzR (for more information see https://github.com/lgatto/MSnbase/issues/517).
#'
#'     This function uses the `xml2` package to remove the part which is giving
#'     problems.
#'
#' @author Rico Derks
#'
#' @export
#'
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_find_all xml_set_attr
#'     xml_remove write_xml
#' @importFrom magrittr %>%
#'

fix_mzml <- function(mzml_file, new_mzml_file) {
  if (length(mzml_file) > 1) {
    warning("Length of 'mzml_file' is > 1, only first one is used!")
  }

  if (length(new_mzml_file) > 1) {
    warning("Length of 'new_mzml_file' is > 1, only first one is used!")
  }

  mzml_data <- read_xml(x = mzml_file[1])

  # find the tic node
  TIC <- mzml_data %>%
    xml_ns_strip() %>%
    xml_find_all(xpath = ".//chromatogram[@id='TIC']")

  # change the count to 2
  TIC %>%
    xml_find_all(xpath = ".//binaryDataArrayList") %>%
    xml_set_attr(attr = "count",
                 value = "2")

  # find the node to remove
  remove_me <- TIC %>%
    xml_find_all(xpath = ".//binaryDataArrayList/binaryDataArray")

  # actually remove it, it is 3rd one
  xml_remove(remove_me[[3]])

  # write to new file
  write_xml(x = mzml_data,
            file = new_mzml_file[1])
}
