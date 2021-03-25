wrong_peak_list <- peak_list
colnames(wrong_peak_list)[1] <- "wrong"

test_that("input check", {
  expect_error(reconstruct_spectrum(peak_list = raw_data),
               "'peak_list' does not appear to be a data frame!")

  expect_error(reconstruct_spectrum(peak_list = wrong_peak_list),
               "'peak_list' doesn't contain all the correct column names!")
})

test_that("output check", {
  my_peak_list <- reconstruct_spectrum(peak_list = peak_list)
  my_class <- class(my_peak_list)
  my_cols <- colnames(my_peak_list)
  expect_cols <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim",
                   "peak_group", "num_peaks", "min_rt", "max_rt", "intens_perc",
                   "intens_ratio")

  expect_true(any(my_class == "data.frame"))

  expect_true(all(expect_cols %in% my_cols))
})
